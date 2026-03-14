(ns airj.jvm-type-info
  (:require [airj.jvm-closures :as closures]))

(def ^:private primitive-types
  {'Int :int
   'Float :float
   'Double :double
   'Bool :boolean
   'Unit :void})

(def ^:private named-reference-types
  {'String "java/lang/String"
   'StringSeq "[Ljava/lang/String;"})

(defn declared-type-root
  [type-expr]
  (if (seq? type-expr)
    (first type-expr)
    type-expr))

(defn- type-bindings
  [decl type-expr]
  (zipmap (:type-params decl)
          (if (seq? type-expr)
            (rest type-expr)
            [])))

(defn instantiate-type
  [type-expr bindings]
  (cond
    (symbol? type-expr) (get bindings type-expr type-expr)
    (seq? type-expr) (apply list (map #(instantiate-type % bindings) type-expr))
    :else type-expr))

(defn fail!
  [message data]
  (throw (ex-info message data)))

(defn internal-name
  [module-name]
  (str module-name))

(defn nested-class-name
  [module-name decl-name]
  (str (internal-name module-name) "$" decl-name))

(defn union-variant-class-name
  [module-name union-name variant-name]
  (str (nested-class-name module-name union-name) "$" variant-name))

(defn java-type?
  [type-expr]
  (and (seq? type-expr)
       (= 'Java (first type-expr))))

(defn java-internal-name
  [type-expr]
  (-> type-expr second str (.replace "." "/")))

(defn- declared-type-op?
  [decl]
  (contains? #{:data :enum :union} (:op decl)))

(defn- declared-class-name
  [module-name decl]
  (when (declared-type-op? decl)
    (nested-class-name module-name (:name decl))))

(defn- local-declared-type-name
  [type-expr ctx]
  (some->> (get (:decls ctx) (declared-type-root type-expr))
           (declared-class-name (:module-name ctx))))

(defn- imported-declared-type-name
  [type-expr ctx]
  (some (fn [[_ {:keys [module decl]}]]
          (when (= (declared-type-root type-expr) (:name decl))
            (declared-class-name module decl)))
        (:imported-decls ctx)))

(defn declared-type-name
  [type-expr ctx]
  (or (local-declared-type-name type-expr ctx)
      (imported-declared-type-name type-expr ctx)))

(defn runtime-union-variant-class-name
  [ctx union-name variant-name]
  (str (declared-type-name union-name ctx) "$" variant-name))

(declare lower-type)
(declare fn-decl)
(declare local-lambda)
(declare local-fn-type)

(defn- seq-type?
  [type-expr]
  (and (seq? type-expr)
       (= 'Seq (first type-expr))))

(defn- map-type?
  [type-expr]
  (and (seq? type-expr)
       (= 'Map (first type-expr))
       (= 'String (second type-expr))))

(defn- primitive-or-special-type
  [type-expr]
  (or (get primitive-types type-expr)
      (get named-reference-types type-expr)
      (when (seq-type? type-expr)
        "java/util/List")
      (when (map-type? type-expr)
        "java/util/Map")))

(defn seq-element-type
  [type-expr]
  (cond
    (= 'StringSeq type-expr) 'String
    (and (seq? type-expr)
         (= 'Seq (first type-expr))
         (= 2 (count type-expr))) (second type-expr)
    :else (fail! "Expected lowered sequence type."
                 {:type type-expr})))

(defn map-value-type
  [type-expr]
  (if (and (seq? type-expr)
           (= 'Map (first type-expr))
           (= 3 (count type-expr))
           (= 'String (second type-expr)))
    (nth type-expr 2)
    (fail! "Expected lowered canonical map type."
           {:type type-expr})))

(defn- type-param-runtime-type
  [type-expr ctx]
  (when (and ctx
             (contains? (:type-params ctx) type-expr))
    "java/lang/Object"))

(defn- declared-or-java-type
  [type-expr ctx]
  (or (when (java-type? type-expr)
        (java-internal-name type-expr))
      (when (= :fn-type (:op type-expr))
        (closures/lower-closure-type type-expr ctx lower-type fail!))
      (when ctx
        (declared-type-name type-expr ctx))))

(defn lower-type
  ([type-expr]
   (lower-type type-expr nil))
  ([type-expr ctx]
   (or (primitive-or-special-type type-expr)
       (type-param-runtime-type type-expr ctx)
       (declared-or-java-type type-expr ctx)
       (fail! "Unsupported JVM type."
              {:type type-expr}))))

(defn lower-expr-type
  [type-expr bottom-types ctx]
  (if (bottom-types type-expr)
    :void
    (lower-type type-expr ctx)))

(defn decl-map
  [module]
  (into {} (map (juxt :name identity) (:decls module))))

(defn decl-for-type
  [ctx type-expr]
  (or (get (:decls ctx) (declared-type-root type-expr))
      (get-in ctx [:imported-decls (declared-type-root type-expr) :decl])))

(defn bind-local
  [ctx name type-expr]
  (assoc-in ctx [:locals name] type-expr))

(defn bind-mutable-local
  [ctx name type-expr]
  (-> ctx
      (bind-local name type-expr)
      (assoc-in [:mutable-locals name] type-expr)))

(defn bind-lambda
  [ctx name lambda-expr]
  (assoc-in ctx [:lambdas name] lambda-expr))

(defn with-loop-types
  [ctx loop-types]
  (assoc ctx :loop-types (vec loop-types)))

(defn fn-decl
  [ctx name]
  (if-let [decl (get (:decls ctx) name)]
    (when (= :fn (:op decl))
      (assoc decl :owner-module (:module-name ctx)))
    (when-let [{:keys [module decl]} (get (:imported-decls ctx) name)]
      (when (= :fn (:op decl))
        (assoc decl :owner-module module)))))

(defn local-lambda
  [ctx name]
  (get-in ctx [:lambdas name]))

(defn local-fn-type
  [ctx name]
  (let [type-expr (get-in ctx [:locals name])]
    (when (= :fn-type (:op type-expr))
      type-expr)))

(defn local-type
  [ctx name]
  (or (get-in ctx [:locals name])
      (when-let [decl (fn-decl ctx name)]
        {:op :fn-type
         :params (mapv :type (:params decl))
         :return-type (:return-type decl)
         :effects (vec (:effects decl))})
      (fail! "Unknown lowered local."
             {:name name})))

(defn mutable-local-type
  [ctx name]
  (get-in ctx [:mutable-locals name]))

(defn field-type
  ([decl field-name]
   (field-type decl field-name (:name decl)))
  ([decl field-name type-expr]
   (or (some (fn [field]
               (when (= field-name (:name field))
                 (instantiate-type (:type field)
                                   (type-bindings decl type-expr))))
             (:fields decl))
       (fail! "Unknown lowered field."
              {:field field-name
               :decl (:name decl)}))))

(defn- decl-runtime-ctx
  [ctx decl]
  (assoc ctx :type-params (set (:type-params decl))))

(defn runtime-field-jvm-types
  [ctx type-expr]
  (let [decl (decl-for-type ctx type-expr)]
    (mapv #(lower-type (:type %) (decl-runtime-ctx ctx decl))
          (:fields decl))))

(defn runtime-field-jvm-type
  [ctx type-expr field-name]
  (let [decl (decl-for-type ctx type-expr)]
    (or (some (fn [field]
                (when (= field-name (:name field))
                  (lower-type (:type field) (decl-runtime-ctx ctx decl))))
              (:fields decl))
        (fail! "Unknown lowered field."
               {:field field-name
                :decl (:name decl)}))))

(defn record-class-name
  [ctx type-name]
  (declared-type-name type-name ctx))

(defn union-variant
  [ctx union-name variant-name]
  (let [decl (decl-for-type ctx union-name)]
    (or (some (fn [variant]
                (when (= variant-name (:name variant))
                  (update variant
                          :fields
                          (fn [fields]
                            (mapv (fn [field]
                                    (update field
                                            :type
                                            instantiate-type
                                            (type-bindings decl union-name)))
                                  fields)))))
              (:variants decl))
        (fail! "Unknown lowered variant."
               {:type union-name
                :variant variant-name}))))

(defn runtime-union-variant-field-jvm-types
  [ctx union-name variant-name]
  (let [decl (decl-for-type ctx union-name)]
    (or (some (fn [variant]
                (when (= variant-name (:name variant))
                  (mapv #(lower-type (:type %) (decl-runtime-ctx ctx decl))
                        (:fields variant))))
              (:variants decl))
        (fail! "Unknown lowered variant."
               {:type union-name
                :variant variant-name}))))

(defn join-branch-types
  [bottom-types current next-branch data]
  (cond
    (bottom-types current) next-branch
    (bottom-types next-branch) current
    (= current next-branch) current
    :else (fail! "Lowered branch types must agree."
                 (assoc data
                        :expected current
                        :actual next-branch))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T09:36:20.749659-05:00", :module-hash "278143033", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1792666066"} {:id "def/primitive-types", :kind "def", :line 4, :end-line 9, :hash "-53859917"} {:id "def/named-reference-types", :kind "def", :line 11, :end-line 13, :hash "1891324621"} {:id "defn/declared-type-root", :kind "defn", :line 15, :end-line 19, :hash "1516159140"} {:id "defn-/type-bindings", :kind "defn-", :line 21, :end-line 26, :hash "360352681"} {:id "defn/instantiate-type", :kind "defn", :line 28, :end-line 33, :hash "-732736415"} {:id "defn/fail!", :kind "defn", :line 35, :end-line 37, :hash "2111196879"} {:id "defn/internal-name", :kind "defn", :line 39, :end-line 41, :hash "-1330388378"} {:id "defn/nested-class-name", :kind "defn", :line 43, :end-line 45, :hash "-69163617"} {:id "defn/union-variant-class-name", :kind "defn", :line 47, :end-line 49, :hash "1810905660"} {:id "defn/java-type?", :kind "defn", :line 51, :end-line 54, :hash "1469399228"} {:id "defn/java-internal-name", :kind "defn", :line 56, :end-line 58, :hash "-653806206"} {:id "defn-/declared-type-op?", :kind "defn-", :line 60, :end-line 62, :hash "1769082278"} {:id "defn-/declared-class-name", :kind "defn-", :line 64, :end-line 67, :hash "864307960"} {:id "defn-/local-declared-type-name", :kind "defn-", :line 69, :end-line 72, :hash "-288898901"} {:id "defn-/imported-declared-type-name", :kind "defn-", :line 74, :end-line 79, :hash "429449799"} {:id "defn/declared-type-name", :kind "defn", :line 81, :end-line 84, :hash "-1082717974"} {:id "defn/runtime-union-variant-class-name", :kind "defn", :line 86, :end-line 88, :hash "222768997"} {:id "form/18/declare", :kind "declare", :line 90, :end-line 90, :hash "921248303"} {:id "form/19/declare", :kind "declare", :line 91, :end-line 91, :hash "885577939"} {:id "form/20/declare", :kind "declare", :line 92, :end-line 92, :hash "-749778473"} {:id "form/21/declare", :kind "declare", :line 93, :end-line 93, :hash "-1494776713"} {:id "defn-/seq-type?", :kind "defn-", :line 95, :end-line 98, :hash "1535070684"} {:id "defn-/map-type?", :kind "defn-", :line 100, :end-line 104, :hash "-1731281190"} {:id "defn-/primitive-or-special-type", :kind "defn-", :line 106, :end-line 113, :hash "1814365464"} {:id "defn/seq-element-type", :kind "defn", :line 115, :end-line 123, :hash "1036848447"} {:id "defn/map-value-type", :kind "defn", :line 125, :end-line 133, :hash "1364261658"} {:id "defn-/type-param-runtime-type", :kind "defn-", :line 135, :end-line 139, :hash "397189850"} {:id "defn-/declared-or-java-type", :kind "defn-", :line 141, :end-line 148, :hash "-579406487"} {:id "defn/lower-type", :kind "defn", :line 150, :end-line 158, :hash "-694394494"} {:id "defn/lower-expr-type", :kind "defn", :line 160, :end-line 164, :hash "650136943"} {:id "defn/decl-map", :kind "defn", :line 166, :end-line 168, :hash "987686002"} {:id "defn/decl-for-type", :kind "defn", :line 170, :end-line 173, :hash "-532122863"} {:id "defn/bind-local", :kind "defn", :line 175, :end-line 177, :hash "1561458697"} {:id "defn/bind-mutable-local", :kind "defn", :line 179, :end-line 183, :hash "-741002783"} {:id "defn/bind-lambda", :kind "defn", :line 185, :end-line 187, :hash "1527841059"} {:id "defn/with-loop-types", :kind "defn", :line 189, :end-line 191, :hash "-2037637816"} {:id "defn/fn-decl", :kind "defn", :line 193, :end-line 200, :hash "-774885008"} {:id "defn/local-lambda", :kind "defn", :line 202, :end-line 204, :hash "-436400273"} {:id "defn/local-fn-type", :kind "defn", :line 206, :end-line 210, :hash "1216422506"} {:id "defn/local-type", :kind "defn", :line 212, :end-line 221, :hash "894152482"} {:id "defn/mutable-local-type", :kind "defn", :line 223, :end-line 225, :hash "791465470"} {:id "defn/field-type", :kind "defn", :line 227, :end-line 238, :hash "-15531062"} {:id "defn-/decl-runtime-ctx", :kind "defn-", :line 240, :end-line 242, :hash "790580277"} {:id "defn/runtime-field-jvm-types", :kind "defn", :line 244, :end-line 248, :hash "-1235133416"} {:id "defn/runtime-field-jvm-type", :kind "defn", :line 250, :end-line 259, :hash "-1107912378"} {:id "defn/record-class-name", :kind "defn", :line 261, :end-line 263, :hash "-747750698"} {:id "defn/union-variant", :kind "defn", :line 265, :end-line 282, :hash "1460327280"} {:id "defn/runtime-union-variant-field-jvm-types", :kind "defn", :line 284, :end-line 294, :hash "-1924932217"} {:id "defn/join-branch-types", :kind "defn", :line 296, :end-line 305, :hash "526632981"}]}
;; clj-mutate-manifest-end
