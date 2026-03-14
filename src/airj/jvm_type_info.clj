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
   'Bytes "[B"
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
;; {:version 1, :tested-at "2026-03-14T14:38:43.594583-05:00", :module-hash "-1717351289", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1792666066"} {:id "def/primitive-types", :kind "def", :line 4, :end-line 9, :hash "-53859917"} {:id "def/named-reference-types", :kind "def", :line 11, :end-line 14, :hash "2028503680"} {:id "defn/declared-type-root", :kind "defn", :line 16, :end-line 20, :hash "1516159140"} {:id "defn-/type-bindings", :kind "defn-", :line 22, :end-line 27, :hash "360352681"} {:id "defn/instantiate-type", :kind "defn", :line 29, :end-line 34, :hash "-732736415"} {:id "defn/fail!", :kind "defn", :line 36, :end-line 38, :hash "2111196879"} {:id "defn/internal-name", :kind "defn", :line 40, :end-line 42, :hash "-1330388378"} {:id "defn/nested-class-name", :kind "defn", :line 44, :end-line 46, :hash "-69163617"} {:id "defn/union-variant-class-name", :kind "defn", :line 48, :end-line 50, :hash "1810905660"} {:id "defn/java-type?", :kind "defn", :line 52, :end-line 55, :hash "1469399228"} {:id "defn/java-internal-name", :kind "defn", :line 57, :end-line 59, :hash "-653806206"} {:id "defn-/declared-type-op?", :kind "defn-", :line 61, :end-line 63, :hash "1769082278"} {:id "defn-/declared-class-name", :kind "defn-", :line 65, :end-line 68, :hash "864307960"} {:id "defn-/local-declared-type-name", :kind "defn-", :line 70, :end-line 73, :hash "-288898901"} {:id "defn-/imported-declared-type-name", :kind "defn-", :line 75, :end-line 80, :hash "429449799"} {:id "defn/declared-type-name", :kind "defn", :line 82, :end-line 85, :hash "-1082717974"} {:id "defn/runtime-union-variant-class-name", :kind "defn", :line 87, :end-line 89, :hash "222768997"} {:id "form/18/declare", :kind "declare", :line 91, :end-line 91, :hash "921248303"} {:id "form/19/declare", :kind "declare", :line 92, :end-line 92, :hash "885577939"} {:id "form/20/declare", :kind "declare", :line 93, :end-line 93, :hash "-749778473"} {:id "form/21/declare", :kind "declare", :line 94, :end-line 94, :hash "-1494776713"} {:id "defn-/seq-type?", :kind "defn-", :line 96, :end-line 99, :hash "1535070684"} {:id "defn-/map-type?", :kind "defn-", :line 101, :end-line 105, :hash "-1731281190"} {:id "defn-/primitive-or-special-type", :kind "defn-", :line 107, :end-line 114, :hash "1814365464"} {:id "defn/seq-element-type", :kind "defn", :line 116, :end-line 124, :hash "1036848447"} {:id "defn/map-value-type", :kind "defn", :line 126, :end-line 134, :hash "1364261658"} {:id "defn-/type-param-runtime-type", :kind "defn-", :line 136, :end-line 140, :hash "397189850"} {:id "defn-/declared-or-java-type", :kind "defn-", :line 142, :end-line 149, :hash "-579406487"} {:id "defn/lower-type", :kind "defn", :line 151, :end-line 159, :hash "-694394494"} {:id "defn/lower-expr-type", :kind "defn", :line 161, :end-line 165, :hash "650136943"} {:id "defn/decl-map", :kind "defn", :line 167, :end-line 169, :hash "987686002"} {:id "defn/decl-for-type", :kind "defn", :line 171, :end-line 174, :hash "-532122863"} {:id "defn/bind-local", :kind "defn", :line 176, :end-line 178, :hash "1561458697"} {:id "defn/bind-mutable-local", :kind "defn", :line 180, :end-line 184, :hash "-741002783"} {:id "defn/bind-lambda", :kind "defn", :line 186, :end-line 188, :hash "1527841059"} {:id "defn/with-loop-types", :kind "defn", :line 190, :end-line 192, :hash "-2037637816"} {:id "defn/fn-decl", :kind "defn", :line 194, :end-line 201, :hash "-774885008"} {:id "defn/local-lambda", :kind "defn", :line 203, :end-line 205, :hash "-436400273"} {:id "defn/local-fn-type", :kind "defn", :line 207, :end-line 211, :hash "1216422506"} {:id "defn/local-type", :kind "defn", :line 213, :end-line 222, :hash "894152482"} {:id "defn/mutable-local-type", :kind "defn", :line 224, :end-line 226, :hash "791465470"} {:id "defn/field-type", :kind "defn", :line 228, :end-line 239, :hash "-15531062"} {:id "defn-/decl-runtime-ctx", :kind "defn-", :line 241, :end-line 243, :hash "790580277"} {:id "defn/runtime-field-jvm-types", :kind "defn", :line 245, :end-line 249, :hash "-1235133416"} {:id "defn/runtime-field-jvm-type", :kind "defn", :line 251, :end-line 260, :hash "-1107912378"} {:id "defn/record-class-name", :kind "defn", :line 262, :end-line 264, :hash "-747750698"} {:id "defn/union-variant", :kind "defn", :line 266, :end-line 283, :hash "1460327280"} {:id "defn/runtime-union-variant-field-jvm-types", :kind "defn", :line 285, :end-line 295, :hash "-1924932217"} {:id "defn/join-branch-types", :kind "defn", :line 297, :end-line 306, :hash "526632981"}]}
;; clj-mutate-manifest-end
