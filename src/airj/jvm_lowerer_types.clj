(ns airj.jvm-lowerer-types
  (:require [airj.jvm-closures :as closures]
            [airj.jvm-lowerer-match :as lowerer-match]))

(def recur-type ::recur)
(def raise-type ::raise)
(def bottom-types #{recur-type raise-type})

(def ^:private primitive-types
  {'Int :int
   'Float :float
   'Double :double
   'Bool :boolean
   'Unit :void})

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

(defn- instantiate-type
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
(declare infer-type)
(declare fn-decl)
(declare local-lambda)
(declare local-fn-type)

(defn- primitive-or-special-type
  [type-expr]
  (or (get primitive-types type-expr)
      (when (= 'String type-expr)
        "java/lang/String")
      (when (= 'StringSeq type-expr)
        "[Ljava/lang/String;")))

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
  [type-expr ctx]
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
  [current next-branch data]
  (cond
    (bottom-types current) next-branch
    (bottom-types next-branch) current
    (= current next-branch) current
    :else (fail! "Lowered branch types must agree."
                 (assoc data
                        :expected current
                        :actual next-branch))))

(defn- infer-literal-type
  [expr]
  (or (when (integer? expr)
        'Int)
      (when (instance? Float expr)
        'Float)
      (when (instance? Double expr)
        'Double)
      (when (string? expr)
        'String)
      (when (or (true? expr) (false? expr))
        'Bool)))

(defn- infer-record-get-type
  [expr ctx]
  (let [target-type (infer-type (:target expr) ctx)
        target-decl (decl-for-type ctx target-type)]
    (field-type target-decl (:field expr) target-type)))

(defn- local-lambda-return-type
  [ctx callee]
  (when-let [lambda-expr (local-lambda ctx (:name callee))]
    (:return-type lambda-expr)))

(defn- named-call-return-type
  [ctx callee]
  (when-let [target (fn-decl ctx (:name callee))]
    (:return-type target)))

(defn- infer-call-type
  [expr ctx]
  (let [callee (:callee expr)]
    (or (when (= :lambda (:op callee))
          (:return-type callee))
        (when (= :local (:op callee))
          (local-lambda-return-type ctx callee))
        (when (= :local (:op callee))
          (:return-type (local-fn-type ctx (:name callee))))
        (named-call-return-type ctx callee)
        (fail! "Unknown lowered call target."
               {:callee callee}))))

(defn- infer-if-type
  [expr ctx]
  (let [then-type (infer-type (:then expr) ctx)
        else-type (infer-type (:else expr) ctx)]
    (join-branch-types then-type
                       else-type
                       {:then then-type
                        :else else-type})))

(defn- infer-seq-type
  [expr ctx]
  (let [[_ final-ctx]
        (reduce (fn [[_ current-ctx] subexpr]
                  [nil
                   (if (= :var (:op subexpr))
                     (bind-local current-ctx
                                 (:name subexpr)
                                 (:type subexpr))
                     current-ctx)])
                [nil ctx]
                (butlast (:exprs expr)))]
    (infer-type (last (:exprs expr)) final-ctx)))

(defn- infer-let-type
  [expr ctx]
  (let [[_ body-ctx]
        (reduce (fn [[_ current-ctx] binding]
                  (let [binding-type (infer-type (:expr binding) current-ctx)
                        next-ctx (cond-> (bind-local current-ctx
                                                     (:name binding)
                                                     binding-type)
                                   (= :lambda (:op (:expr binding)))
                                   (bind-lambda (:name binding) (:expr binding)))]
                    [nil next-ctx]))
                [nil ctx]
                (:bindings expr))]
    (infer-type (:body expr) body-ctx)))

(defn- infer-lambda
  [expr _ctx]
  {:op :fn-type
   :params (mapv :type (:params expr))
   :return-type (:return-type expr)
   :effects (vec (:effects expr))})

(defn- infer-loop
  [expr ctx]
  (let [{:keys [body-ctx loop-types]}
        (reduce (fn [{:keys [body-ctx loop-types]} binding]
                  (let [binding-type (infer-type (:expr binding) body-ctx)]
                    {:body-ctx (bind-local body-ctx
                                           (:name binding)
                                           binding-type)
                     :loop-types (conj loop-types binding-type)}))
                {:body-ctx ctx
                 :loop-types []}
                (:bindings expr))]
    (infer-type (:body expr) (with-loop-types body-ctx loop-types))))

(defn- infer-recur
  [expr ctx]
  (let [loop-types (:loop-types ctx)]
    (when-not loop-types
      (fail! "Recur used outside lowered loop."
             {:expr expr}))
    (when-not (= (count loop-types) (count (:args expr)))
      (fail! "Lowered recur arity mismatch."
             {:expected (count loop-types)
              :actual (count (:args expr))}))
    (doseq [[expected-type arg] (map vector loop-types (:args expr))]
      (let [arg-type (infer-type arg ctx)]
        (when-not (= expected-type arg-type)
          (fail! "Lowered recur argument type mismatch."
                 {:expected expected-type
                  :actual arg-type
                  :arg arg}))))
    recur-type))

(defn- infer-try
  [expr ctx]
  (let [body-type (infer-type (:body expr) ctx)
        catch-types (mapv (fn [catch]
                            (infer-type (:body catch)
                                        (bind-local ctx (:name catch) (:type catch))))
                          (:catches expr))]
    (when-let [finally-expr (:finally expr)]
      (infer-type finally-expr ctx))
    (reduce (fn [current catch-type]
              (join-branch-types current
                                 catch-type
                                 {:expr expr}))
            body-type
            catch-types)))

(defn- infer-raise
  [expr ctx]
  (infer-type (:expr expr) ctx)
  raise-type)

(def ^:private infer-type-handlers
  {:local (fn [expr ctx] (local-type ctx (:name expr)))
   :let infer-let-type
   :lambda infer-lambda
   :var (fn [_ _] 'Unit)
   :set (fn [_ _] 'Unit)
   :loop infer-loop
   :recur infer-recur
   :try infer-try
   :raise infer-raise
   :construct (fn [expr _] (:type expr))
   :variant (fn [expr _] (:type expr))
   :call infer-call-type
   :int-add (fn [_ _] 'Int)
   :int-sub (fn [_ _] 'Int)
   :int-mul (fn [_ _] 'Int)
   :int-div (fn [_ _] 'Int)
   :int-mod (fn [_ _] 'Int)
   :int-eq (fn [_ _] 'Bool)
   :int-lt (fn [_ _] 'Bool)
   :int-le (fn [_ _] 'Bool)
   :int-gt (fn [_ _] 'Bool)
   :int-ge (fn [_ _] 'Bool)
   :float-add (fn [_ _] 'Float)
   :float-sub (fn [_ _] 'Float)
   :float-mul (fn [_ _] 'Float)
   :float-div (fn [_ _] 'Float)
   :float-eq (fn [_ _] 'Bool)
   :float-lt (fn [_ _] 'Bool)
   :float-le (fn [_ _] 'Bool)
   :float-gt (fn [_ _] 'Bool)
   :float-ge (fn [_ _] 'Bool)
   :double-add (fn [_ _] 'Double)
   :double-sub (fn [_ _] 'Double)
   :double-mul (fn [_ _] 'Double)
   :double-div (fn [_ _] 'Double)
   :double-eq (fn [_ _] 'Bool)
   :double-lt (fn [_ _] 'Bool)
   :double-le (fn [_ _] 'Bool)
   :double-gt (fn [_ _] 'Bool)
   :double-ge (fn [_ _] 'Bool)
   :bool-eq (fn [_ _] 'Bool)
   :int-ne (fn [_ _] 'Bool)
   :string-eq (fn [_ _] 'Bool)
   :string-concat (fn [_ _] 'String)
   :string-split-on (fn [_ _] 'StringSeq)
   :string-char-at (fn [_ _] 'String)
   :string-substring (fn [_ _] 'String)
   :int->string (fn [_ _] 'String)
   :int->float (fn [_ _] 'Float)
   :int->double (fn [_ _] 'Double)
   :float->double (fn [_ _] 'Double)
   :double->float (fn [_ _] 'Float)
   :string->int (fn [_ _] 'Int)
   :string-length (fn [_ _] 'Int)
   :string-trim (fn [_ _] 'String)
   :string-empty? (fn [_ _] 'Bool)
   :seq-empty? (fn [_ _] 'Bool)
   :seq-length (fn [_ _] 'Int)
   :seq-first (fn [_ _] 'String)
   :seq-get (fn [_ _] 'String)
   :io-read-line (fn [_ _] 'String)
   :io-print (fn [_ _] 'Unit)
   :io-println (fn [_ _] 'Unit)
   :bool-not (fn [_ _] 'Bool)
   :bool-and (fn [_ _] 'Bool)
   :bool-or (fn [_ _] 'Bool)
   :record-get infer-record-get-type
   :if infer-if-type
   :match (fn [expr ctx]
            (lowerer-match/infer-match-type expr
                                            ctx
                                            {:fail! fail!
                                             :infer-type infer-type
                                             :bind-local bind-local
                                             :join-branch-types join-branch-types
                                             :union-variant union-variant}))
   :seq infer-seq-type
   :java-new (fn [expr _] (list 'Java (:class-name expr)))
   :java-call (fn [expr _] (get-in expr [:signature :return-type]))
   :java-static-call (fn [expr _] (get-in expr [:signature :return-type]))
   :java-get-field (fn [expr _] (:field-type expr))
   :java-set-field (fn [_ _] 'Unit)
   :java-static-get-field (fn [expr _] (:field-type expr))
   :java-static-set-field (fn [_ _] 'Unit)})

(defn infer-type
  [expr ctx]
  (or (infer-literal-type expr)
      (if-let [handler (get infer-type-handlers (:op expr))]
        (handler expr ctx)
        (fail! "Unsupported lowered type inference."
               {:expr expr}))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T06:57:47.051407-05:00", :module-hash "580440757", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "360843331"} {:id "def/recur-type", :kind "def", :line 5, :end-line 5, :hash "-2071027665"} {:id "def/raise-type", :kind "def", :line 6, :end-line 6, :hash "692699866"} {:id "def/bottom-types", :kind "def", :line 7, :end-line 7, :hash "1188722171"} {:id "def/primitive-types", :kind "def", :line 9, :end-line 14, :hash "-53859917"} {:id "defn/declared-type-root", :kind "defn", :line 16, :end-line 20, :hash "1516159140"} {:id "defn-/type-bindings", :kind "defn-", :line 22, :end-line 27, :hash "360352681"} {:id "defn-/instantiate-type", :kind "defn-", :line 29, :end-line 34, :hash "1150212064"} {:id "defn/fail!", :kind "defn", :line 36, :end-line 38, :hash "2111196879"} {:id "defn/internal-name", :kind "defn", :line 40, :end-line 42, :hash "-1330388378"} {:id "defn/nested-class-name", :kind "defn", :line 44, :end-line 46, :hash "-69163617"} {:id "defn/union-variant-class-name", :kind "defn", :line 48, :end-line 50, :hash "1810905660"} {:id "defn/java-type?", :kind "defn", :line 52, :end-line 55, :hash "1469399228"} {:id "defn/java-internal-name", :kind "defn", :line 57, :end-line 59, :hash "-653806206"} {:id "defn-/declared-type-op?", :kind "defn-", :line 61, :end-line 63, :hash "1769082278"} {:id "defn-/declared-class-name", :kind "defn-", :line 65, :end-line 68, :hash "864307960"} {:id "defn-/local-declared-type-name", :kind "defn-", :line 70, :end-line 73, :hash "-288898901"} {:id "defn-/imported-declared-type-name", :kind "defn-", :line 75, :end-line 80, :hash "429449799"} {:id "defn/declared-type-name", :kind "defn", :line 82, :end-line 85, :hash "-1082717974"} {:id "defn/runtime-union-variant-class-name", :kind "defn", :line 87, :end-line 89, :hash "222768997"} {:id "form/20/declare", :kind "declare", :line 91, :end-line 91, :hash "921248303"} {:id "form/21/declare", :kind "declare", :line 92, :end-line 92, :hash "346281442"} {:id "form/22/declare", :kind "declare", :line 93, :end-line 93, :hash "885577939"} {:id "form/23/declare", :kind "declare", :line 94, :end-line 94, :hash "-749778473"} {:id "form/24/declare", :kind "declare", :line 95, :end-line 95, :hash "-1494776713"} {:id "defn-/primitive-or-special-type", :kind "defn-", :line 97, :end-line 103, :hash "1719522028"} {:id "defn-/type-param-runtime-type", :kind "defn-", :line 105, :end-line 109, :hash "397189850"} {:id "defn-/declared-or-java-type", :kind "defn-", :line 111, :end-line 118, :hash "-579406487"} {:id "defn/lower-type", :kind "defn", :line 120, :end-line 128, :hash "-694394494"} {:id "defn/lower-expr-type", :kind "defn", :line 130, :end-line 134, :hash "1513759038"} {:id "defn/decl-map", :kind "defn", :line 136, :end-line 138, :hash "987686002"} {:id "defn/decl-for-type", :kind "defn", :line 140, :end-line 143, :hash "-532122863"} {:id "defn/bind-local", :kind "defn", :line 145, :end-line 147, :hash "1561458697"} {:id "defn/bind-mutable-local", :kind "defn", :line 149, :end-line 153, :hash "-741002783"} {:id "defn/bind-lambda", :kind "defn", :line 155, :end-line 157, :hash "1527841059"} {:id "defn/with-loop-types", :kind "defn", :line 159, :end-line 161, :hash "-2037637816"} {:id "defn/local-type", :kind "defn", :line 163, :end-line 172, :hash "894152482"} {:id "defn/mutable-local-type", :kind "defn", :line 174, :end-line 176, :hash "791465470"} {:id "defn/field-type", :kind "defn", :line 178, :end-line 189, :hash "-15531062"} {:id "defn-/decl-runtime-ctx", :kind "defn-", :line 191, :end-line 193, :hash "790580277"} {:id "defn/runtime-field-jvm-types", :kind "defn", :line 195, :end-line 199, :hash "-122114376"} {:id "defn/runtime-field-jvm-type", :kind "defn", :line 201, :end-line 210, :hash "-1107912378"} {:id "defn/record-class-name", :kind "defn", :line 212, :end-line 214, :hash "-747750698"} {:id "defn/fn-decl", :kind "defn", :line 216, :end-line 223, :hash "-774885008"} {:id "defn/local-lambda", :kind "defn", :line 225, :end-line 227, :hash "-436400273"} {:id "defn/local-fn-type", :kind "defn", :line 229, :end-line 233, :hash "1216422506"} {:id "defn/union-variant", :kind "defn", :line 235, :end-line 252, :hash "1460327280"} {:id "defn/runtime-union-variant-field-jvm-types", :kind "defn", :line 254, :end-line 264, :hash "-82967641"} {:id "defn/join-branch-types", :kind "defn", :line 266, :end-line 275, :hash "-2130619843"} {:id "defn-/infer-literal-type", :kind "defn-", :line 277, :end-line 288, :hash "-2072052525"} {:id "defn-/infer-record-get-type", :kind "defn-", :line 290, :end-line 294, :hash "-1320321895"} {:id "defn-/local-lambda-return-type", :kind "defn-", :line 296, :end-line 299, :hash "-377855316"} {:id "defn-/named-call-return-type", :kind "defn-", :line 301, :end-line 304, :hash "737420465"} {:id "defn-/infer-call-type", :kind "defn-", :line 306, :end-line 317, :hash "910305658"} {:id "defn-/infer-if-type", :kind "defn-", :line 319, :end-line 326, :hash "-2112938377"} {:id "defn-/infer-seq-type", :kind "defn-", :line 328, :end-line 340, :hash "604684926"} {:id "defn-/infer-let-type", :kind "defn-", :line 342, :end-line 355, :hash "327096300"} {:id "defn-/infer-lambda", :kind "defn-", :line 357, :end-line 362, :hash "-1117964483"} {:id "defn-/infer-loop", :kind "defn-", :line 364, :end-line 376, :hash "-46270947"} {:id "defn-/infer-recur", :kind "defn-", :line 378, :end-line 395, :hash "-1368982370"} {:id "defn-/infer-try", :kind "defn-", :line 397, :end-line 411, :hash "641639781"} {:id "defn-/infer-raise", :kind "defn-", :line 413, :end-line 416, :hash "465355386"} {:id "def/infer-type-handlers", :kind "def", :line 418, :end-line 502, :hash "2128942467"} {:id "defn/infer-type", :kind "defn", :line 504, :end-line 510, :hash "600232947"}]}
;; clj-mutate-manifest-end
