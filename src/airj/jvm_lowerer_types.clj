(ns airj.jvm-lowerer-types
  (:require [airj.jvm-closures :as closures]
            [airj.jvm-lowerer-match :as lowerer-match]))

(def recur-type ::recur)
(def raise-type ::raise)
(def bottom-types #{recur-type raise-type})

(def ^:private primitive-types
  {'Int :int
   'Bool :boolean
   'Unit :void})

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
  (some->> (get (:decls ctx) type-expr)
           (declared-class-name (:module-name ctx))))

(defn- imported-declared-type-name
  [type-expr ctx]
  (some (fn [[_ {:keys [module decl]}]]
          (when (= type-expr (:name decl))
            (declared-class-name module decl)))
        (:imported-decls ctx)))

(defn declared-type-name
  [type-expr ctx]
  (or (local-declared-type-name type-expr ctx)
      (imported-declared-type-name type-expr ctx)))

(declare lower-type)
(declare infer-type)
(declare fn-decl)
(declare local-lambda)
(declare local-fn-type)

(defn lower-type
  ([type-expr]
   (lower-type type-expr nil))
  ([type-expr ctx]
   (or (get primitive-types type-expr)
       (when (= 'String type-expr)
         "java/lang/String")
       (when (= 'StringSeq type-expr)
         "[Ljava/lang/String;")
       (when (java-type? type-expr)
         (java-internal-name type-expr))
       (when (= :fn-type (:op type-expr))
         (closures/lower-closure-type type-expr ctx lower-type fail!))
       (when ctx
         (declared-type-name type-expr ctx))
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
  [decl field-name]
  (or (some (fn [field]
              (when (= field-name (:name field))
                (:type field)))
            (:fields decl))
      (fail! "Unknown lowered field."
             {:field field-name
              :decl (:name decl)})))

(defn record-class-name
  [ctx type-name]
  (nested-class-name (:module-name ctx) type-name))

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
  (or (some (fn [variant]
              (when (= variant-name (:name variant))
                variant))
            (:variants (get (:decls ctx) union-name)))
      (fail! "Unknown lowered variant."
             {:type union-name
              :variant variant-name})))

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
  (cond
    (integer? expr) 'Int
    (string? expr) 'String
    (true? expr) 'Bool
    (false? expr) 'Bool
    :else nil))

(defn- infer-record-get-type
  [expr ctx]
  (let [target-type (infer-type (:target expr) ctx)
        target-decl (get (:decls ctx) target-type)]
    (field-type target-decl (:field expr))))

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
   :bool-eq (fn [_ _] 'Bool)
   :int-ne (fn [_ _] 'Bool)
   :string-eq (fn [_ _] 'Bool)
   :string-concat (fn [_ _] 'String)
   :int->string (fn [_ _] 'String)
   :string->int (fn [_ _] 'Int)
   :string-length (fn [_ _] 'Int)
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
;; {:version 1, :tested-at "2026-03-13T13:48:53.346458-05:00", :module-hash "52041313", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "360843331"} {:id "def/recur-type", :kind "def", :line 5, :end-line 5, :hash "-2071027665"} {:id "def/raise-type", :kind "def", :line 6, :end-line 6, :hash "692699866"} {:id "def/bottom-types", :kind "def", :line 7, :end-line 7, :hash "1188722171"} {:id "def/primitive-types", :kind "def", :line 9, :end-line 12, :hash "-746134113"} {:id "defn/fail!", :kind "defn", :line 14, :end-line 16, :hash "2111196879"} {:id "defn/internal-name", :kind "defn", :line 18, :end-line 20, :hash "-1330388378"} {:id "defn/nested-class-name", :kind "defn", :line 22, :end-line 24, :hash "-69163617"} {:id "defn/union-variant-class-name", :kind "defn", :line 26, :end-line 28, :hash "1810905660"} {:id "defn/java-type?", :kind "defn", :line 30, :end-line 33, :hash "1469399228"} {:id "defn/java-internal-name", :kind "defn", :line 35, :end-line 37, :hash "-653806206"} {:id "defn-/declared-type-op?", :kind "defn-", :line 39, :end-line 41, :hash "1769082278"} {:id "defn-/declared-class-name", :kind "defn-", :line 43, :end-line 46, :hash "864307960"} {:id "defn-/local-declared-type-name", :kind "defn-", :line 48, :end-line 51, :hash "448982123"} {:id "defn-/imported-declared-type-name", :kind "defn-", :line 53, :end-line 58, :hash "-1013484411"} {:id "defn/declared-type-name", :kind "defn", :line 60, :end-line 63, :hash "-1082717974"} {:id "form/16/declare", :kind "declare", :line 65, :end-line 65, :hash "921248303"} {:id "form/17/declare", :kind "declare", :line 66, :end-line 66, :hash "346281442"} {:id "form/18/declare", :kind "declare", :line 67, :end-line 67, :hash "885577939"} {:id "form/19/declare", :kind "declare", :line 68, :end-line 68, :hash "-749778473"} {:id "form/20/declare", :kind "declare", :line 69, :end-line 69, :hash "-1494776713"} {:id "defn/lower-type", :kind "defn", :line 71, :end-line 87, :hash "-1615388979"} {:id "defn/lower-expr-type", :kind "defn", :line 89, :end-line 93, :hash "1513759038"} {:id "defn/decl-map", :kind "defn", :line 95, :end-line 97, :hash "987686002"} {:id "defn/bind-local", :kind "defn", :line 99, :end-line 101, :hash "1561458697"} {:id "defn/bind-mutable-local", :kind "defn", :line 103, :end-line 107, :hash "-741002783"} {:id "defn/bind-lambda", :kind "defn", :line 109, :end-line 111, :hash "1527841059"} {:id "defn/with-loop-types", :kind "defn", :line 113, :end-line 115, :hash "-2037637816"} {:id "defn/local-type", :kind "defn", :line 117, :end-line 126, :hash "894152482"} {:id "defn/mutable-local-type", :kind "defn", :line 128, :end-line 130, :hash "791465470"} {:id "defn/field-type", :kind "defn", :line 132, :end-line 140, :hash "1633531799"} {:id "defn/record-class-name", :kind "defn", :line 142, :end-line 144, :hash "1389626956"} {:id "defn/fn-decl", :kind "defn", :line 146, :end-line 153, :hash "-774885008"} {:id "defn/local-lambda", :kind "defn", :line 155, :end-line 157, :hash "-436400273"} {:id "defn/local-fn-type", :kind "defn", :line 159, :end-line 163, :hash "1216422506"} {:id "defn/union-variant", :kind "defn", :line 165, :end-line 173, :hash "-1286541301"} {:id "defn/join-branch-types", :kind "defn", :line 175, :end-line 184, :hash "-2130619843"} {:id "defn-/infer-literal-type", :kind "defn-", :line 186, :end-line 193, :hash "-495777708"} {:id "defn-/infer-record-get-type", :kind "defn-", :line 195, :end-line 199, :hash "-612672585"} {:id "defn-/local-lambda-return-type", :kind "defn-", :line 201, :end-line 204, :hash "-377855316"} {:id "defn-/named-call-return-type", :kind "defn-", :line 206, :end-line 209, :hash "737420465"} {:id "defn-/infer-call-type", :kind "defn-", :line 211, :end-line 222, :hash "910305658"} {:id "defn-/infer-if-type", :kind "defn-", :line 224, :end-line 231, :hash "-2112938377"} {:id "defn-/infer-seq-type", :kind "defn-", :line 233, :end-line 245, :hash "604684926"} {:id "defn-/infer-let-type", :kind "defn-", :line 247, :end-line 260, :hash "327096300"} {:id "defn-/infer-lambda", :kind "defn-", :line 262, :end-line 267, :hash "-1117964483"} {:id "defn-/infer-loop", :kind "defn-", :line 269, :end-line 281, :hash "-46270947"} {:id "defn-/infer-recur", :kind "defn-", :line 283, :end-line 300, :hash "-1368982370"} {:id "defn-/infer-try", :kind "defn-", :line 302, :end-line 316, :hash "641639781"} {:id "defn-/infer-raise", :kind "defn-", :line 318, :end-line 321, :hash "465355386"} {:id "def/infer-type-handlers", :kind "def", :line 323, :end-line 376, :hash "-1452403554"} {:id "defn/infer-type", :kind "defn", :line 378, :end-line 384, :hash "600232947"}]}
;; clj-mutate-manifest-end
