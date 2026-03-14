(ns airj.jvm-lowerer-types
  (:require [airj.jvm-lowerer-match :as lowerer-match]
            [airj.jvm-type-info :as type-info]))

(def recur-type ::recur)
(def raise-type ::raise)
(def bottom-types #{recur-type raise-type})

(def declared-type-root type-info/declared-type-root)
(def instantiate-type type-info/instantiate-type)
(def fail! type-info/fail!)
(def internal-name type-info/internal-name)
(def nested-class-name type-info/nested-class-name)
(def union-variant-class-name type-info/union-variant-class-name)
(def java-type? type-info/java-type?)
(def java-internal-name type-info/java-internal-name)
(def declared-type-name type-info/declared-type-name)
(def runtime-union-variant-class-name type-info/runtime-union-variant-class-name)

(declare lower-type)
(declare infer-type)
(declare fn-decl)
(declare local-lambda)
(declare local-fn-type)

(def seq-element-type type-info/seq-element-type)
(def map-value-type type-info/map-value-type)
(def lower-type type-info/lower-type)

(defn lower-expr-type
  [type-expr ctx]
  (if (bottom-types type-expr)
    :void
    (type-info/lower-expr-type type-expr bottom-types ctx)))

(def decl-map type-info/decl-map)
(def decl-for-type type-info/decl-for-type)
(def bind-local type-info/bind-local)
(def bind-mutable-local type-info/bind-mutable-local)
(def bind-lambda type-info/bind-lambda)
(def with-loop-types type-info/with-loop-types)
(def local-type type-info/local-type)
(def mutable-local-type type-info/mutable-local-type)
(def field-type type-info/field-type)
(def runtime-field-jvm-types type-info/runtime-field-jvm-types)
(def runtime-field-jvm-type type-info/runtime-field-jvm-type)
(def record-class-name type-info/record-class-name)
(def fn-decl type-info/fn-decl)
(def local-lambda type-info/local-lambda)
(def local-fn-type type-info/local-fn-type)
(def union-variant type-info/union-variant)
(def runtime-union-variant-field-jvm-types type-info/runtime-union-variant-field-jvm-types)
(defn join-branch-types
  [current next-branch data]
  (type-info/join-branch-types bottom-types current next-branch data))

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
   :string-split-on (fn [_ _] '(Seq String))
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
   :seq-first (fn [expr ctx] (seq-element-type (infer-type (:arg expr) ctx)))
   :seq-rest (fn [expr ctx] (list 'Seq (seq-element-type (infer-type (:arg expr) ctx))))
   :seq-concat (fn [expr ctx] (list 'Seq (seq-element-type (infer-type (first (:args expr)) ctx))))
   :seq-get (fn [expr ctx] (seq-element-type (infer-type (first (:args expr)) ctx)))
   :map-empty (fn [expr _] (list 'Map 'String (:value-type expr)))
   :map-set (fn [expr ctx] (infer-type (first (:args expr)) ctx))
   :map-get (fn [expr ctx] (list 'Option (map-value-type (infer-type (first (:args expr)) ctx))))
   :map-contains? (fn [_ _] 'Bool)
   :map-keys (fn [_ _] '(Seq String))
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
;; {:version 1, :tested-at "2026-03-14T09:39:04.915652-05:00", :module-hash "-1933829332", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "1324800056"} {:id "def/recur-type", :kind "def", :line 5, :end-line 5, :hash "-2071027665"} {:id "def/raise-type", :kind "def", :line 6, :end-line 6, :hash "692699866"} {:id "def/bottom-types", :kind "def", :line 7, :end-line 7, :hash "1188722171"} {:id "def/declared-type-root", :kind "def", :line 9, :end-line 9, :hash "-820643466"} {:id "def/instantiate-type", :kind "def", :line 10, :end-line 10, :hash "506690307"} {:id "def/fail!", :kind "def", :line 11, :end-line 11, :hash "-1234082344"} {:id "def/internal-name", :kind "def", :line 12, :end-line 12, :hash "1104241349"} {:id "def/nested-class-name", :kind "def", :line 13, :end-line 13, :hash "1507251270"} {:id "def/union-variant-class-name", :kind "def", :line 14, :end-line 14, :hash "1845129393"} {:id "def/java-type?", :kind "def", :line 15, :end-line 15, :hash "863683803"} {:id "def/java-internal-name", :kind "def", :line 16, :end-line 16, :hash "553123045"} {:id "def/declared-type-name", :kind "def", :line 17, :end-line 17, :hash "-1358919610"} {:id "def/runtime-union-variant-class-name", :kind "def", :line 18, :end-line 18, :hash "-327128136"} {:id "form/14/declare", :kind "declare", :line 20, :end-line 20, :hash "921248303"} {:id "form/15/declare", :kind "declare", :line 21, :end-line 21, :hash "346281442"} {:id "form/16/declare", :kind "declare", :line 22, :end-line 22, :hash "885577939"} {:id "form/17/declare", :kind "declare", :line 23, :end-line 23, :hash "-749778473"} {:id "form/18/declare", :kind "declare", :line 24, :end-line 24, :hash "-1494776713"} {:id "def/seq-element-type", :kind "def", :line 26, :end-line 26, :hash "749716170"} {:id "def/map-value-type", :kind "def", :line 27, :end-line 27, :hash "-1953103181"} {:id "def/lower-type", :kind "def", :line 28, :end-line 28, :hash "-2104515439"} {:id "defn/lower-expr-type", :kind "defn", :line 30, :end-line 34, :hash "-220430031"} {:id "def/decl-map", :kind "def", :line 36, :end-line 36, :hash "-1096496128"} {:id "def/decl-for-type", :kind "def", :line 37, :end-line 37, :hash "123583536"} {:id "def/bind-local", :kind "def", :line 38, :end-line 38, :hash "1010792415"} {:id "def/bind-mutable-local", :kind "def", :line 39, :end-line 39, :hash "1415151668"} {:id "def/bind-lambda", :kind "def", :line 40, :end-line 40, :hash "-1010465658"} {:id "def/with-loop-types", :kind "def", :line 41, :end-line 41, :hash "2074726521"} {:id "def/local-type", :kind "def", :line 42, :end-line 42, :hash "-184716702"} {:id "def/mutable-local-type", :kind "def", :line 43, :end-line 43, :hash "2061938541"} {:id "def/field-type", :kind "def", :line 44, :end-line 44, :hash "2037394996"} {:id "def/runtime-field-jvm-types", :kind "def", :line 45, :end-line 45, :hash "-1204637311"} {:id "def/runtime-field-jvm-type", :kind "def", :line 46, :end-line 46, :hash "991757085"} {:id "def/record-class-name", :kind "def", :line 47, :end-line 47, :hash "1011763831"} {:id "def/fn-decl", :kind "def", :line 48, :end-line 48, :hash "204735580"} {:id "def/local-lambda", :kind "def", :line 49, :end-line 49, :hash "907689216"} {:id "def/local-fn-type", :kind "def", :line 50, :end-line 50, :hash "-2079120114"} {:id "def/union-variant", :kind "def", :line 51, :end-line 51, :hash "-786236575"} {:id "def/runtime-union-variant-field-jvm-types", :kind "def", :line 52, :end-line 52, :hash "1485409556"} {:id "defn/join-branch-types", :kind "defn", :line 53, :end-line 55, :hash "-460332746"} {:id "defn-/infer-literal-type", :kind "defn-", :line 57, :end-line 68, :hash "-2072052525"} {:id "defn-/infer-record-get-type", :kind "defn-", :line 70, :end-line 74, :hash "-1320321895"} {:id "defn-/local-lambda-return-type", :kind "defn-", :line 76, :end-line 79, :hash "-377855316"} {:id "defn-/named-call-return-type", :kind "defn-", :line 81, :end-line 84, :hash "737420465"} {:id "defn-/infer-call-type", :kind "defn-", :line 86, :end-line 97, :hash "910305658"} {:id "defn-/infer-if-type", :kind "defn-", :line 99, :end-line 106, :hash "-2112938377"} {:id "defn-/infer-seq-type", :kind "defn-", :line 108, :end-line 120, :hash "604684926"} {:id "defn-/infer-let-type", :kind "defn-", :line 122, :end-line 135, :hash "327096300"} {:id "defn-/infer-lambda", :kind "defn-", :line 137, :end-line 142, :hash "-1117964483"} {:id "defn-/infer-loop", :kind "defn-", :line 144, :end-line 156, :hash "-46270947"} {:id "defn-/infer-recur", :kind "defn-", :line 158, :end-line 175, :hash "-1368982370"} {:id "defn-/infer-try", :kind "defn-", :line 177, :end-line 191, :hash "641639781"} {:id "defn-/infer-raise", :kind "defn-", :line 193, :end-line 196, :hash "465355386"} {:id "def/infer-type-handlers", :kind "def", :line 198, :end-line 289, :hash "212336352"} {:id "defn/infer-type", :kind "defn", :line 291, :end-line 297, :hash "600232947"}]}
;; clj-mutate-manifest-end
