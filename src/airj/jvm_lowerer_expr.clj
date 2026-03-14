(ns airj.jvm-lowerer-expr
  (:require [airj.jvm-cells :as jvm-cells]
            [airj.jvm-closures :as closures]
            [airj.jvm-lowerer-match :as lowerer-match]
            [airj.jvm-lowerer-types :as types]))

(declare lower-expr)
(declare lower-param)

(defn- lower-floating-literal
  [expr]
  (cond
    (instance? Float expr) {:op :jvm-float
                            :value expr
                            :jvm-type :float}
    (instance? Double expr) {:op :jvm-double
                             :value expr
                             :jvm-type :double}
    :else nil))

(defn lower-literal
  [expr]
  (or (when (integer? expr)
        {:op :jvm-int
         :value expr
         :jvm-type :int})
      (lower-floating-literal expr)
      (when (string? expr)
        {:op :jvm-string
         :value expr
         :jvm-type "java/lang/String"})
      (when (or (true? expr) (false? expr))
        {:op :jvm-boolean
         :value (boolean expr)
         :jvm-type :boolean})))

(defn- local-callee-name
  [callee]
  (when (= :local (:op callee))
    (:name callee)))

(defn- lower-local
  [expr ctx]
  (if-let [type-expr (types/mutable-local-type ctx (:name expr))]
    {:op :jvm-cell-get
     :name (:name expr)
     :cell-jvm-type (jvm-cells/cell-jvm-type (types/lower-type type-expr ctx))
     :jvm-type (types/lower-type type-expr ctx)}
    (if (and (types/fn-decl ctx (:name expr))
             (not (contains? (:locals ctx) (:name expr))))
      (closures/lower-fn-value (types/fn-decl ctx (:name expr))
                               ctx
                               {:fail! types/fail!
                                :lower-type types/lower-type
                                :lower-param lower-param})
      {:op :jvm-local
       :name (:name expr)
       :jvm-type (types/lower-type (types/local-type ctx (:name expr)) ctx)})))

(defn- lower-construct
  [expr ctx]
  {:op :jvm-construct
   :class-name (types/record-class-name ctx (:type expr))
   :parameter-types (types/runtime-field-jvm-types ctx (:type expr))
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (types/record-class-name ctx (:type expr))})

(defn- lower-static-call
  [target args ctx]
  {:op :jvm-invoke-static
   :owner (types/internal-name (:owner-module target))
   :name (:name target)
   :parameter-types (mapv #(types/lower-type (:type %) ctx) (:params target))
   :return-type (types/lower-type (:return-type target) ctx)
   :args (mapv #(lower-expr % ctx) args)
   :jvm-type (types/lower-type (:return-type target) ctx)})

(defn- lower-closure-call
  [callee args callee-type ctx]
  {:op :jvm-closure-call
   :callee (lower-expr callee ctx)
   :interface-name (types/lower-type callee-type ctx)
   :method-name "apply"
   :parameter-types (mapv #(types/lower-type % ctx) (:params callee-type))
   :return-type (types/lower-type (:return-type callee-type) ctx)
   :args (mapv #(lower-expr % ctx) args)
   :jvm-type (types/lower-type (:return-type callee-type) ctx)})

(defn- lower-call
  [expr ctx]
  (let [callee (:callee expr)
        local-name (local-callee-name callee)]
    (cond
      (= :lambda (:op callee))
      (closures/lower-inline-lambda-call callee
                                         (:args expr)
                                         ctx
                                         types/bind-local
                                         lower-expr
                                         types/lower-type)

      (types/local-lambda ctx local-name)
      (closures/lower-inline-lambda-call (types/local-lambda ctx local-name)
                                         (:args expr)
                                         ctx
                                         types/bind-local
                                         lower-expr
                                         types/lower-type)

      (types/fn-decl ctx local-name)
      (lower-static-call (types/fn-decl ctx local-name) (:args expr) ctx)

      (= :fn-type (:op (types/infer-type callee ctx)))
      (lower-closure-call callee (:args expr) (types/infer-type callee ctx) ctx)

      :else
      (types/fail! "Unknown lowered call target."
                   {:callee callee}))))

(defn- lower-variant
  [expr ctx]
  (types/union-variant ctx (:type expr) (:name expr))
  {:op :jvm-variant
   :class-name (types/runtime-union-variant-class-name ctx
                                                       (:type expr)
                                                       (:name expr))
   :parameter-types (types/runtime-union-variant-field-jvm-types ctx (:type expr) (:name expr))
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (types/declared-type-name (:type expr) ctx)})

(defn- lower-record-get
  [expr ctx]
  (let [target-type (types/infer-type (:target expr) ctx)
        target-decl (types/decl-for-type ctx target-type)]
    {:op :jvm-record-get
     :target (lower-expr (:target expr) ctx)
     :field (:field expr)
     :field-jvm-type (types/runtime-field-jvm-type ctx target-type (:field expr))
     :jvm-type (types/lower-type (types/field-type target-decl (:field expr) target-type) ctx)}))

(defn- lower-if
  [expr ctx]
  {:op :jvm-if
   :test (lower-expr (:test expr) ctx)
   :then (lower-expr (:then expr) ctx)
   :else (lower-expr (:else expr) ctx)
   :jvm-type (types/lower-expr-type (types/infer-type expr ctx) ctx)})

(defn- lower-seq
  [expr ctx]
  (let [[exprs _]
        (reduce (fn [[acc current-ctx] subexpr]
                  (let [lowered (lower-expr subexpr current-ctx)
                        next-ctx (if (= :var (:op subexpr))
                                   (types/bind-mutable-local current-ctx
                                                             (:name subexpr)
                                                             (:type subexpr))
                                   current-ctx)]
                    [(conj acc lowered) next-ctx]))
                [[] ctx]
                (:exprs expr))]
    {:op :jvm-seq
     :exprs exprs
     :jvm-type (types/lower-expr-type (types/infer-type expr ctx) ctx)}))

(defn- lower-let
  [expr ctx]
  (let [[bindings body-ctx]
        (reduce (fn [[acc current-ctx] binding]
                  (let [binding-type (types/infer-type (:expr binding) current-ctx)
                        inline-lambda? (= :lambda (:op (:expr binding)))
                        next-ctx (cond-> (types/bind-local current-ctx
                                                           (:name binding)
                                                           binding-type)
                                   inline-lambda?
                                   (types/bind-lambda (:name binding) (:expr binding)))]
                    (if inline-lambda?
                      [acc next-ctx]
                      [(conj acc {:name (:name binding)
                                  :expr (lower-expr (:expr binding) current-ctx)})
                       next-ctx])))
                [[] ctx]
                (:bindings expr))
        lowered-body (lower-expr (:body expr) body-ctx)
        jvm-type (types/lower-expr-type (types/infer-type expr ctx) ctx)]
    (if (empty? bindings)
      lowered-body
      {:op :jvm-let
       :bindings bindings
       :body lowered-body
       :jvm-type jvm-type})))

(defn- lower-java-static-call
  [expr ctx]
  {:op :jvm-java-static-call
   :class-name (-> (:class-name expr) str (.replace "." "/"))
   :member-id (:member-id expr)
   :parameter-types (mapv #(types/lower-type %) (get-in expr [:signature :params]))
   :return-type (types/lower-type (get-in expr [:signature :return-type]))
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (types/lower-type (get-in expr [:signature :return-type]))})

(defn- lower-json-parse
  [expr ctx]
  {:op :jvm-json-parse
   :arg (lower-expr (:arg expr) ctx)
   :root-class-name (types/declared-type-name 'Interchange ctx)
   :jvm-type (types/lower-type 'Interchange ctx)})

(defn- lower-json-write
  [expr ctx]
  {:op :jvm-json-write
   :arg (lower-expr (:arg expr) ctx)
   :jvm-type "java/lang/String"})

(defn- lower-env-get
  [expr ctx]
  {:op :jvm-env-get
   :arg (lower-expr (:arg expr) ctx)
   :root-class-name (types/declared-type-name 'Option ctx)
   :jvm-type (types/lower-type '(Option String) ctx)})

(defn- lower-process-run
  [expr ctx]
  {:op :jvm-process-run
   :args (mapv #(lower-expr % ctx) (:args expr))
   :root-class-name (types/declared-type-name 'ProcessResult ctx)
   :jvm-type (types/lower-type 'ProcessResult ctx)})

(defn- lower-java-call
  [expr ctx]
  {:op :jvm-java-call
   :target (lower-expr (:target expr) ctx)
   :member-id (:member-id expr)
   :parameter-types (mapv #(types/lower-type %) (get-in expr [:signature :params]))
   :return-type (types/lower-type (get-in expr [:signature :return-type]))
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (types/lower-type (get-in expr [:signature :return-type]))})

(defn- lower-java-get-field
  [expr ctx]
  {:op :jvm-java-get-field
   :target (lower-expr (:target expr) ctx)
   :field-name (:field-name expr)
   :field-type (types/lower-type (:field-type expr))
   :jvm-type (types/lower-type (:field-type expr))})

(defn- lower-java-static-get-field
  [expr _ctx]
  {:op :jvm-java-static-get-field
   :class-name (-> (:class-name expr) str (.replace "." "/"))
   :field-name (:field-name expr)
   :field-type (types/lower-type (:field-type expr))
   :jvm-type (types/lower-type (:field-type expr))})

(defn- lower-java-set-field
  [expr ctx]
  {:op :jvm-java-set-field
   :target (lower-expr (:target expr) ctx)
   :field-name (:field-name expr)
   :field-type (types/lower-type (:field-type expr))
   :expr (lower-expr (:expr expr) ctx)
   :jvm-type :void})

(defn- lower-java-static-set-field
  [expr ctx]
  {:op :jvm-java-static-set-field
   :class-name (-> (:class-name expr) str (.replace "." "/"))
   :field-name (:field-name expr)
   :field-type (types/lower-type (:field-type expr))
   :expr (lower-expr (:expr expr) ctx)
   :jvm-type :void})

(defn- lower-java-new
  [expr ctx]
  {:op :jvm-java-new
   :class-name (-> (:class-name expr) str (.replace "." "/"))
   :parameter-types (mapv #(types/lower-type (types/infer-type % ctx) ctx) (:args expr))
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (-> (:class-name expr) str (.replace "." "/"))})

(defn- lower-primitive-binary
  [op expr ctx jvm-type]
  {:op op
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type jvm-type})

(defn- lower-primitive-unary
  [op expr ctx jvm-type]
  {:op op
   :arg (lower-expr (:arg expr) ctx)
   :jvm-type jvm-type})

(defn- lower-seq-unary
  [op expr ctx]
  (lower-primitive-unary op expr ctx
                         (types/lower-type (types/infer-type (:arg expr) ctx) ctx)))

(defn- lower-seq-element
  [op expr ctx]
  (lower-primitive-unary op expr ctx
                         (types/lower-type (types/infer-type expr ctx) ctx)))

(defn- lower-seq-get
  [expr ctx]
  {:op :jvm-seq-get
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (types/lower-type (types/infer-type expr ctx) ctx)})

(defn- lower-map-empty
  [expr ctx]
  {:op :jvm-map-empty
   :value-jvm-type (types/lower-type (:value-type expr) ctx)
   :jvm-type "java/util/Map"})

(defn- lower-map-set
  [expr ctx]
  {:op :jvm-map-set
   :args (mapv #(lower-expr % ctx) (:args expr))
   :value-jvm-type (types/lower-type (types/map-value-type
                                      (types/infer-type (first (:args expr)) ctx))
                                     ctx)
   :jvm-type "java/util/Map"})

(defn- lower-map-get
  [expr ctx]
  (let [map-type (types/infer-type (first (:args expr)) ctx)
        value-type (types/map-value-type map-type)
        option-type (types/infer-type expr ctx)]
    {:op :jvm-map-get
     :args (mapv #(lower-expr % ctx) (:args expr))
     :none-class-name (types/runtime-union-variant-class-name ctx option-type 'None)
     :some-class-name (types/runtime-union-variant-class-name ctx option-type 'Some)
     :some-parameter-types (types/runtime-union-variant-field-jvm-types ctx option-type 'Some)
     :value-jvm-type (types/lower-type value-type ctx)
     :jvm-type (types/lower-type option-type ctx)}))

(defn- lower-map-contains
  [expr ctx]
  {:op :jvm-map-contains
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type :boolean})

(defn- lower-map-keys
  [expr ctx]
  {:op :jvm-map-keys
   :arg (lower-expr (:arg expr) ctx)
   :jvm-type "java/util/List"})

(defn- lower-lambda
  [expr ctx]
  (closures/lower-lambda expr
                         ctx
                         {:fail! types/fail!
                          :lower-type types/lower-type
                          :lower-expr lower-expr
                          :lower-param lower-param
                          :bind-local types/bind-local}))

(defn- lower-var
  [expr ctx]
  {:op :jvm-var
   :name (:name expr)
   :init (lower-expr (:init expr) ctx)
   :value-jvm-type (types/lower-type (:type expr) ctx)
   :cell-jvm-type (jvm-cells/cell-jvm-type (types/lower-type (:type expr) ctx))
   :jvm-type :void})

(defn- lower-set
  [expr ctx]
  (let [type-expr (or (types/mutable-local-type ctx (:name expr))
                      (types/local-type ctx (:name expr)))]
    {:op :jvm-set
     :name (:name expr)
     :expr (lower-expr (:expr expr) ctx)
     :value-jvm-type (types/lower-type type-expr ctx)
     :cell-jvm-type (jvm-cells/cell-jvm-type (types/lower-type type-expr ctx))
     :jvm-type :void}))

(defn- lower-loop
  [expr ctx]
  (let [{:keys [bindings body-ctx loop-types]}
        (reduce (fn [{:keys [bindings body-ctx loop-types]} binding]
                  (let [binding-type (types/infer-type (:expr binding) body-ctx)
                        lowered-expr (lower-expr (:expr binding) body-ctx)]
                    {:bindings (conj bindings
                                     {:name (:name binding)
                                      :init lowered-expr
                                      :jvm-type (types/lower-type binding-type ctx)})
                     :body-ctx (types/bind-local body-ctx
                                                (:name binding)
                                                binding-type)
                     :loop-types (conj loop-types binding-type)}))
                {:bindings []
                 :body-ctx ctx
                 :loop-types []}
                (:bindings expr))
        loop-ctx (types/with-loop-types body-ctx loop-types)]
    {:op :jvm-loop
     :bindings bindings
     :body (lower-expr (:body expr) loop-ctx)
     :jvm-type (types/lower-expr-type (types/infer-type expr ctx) ctx)}))

(defn- lower-recur
  [expr ctx]
  {:op :jvm-recur
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type :void})

(defn- lower-catch-type
  [type-expr]
  (if (types/java-type? type-expr)
    (types/java-internal-name type-expr)
    (types/fail! "Unsupported JVM catch type."
                 {:type type-expr})))

(defn- lower-try
  [expr ctx]
  {:op :jvm-try
   :body (lower-expr (:body expr) ctx)
   :catches (mapv (fn [catch]
                    {:type (lower-catch-type (:type catch))
                     :name (:name catch)
                     :body (lower-expr (:body catch)
                                       (types/bind-local ctx
                                                         (:name catch)
                                                         (:type catch)))})
                  (:catches expr))
   :finally (when-let [finally-expr (:finally expr)]
              (lower-expr finally-expr ctx))
   :jvm-type (types/lower-expr-type (types/infer-type expr ctx) ctx)})

(defn- lower-raise
  [expr ctx]
  (let [raised-type (types/lower-type (types/infer-type (:expr expr) ctx) ctx)]
    (when (contains? #{:int :boolean :void} raised-type)
      (types/fail! "Unsupported JVM raise type."
                   {:type raised-type})))
  {:op :jvm-raise
   :expr (lower-expr (:expr expr) ctx)
   :jvm-type :void})

(def ^:private lower-expr-handlers
  {:local lower-local
   :let lower-let
   :var lower-var
   :set lower-set
   :loop lower-loop
   :recur lower-recur
   :try lower-try
   :raise lower-raise
   :lambda lower-lambda
   :construct lower-construct
   :variant lower-variant
   :call lower-call
   :int-add (fn [expr ctx] (lower-primitive-binary :jvm-int-add expr ctx :int))
   :int-sub (fn [expr ctx] (lower-primitive-binary :jvm-int-sub expr ctx :int))
   :int-mul (fn [expr ctx] (lower-primitive-binary :jvm-int-mul expr ctx :int))
   :int-div (fn [expr ctx] (lower-primitive-binary :jvm-int-div expr ctx :int))
   :int-mod (fn [expr ctx] (lower-primitive-binary :jvm-int-mod expr ctx :int))
   :int-eq (fn [expr ctx] (lower-primitive-binary :jvm-int-eq expr ctx :boolean))
   :int-lt (fn [expr ctx] (lower-primitive-binary :jvm-int-lt expr ctx :boolean))
   :int-le (fn [expr ctx] (lower-primitive-binary :jvm-int-le expr ctx :boolean))
   :int-gt (fn [expr ctx] (lower-primitive-binary :jvm-int-gt expr ctx :boolean))
   :int-ge (fn [expr ctx] (lower-primitive-binary :jvm-int-ge expr ctx :boolean))
   :float-add (fn [expr ctx] (lower-primitive-binary :jvm-float-add expr ctx :float))
   :float-sub (fn [expr ctx] (lower-primitive-binary :jvm-float-sub expr ctx :float))
   :float-mul (fn [expr ctx] (lower-primitive-binary :jvm-float-mul expr ctx :float))
   :float-div (fn [expr ctx] (lower-primitive-binary :jvm-float-div expr ctx :float))
   :float-eq (fn [expr ctx] (lower-primitive-binary :jvm-float-eq expr ctx :boolean))
   :float-lt (fn [expr ctx] (lower-primitive-binary :jvm-float-lt expr ctx :boolean))
   :float-le (fn [expr ctx] (lower-primitive-binary :jvm-float-le expr ctx :boolean))
   :float-gt (fn [expr ctx] (lower-primitive-binary :jvm-float-gt expr ctx :boolean))
   :float-ge (fn [expr ctx] (lower-primitive-binary :jvm-float-ge expr ctx :boolean))
   :double-add (fn [expr ctx] (lower-primitive-binary :jvm-double-add expr ctx :double))
   :double-sub (fn [expr ctx] (lower-primitive-binary :jvm-double-sub expr ctx :double))
   :double-mul (fn [expr ctx] (lower-primitive-binary :jvm-double-mul expr ctx :double))
   :double-div (fn [expr ctx] (lower-primitive-binary :jvm-double-div expr ctx :double))
   :double-eq (fn [expr ctx] (lower-primitive-binary :jvm-double-eq expr ctx :boolean))
   :double-lt (fn [expr ctx] (lower-primitive-binary :jvm-double-lt expr ctx :boolean))
   :double-le (fn [expr ctx] (lower-primitive-binary :jvm-double-le expr ctx :boolean))
   :double-gt (fn [expr ctx] (lower-primitive-binary :jvm-double-gt expr ctx :boolean))
   :double-ge (fn [expr ctx] (lower-primitive-binary :jvm-double-ge expr ctx :boolean))
   :bool-eq (fn [expr ctx] (lower-primitive-binary :jvm-bool-eq expr ctx :boolean))
   :int-ne (fn [expr ctx] (lower-primitive-binary :jvm-int-ne expr ctx :boolean))
   :string-eq (fn [expr ctx] (lower-primitive-binary :jvm-string-eq expr ctx :boolean))
   :string-concat (fn [expr ctx] (lower-primitive-binary :jvm-string-concat expr ctx "java/lang/String"))
   :string-split-on (fn [expr ctx] (lower-primitive-binary :jvm-string-split-on expr ctx "java/util/List"))
   :string-char-at (fn [expr ctx] (lower-primitive-binary :jvm-string-char-at expr ctx "java/lang/String"))
   :string-substring (fn [expr ctx]
                       {:op :jvm-string-substring
                        :args (mapv #(lower-expr % ctx) (:args expr))
                        :jvm-type "java/lang/String"})
   :int->string (fn [expr ctx] (lower-primitive-unary :jvm-int->string expr ctx "java/lang/String"))
   :int->float (fn [expr ctx] (lower-primitive-unary :jvm-int->float expr ctx :float))
   :int->double (fn [expr ctx] (lower-primitive-unary :jvm-int->double expr ctx :double))
   :float->double (fn [expr ctx] (lower-primitive-unary :jvm-float->double expr ctx :double))
   :double->float (fn [expr ctx] (lower-primitive-unary :jvm-double->float expr ctx :float))
   :json-parse lower-json-parse
   :json-write lower-json-write
   :env-get lower-env-get
   :process-run lower-process-run
   :string->int (fn [expr ctx] (lower-primitive-unary :jvm-string->int expr ctx :int))
   :string-length (fn [expr ctx] (lower-primitive-unary :jvm-string-length expr ctx :int))
   :string-trim (fn [expr ctx] (lower-primitive-unary :jvm-string-trim expr ctx "java/lang/String"))
   :string-empty? (fn [expr ctx] (lower-primitive-unary :jvm-string-empty expr ctx :boolean))
   :seq-empty? (fn [expr ctx] (lower-seq-unary :jvm-seq-empty expr ctx))
   :seq-length (fn [expr ctx] (lower-seq-unary :jvm-seq-length expr ctx))
   :seq-first (fn [expr ctx] (lower-seq-element :jvm-seq-first expr ctx))
   :seq-rest (fn [expr ctx] (lower-seq-unary :jvm-seq-rest expr ctx))
   :seq-concat (fn [expr ctx] (lower-primitive-binary :jvm-seq-concat expr ctx "java/util/List"))
   :seq-get lower-seq-get
   :map-empty lower-map-empty
   :map-set lower-map-set
   :map-get lower-map-get
   :map-contains? lower-map-contains
   :map-keys lower-map-keys
   :io-read-line (fn [_expr _ctx] {:op :jvm-io-read-line
                                   :jvm-type "java/lang/String"})
   :io-print (fn [expr ctx] {:op :jvm-io-print
                             :arg (lower-expr (:arg expr) ctx)
                             :jvm-type :void})
   :bool-not (fn [expr ctx] (lower-primitive-unary :jvm-bool-not expr ctx :boolean))
   :bool-and (fn [expr ctx] (lower-primitive-binary :jvm-bool-and expr ctx :boolean))
   :bool-or (fn [expr ctx] (lower-primitive-binary :jvm-bool-or expr ctx :boolean))
   :io-println (fn [expr ctx] {:op :jvm-io-println
                               :arg (lower-expr (:arg expr) ctx)
                               :jvm-type :void})
   :record-get lower-record-get
   :if lower-if
   :match (fn [expr ctx]
            (lowerer-match/lower-match expr
                                       ctx
                                       {:fail! types/fail!
                                        :lower-expr lower-expr
                                        :infer-type types/infer-type
                                        :lower-type types/lower-type
                                        :lower-expr-type types/lower-expr-type
                                        :bind-local types/bind-local
                                        :join-branch-types types/join-branch-types
                                        :union-variant types/union-variant
                                        :runtime-union-variant-field-jvm-types types/runtime-union-variant-field-jvm-types
                                        :runtime-union-variant-class-name types/runtime-union-variant-class-name
                                        :union-variant-class-name types/union-variant-class-name
                                        :lower-literal lower-literal}))
   :seq lower-seq
   :java-new lower-java-new
   :java-call lower-java-call
   :java-static-call lower-java-static-call
   :java-get-field lower-java-get-field
   :java-set-field lower-java-set-field
   :java-static-get-field lower-java-static-get-field
   :java-static-set-field lower-java-static-set-field})

(defn lower-expr
  [expr ctx]
  (or (lower-literal expr)
      (if-let [handler (get lower-expr-handlers (:op expr))]
        (handler expr ctx)
        (types/fail! "Unsupported JVM expression."
                     {:expr expr}))))

(defn lower-param
  [param ctx]
  {:name (:name param)
   :jvm-type (types/lower-type (:type param) ctx)})

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T14:37:55.905557-05:00", :module-hash "-983037941", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-816319332"} {:id "form/1/declare", :kind "declare", :line 7, :end-line 7, :hash "-1588192225"} {:id "form/2/declare", :kind "declare", :line 8, :end-line 8, :hash "-350471521"} {:id "defn-/lower-floating-literal", :kind "defn-", :line 10, :end-line 19, :hash "193702899"} {:id "defn/lower-literal", :kind "defn", :line 21, :end-line 35, :hash "1784475901"} {:id "defn-/local-callee-name", :kind "defn-", :line 37, :end-line 40, :hash "291537367"} {:id "defn-/lower-local", :kind "defn-", :line 42, :end-line 58, :hash "-672533136"} {:id "defn-/lower-construct", :kind "defn-", :line 60, :end-line 66, :hash "756421074"} {:id "defn-/lower-static-call", :kind "defn-", :line 68, :end-line 76, :hash "1812359444"} {:id "defn-/lower-closure-call", :kind "defn-", :line 78, :end-line 87, :hash "1094309029"} {:id "defn-/lower-call", :kind "defn-", :line 89, :end-line 118, :hash "-345196595"} {:id "defn-/lower-variant", :kind "defn-", :line 120, :end-line 129, :hash "-1867821314"} {:id "defn-/lower-record-get", :kind "defn-", :line 131, :end-line 139, :hash "-1351678206"} {:id "defn-/lower-if", :kind "defn-", :line 141, :end-line 147, :hash "-2017247463"} {:id "defn-/lower-seq", :kind "defn-", :line 149, :end-line 164, :hash "1082177207"} {:id "defn-/lower-let", :kind "defn-", :line 166, :end-line 191, :hash "-64431889"} {:id "defn-/lower-java-static-call", :kind "defn-", :line 193, :end-line 201, :hash "-1681813544"} {:id "defn-/lower-json-parse", :kind "defn-", :line 203, :end-line 208, :hash "1797385827"} {:id "defn-/lower-json-write", :kind "defn-", :line 210, :end-line 214, :hash "-446845756"} {:id "defn-/lower-env-get", :kind "defn-", :line 216, :end-line 221, :hash "-1306325642"} {:id "defn-/lower-process-run", :kind "defn-", :line 223, :end-line 228, :hash "-775234508"} {:id "defn-/lower-java-call", :kind "defn-", :line 230, :end-line 238, :hash "-1452735591"} {:id "defn-/lower-java-get-field", :kind "defn-", :line 240, :end-line 246, :hash "2032560028"} {:id "defn-/lower-java-static-get-field", :kind "defn-", :line 248, :end-line 254, :hash "-1037422566"} {:id "defn-/lower-java-set-field", :kind "defn-", :line 256, :end-line 263, :hash "-203168201"} {:id "defn-/lower-java-static-set-field", :kind "defn-", :line 265, :end-line 272, :hash "824180224"} {:id "defn-/lower-java-new", :kind "defn-", :line 274, :end-line 280, :hash "-1502399898"} {:id "defn-/lower-primitive-binary", :kind "defn-", :line 282, :end-line 286, :hash "706056105"} {:id "defn-/lower-primitive-unary", :kind "defn-", :line 288, :end-line 292, :hash "-597151269"} {:id "defn-/lower-seq-unary", :kind "defn-", :line 294, :end-line 297, :hash "104266702"} {:id "defn-/lower-seq-element", :kind "defn-", :line 299, :end-line 302, :hash "298832021"} {:id "defn-/lower-seq-get", :kind "defn-", :line 304, :end-line 308, :hash "-1493025967"} {:id "defn-/lower-map-empty", :kind "defn-", :line 310, :end-line 314, :hash "-600183760"} {:id "defn-/lower-map-set", :kind "defn-", :line 316, :end-line 323, :hash "385767161"} {:id "defn-/lower-map-get", :kind "defn-", :line 325, :end-line 336, :hash "867778718"} {:id "defn-/lower-map-contains", :kind "defn-", :line 338, :end-line 342, :hash "1638229561"} {:id "defn-/lower-map-keys", :kind "defn-", :line 344, :end-line 348, :hash "-1109401239"} {:id "defn-/lower-lambda", :kind "defn-", :line 350, :end-line 358, :hash "1375267938"} {:id "defn-/lower-var", :kind "defn-", :line 360, :end-line 367, :hash "550518548"} {:id "defn-/lower-set", :kind "defn-", :line 369, :end-line 378, :hash "-1854533276"} {:id "defn-/lower-loop", :kind "defn-", :line 380, :end-line 402, :hash "-2120471898"} {:id "defn-/lower-recur", :kind "defn-", :line 404, :end-line 408, :hash "-324719607"} {:id "defn-/lower-catch-type", :kind "defn-", :line 410, :end-line 415, :hash "828723469"} {:id "defn-/lower-try", :kind "defn-", :line 417, :end-line 431, :hash "2141795572"} {:id "defn-/lower-raise", :kind "defn-", :line 433, :end-line 441, :hash "517260333"} {:id "def/lower-expr-handlers", :kind "def", :line 443, :end-line 553, :hash "283053491"} {:id "defn/lower-expr", :kind "defn", :line 555, :end-line 561, :hash "-1590709867"} {:id "defn/lower-param", :kind "defn", :line 563, :end-line 566, :hash "-480161331"}]}
;; clj-mutate-manifest-end
