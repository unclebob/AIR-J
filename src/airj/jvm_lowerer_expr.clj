(ns airj.jvm-lowerer-expr
  (:require [airj.jvm-cells :as jvm-cells]
            [airj.jvm-closures :as closures]
            [airj.jvm-lowerer-match :as lowerer-match]
            [airj.jvm-lowerer-types :as types]))

(declare lower-expr)
(declare lower-param)

(defn lower-literal
  [expr]
  (cond
    (integer? expr) {:op :jvm-int
                     :value expr
                     :jvm-type :int}
    (string? expr) {:op :jvm-string
                    :value expr
                    :jvm-type "java/lang/String"}
    (true? expr) {:op :jvm-boolean
                  :value true
                  :jvm-type :boolean}
    (false? expr) {:op :jvm-boolean
                   :value false
                   :jvm-type :boolean}
    :else nil))

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
   :class-name (types/union-variant-class-name (:module-name ctx) (:type expr) (:name expr))
   :args (mapv #(lower-expr % ctx) (:args expr))
   :jvm-type (types/nested-class-name (:module-name ctx) (:type expr))})

(defn- lower-record-get
  [expr ctx]
  (let [target-type (types/infer-type (:target expr) ctx)
        target-decl (get (:decls ctx) target-type)]
    {:op :jvm-record-get
     :target (lower-expr (:target expr) ctx)
     :field (:field expr)
     :jvm-type (types/lower-type (types/field-type target-decl (:field expr)) ctx)}))

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

(defn- lower-java-set-field
  [expr ctx]
  {:op :jvm-java-set-field
   :target (lower-expr (:target expr) ctx)
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
                                        :union-variant-class-name types/union-variant-class-name
                                        :lower-literal lower-literal}))
   :seq lower-seq
   :java-new lower-java-new
   :java-call lower-java-call
   :java-static-call lower-java-static-call
   :java-get-field lower-java-get-field
   :java-set-field lower-java-set-field})

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
;; {:version 1, :tested-at "2026-03-12T17:25:16.313861-05:00", :module-hash "-714566653", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-816319332"} {:id "form/1/declare", :kind "declare", :line 7, :end-line 7, :hash "-1588192225"} {:id "form/2/declare", :kind "declare", :line 8, :end-line 8, :hash "-350471521"} {:id "defn/lower-literal", :kind "defn", :line 10, :end-line 25, :hash "1026711281"} {:id "defn-/local-callee-name", :kind "defn-", :line 27, :end-line 30, :hash "291537367"} {:id "defn-/lower-local", :kind "defn-", :line 32, :end-line 48, :hash "-672533136"} {:id "defn-/lower-construct", :kind "defn-", :line 50, :end-line 55, :hash "1995610050"} {:id "defn-/lower-static-call", :kind "defn-", :line 57, :end-line 65, :hash "1240643892"} {:id "defn-/lower-closure-call", :kind "defn-", :line 67, :end-line 76, :hash "2122727009"} {:id "defn-/lower-call", :kind "defn-", :line 78, :end-line 107, :hash "-345196595"} {:id "defn-/lower-variant", :kind "defn-", :line 109, :end-line 115, :hash "-798508506"} {:id "defn-/lower-record-get", :kind "defn-", :line 117, :end-line 124, :hash "-725985551"} {:id "defn-/lower-if", :kind "defn-", :line 126, :end-line 132, :hash "-2017247463"} {:id "defn-/lower-seq", :kind "defn-", :line 134, :end-line 149, :hash "1082177207"} {:id "defn-/lower-let", :kind "defn-", :line 151, :end-line 176, :hash "-64431889"} {:id "defn-/lower-java-static-call", :kind "defn-", :line 178, :end-line 186, :hash "1092412591"} {:id "defn-/lower-java-call", :kind "defn-", :line 188, :end-line 196, :hash "115496493"} {:id "defn-/lower-java-get-field", :kind "defn-", :line 198, :end-line 204, :hash "2032560028"} {:id "defn-/lower-java-set-field", :kind "defn-", :line 206, :end-line 213, :hash "-203168201"} {:id "defn-/lower-java-new", :kind "defn-", :line 215, :end-line 221, :hash "-1767177798"} {:id "defn-/lower-lambda", :kind "defn-", :line 223, :end-line 231, :hash "1375267938"} {:id "defn-/lower-var", :kind "defn-", :line 233, :end-line 240, :hash "550518548"} {:id "defn-/lower-set", :kind "defn-", :line 242, :end-line 251, :hash "-1854533276"} {:id "defn-/lower-loop", :kind "defn-", :line 253, :end-line 275, :hash "-2120471898"} {:id "defn-/lower-recur", :kind "defn-", :line 277, :end-line 281, :hash "-201172198"} {:id "defn-/lower-catch-type", :kind "defn-", :line 283, :end-line 288, :hash "828723469"} {:id "defn-/lower-try", :kind "defn-", :line 290, :end-line 304, :hash "2141795572"} {:id "defn-/lower-raise", :kind "defn-", :line 306, :end-line 314, :hash "517260333"} {:id "def/lower-expr-handlers", :kind "def", :line 316, :end-line 349, :hash "-1296368500"} {:id "defn/lower-expr", :kind "defn", :line 351, :end-line 357, :hash "-1590709867"} {:id "defn/lower-param", :kind "defn", :line 359, :end-line 362, :hash "-480161331"}]}
;; clj-mutate-manifest-end
