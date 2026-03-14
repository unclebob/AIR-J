(ns airj.type-checker
  (:require [airj.imported-interfaces :as imported-interfaces]
            [airj.java-types :as java-types]
            [airj.patterns :as patterns]))

(declare infer-expr)
(declare infer-expr-type)

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :type-check))))

(defn- decl-map
  [module]
  (merge
   (into {}
         (keep (fn [[symbol {:keys [decl]}]]
                 (when (contains? #{:data :enum :union} (:op decl))
                   [symbol decl])))
         (imported-interfaces/imported-decls module))
   (into {} (map (juxt :name identity) (:decls module)))))

(defn- fn-type
  [params return-type effects]
  {:op :fn-type
   :params (vec params)
   :return-type return-type
   :effects (vec effects)})

(defn- bottom-type
  [kind]
  {:op :bottom-type
   :kind kind})

(defn- bottom-type?
  [type-expr]
  (= :bottom-type (:op type-expr)))

(defn- result
  [type-expr ctx]
  {:type type-expr
   :ctx ctx})

(defn- local-type
  [ctx name]
  (get-in ctx [:locals name]))

(defn- mutable-local?
  [ctx name]
  (contains? (:mutable ctx) name))

(defn- bind-local
  [ctx name type-expr]
  (assoc-in ctx [:locals name] type-expr))

(defn- bind-mutable
  [ctx name type-expr]
  (-> ctx
      (bind-local name type-expr)
      (update :mutable conj name)))

(defn- with-loop-types
  [ctx loop-types]
  (assoc ctx :loop-types (vec loop-types)))

(defn- without-loop-types
  [ctx]
  (dissoc ctx :loop-types))

(defn- fn-decl-type
  [decl]
  (fn-type (map :type (:params decl))
           (:return-type decl)
           (:effects decl)))

(defn- module-fn-env
  [module decls]
  (merge
   (into {}
         (map (fn [decl]
                [(:name decl) (fn-decl-type decl)])
              (filter #(= :fn (:op %)) (vals decls))))
   (into {}
         (keep (fn [[symbol {:keys [decl]}]]
                 (when (= :fn (:op decl))
                   [symbol (fn-decl-type decl)])))
         (imported-interfaces/imported-decls module))))

(defn- make-ctx
  ([locals]
   (make-ctx locals {}))
  ([locals {:keys [available-modules imported-decls]}]
   {:locals locals
    :mutable #{}
    :available-modules (or available-modules #{})
    :imported-decls (or imported-decls {})}))

(defn- literal-type
  [expr]
  (or (when (or (true? expr) (false? expr))
        'Bool)
      (when (integer? expr)
        'Int)
      (when (instance? Float expr)
        'Float)
      (when (instance? Double expr)
        'Double)
      (when (string? expr)
        'String)))

(defn- compatible-types?
  [expected actual]
  (or (= expected actual)
      (bottom-type? expected)
      (bottom-type? actual)))

(defn- ensure-type=
  [expected actual context]
  (when-not (compatible-types? expected actual)
    (fail! "Type mismatch."
           (assoc context :expected expected :actual actual))))

(defn- ensure-arity=
  [expected actual context]
  (when-not (= expected actual)
    (fail! "Arity mismatch."
           (assoc context :expected expected :actual actual))))

(defn- join-types
  [left right context]
  (ensure-type= left right context)
  (cond
    (bottom-type? left) right
    (bottom-type? right) left
    :else left))

(defn- enum-pattern?
  [pattern target-type decls]
  (patterns/enum-pattern? pattern target-type decls))

(defn- bind-literal-pattern
  [ctx pattern target-type]
  (ensure-type= target-type
                (literal-type (:literal pattern))
                {:pattern pattern
                 :type target-type})
  ctx)

(defn- bind-binder-pattern
  [ctx pattern target-type decls]
  (if (enum-pattern? pattern target-type decls)
    ctx
    (bind-local ctx (:name pattern) target-type)))

(defn- bind-pattern
  [ctx pattern target-type decls]
  (patterns/bind-pattern ctx
                         pattern
                         target-type
                         decls
                         {:bind-binder bind-binder-pattern
                          :bind-literal bind-literal-pattern
                          :fail! fail!}))

(defn- infer-local
  [expr ctx]
  (result
   (or (local-type ctx (:name expr))
       (fail! "Unknown local type."
              {:name (:name expr)}))
   ctx))

(defn- infer-record-get
  [expr ctx decls]
  (let [target-type (infer-expr-type (:target expr) ctx decls)
        target-decl (patterns/decl-for-type decls target-type)
        result-type (and target-decl
                         (patterns/field-type target-decl (:field expr) target-type))]
    (when-not target-decl
      (fail! "Expected record type."
             {:type target-type}))
    (result (or result-type
                (fail! "Unknown field."
                       {:type target-type
                        :field (:field expr)}))
            ctx)))

(defn- imported-runtime-available?
  [ctx _decls type-expr]
  (let [type-name (patterns/declared-type-name type-expr)]
    (if-let [module (get-in ctx [:imported-decls type-name :module])]
      (contains? (:available-modules ctx) module)
      true)))

(defn- ensure-constructible-type!
  [ctx decls type-expr]
  (when-not (imported-runtime-available? ctx decls type-expr)
    (fail! "Unknown constructed type."
           {:type type-expr})))

(defn- infer-construct
  [expr ctx decls]
  (let [decl (patterns/decl-for-type decls (:type expr))]
    (when-not decl
      (fail! "Unknown constructed type."
             {:type (:type expr)}))
    (ensure-constructible-type! ctx decls (:type expr))
    (ensure-arity= (count (:fields decl))
                   (count (:args expr))
                   {:type (:type expr)})
    (doseq [[field arg] (map vector (:fields decl) (:args expr))]
      (ensure-type= (patterns/field-type decl (:name field) (:type expr))
                    (infer-expr-type arg ctx decls)
                    {:type (:type expr)
                     :field (:name field)}))
    (result (:type expr) ctx)))

(defn- infer-variant
  [expr ctx decls]
  (let [decl (patterns/decl-for-type decls (:type expr))]
    (when-not decl
      (fail! "Unknown constructed type."
             {:type (:type expr)}))
    (ensure-constructible-type! ctx decls (:type expr))
    (when-not (= :union (:op decl))
      (fail! "Expected union type."
             {:type (:type expr)}))
    (let [variant (patterns/union-variant decl (:name expr) (:type expr))]
      (when-not variant
        (fail! "Unknown variant."
               {:type (:type expr)
                :variant (:name expr)}))
      (ensure-arity= (count (:fields variant))
                     (count (:args expr))
                     {:type (:type expr)
                      :variant (:name expr)})
      (doseq [[field arg] (map vector (:fields variant) (:args expr))]
        (ensure-type= (:type field)
                      (infer-expr-type arg ctx decls)
                      {:type (:type expr)
                       :variant (:name expr)
                       :field (:name field)}))
      (result (:type expr) ctx))))

(defn- infer-if
  [expr ctx decls]
  (let [test-type (infer-expr-type (:test expr) ctx decls)
        then-type (infer-expr-type (:then expr) ctx decls)
        else-type (infer-expr-type (:else expr) ctx decls)]
    (when-not (= 'Bool test-type)
      (fail! "Expected Bool."
             {:actual test-type}))
    (result (join-types then-type else-type {:op :if}) ctx)))

(defn- infer-match
  [expr ctx decls]
  (let [target-type (infer-expr-type (:target expr) ctx decls)
        case-types (mapv (fn [case]
                           (let [case-ctx (bind-pattern ctx
                                                        (:pattern case)
                                                        target-type
                                                        decls)]
                             (infer-expr-type (:body case) case-ctx decls)))
                         (:cases expr))]
    (result (reduce (fn [current case-type]
                      (join-types current case-type {:expr expr}))
                    (first case-types)
                    (rest case-types))
            ctx)))

(defn- infer-call
  [expr ctx decls]
  (let [callee-type (infer-expr-type (:callee expr) ctx decls)]
    (when-not (= :fn-type (:op callee-type))
      (fail! "Expected function type."
             {:actual callee-type}))
    (ensure-arity= (count (:params callee-type))
                   (count (:args expr))
                   {:callee (:callee expr)})
    (doseq [[expected-type arg] (map vector (:params callee-type) (:args expr))]
      (ensure-type= expected-type
                    (infer-expr-type arg ctx decls)
                    {:callee (:callee expr)
                     :arg arg}))
    (result (:return-type callee-type) ctx)))

(defn- infer-lambda
  [expr ctx decls]
  (let [lambda-ctx (reduce (fn [acc param]
                             (bind-local acc (:name param) (:type param)))
                           ctx
                           (:params expr))
        body-type (infer-expr-type (:body expr) lambda-ctx decls)]
    (ensure-type= (:return-type expr)
                  body-type
                  {:lambda expr})
    (result (fn-type (map :type (:params expr))
                     (:return-type expr)
                     (:effects expr))
            ctx)))

(defn- infer-binding
  [ctx binding decls]
  (let [{:keys [type ctx]} (infer-expr (:expr binding) ctx decls)]
    {:type type
     :ctx (bind-local ctx (:name binding) type)}))

(defn- infer-let
  [expr ctx decls]
  (let [binding-ctx (reduce (fn [acc binding]
                              (:ctx (infer-binding acc binding decls)))
                            ctx
                            (:bindings expr))]
    (result (infer-expr-type (:body expr) binding-ctx decls)
            ctx)))

(defn- infer-seq
  [expr ctx decls]
  (reduce (fn [_current expr-part]
            (infer-expr expr-part (:ctx _current) decls))
          (result 'Unit ctx)
          (:exprs expr)))

(defn- infer-var
  [expr ctx decls]
  (ensure-type= (:type expr)
                (infer-expr-type (:init expr) ctx decls)
                {:name (:name expr)})
  (result (:type expr)
          (bind-mutable ctx (:name expr) (:type expr))))

(defn- infer-set
  [expr ctx decls]
  (when-not (mutable-local? ctx (:name expr))
    (fail! "Expected mutable binding."
           {:name (:name expr)}))
  (ensure-type= (local-type ctx (:name expr))
                (infer-expr-type (:expr expr) ctx decls)
                {:name (:name expr)})
  (result 'Unit ctx))

(defn- infer-loop
  [expr ctx decls]
  (let [binding-results (reduce (fn [acc binding]
                                  (let [binding-type (infer-expr-type (:expr binding)
                                                                      (:ctx acc)
                                                                      decls)]
                                    {:ctx (bind-local (:ctx acc)
                                                      (:name binding)
                                                      binding-type)
                                     :types (conj (:types acc) binding-type)}))
                                {:ctx ctx
                                 :types []}
                                (:bindings expr))
        loop-ctx (with-loop-types (:ctx binding-results) (:types binding-results))
        body-type (infer-expr-type (:body expr) loop-ctx decls)]
    (result body-type ctx)))

(defn- infer-recur
  [expr ctx decls]
  (let [loop-types (:loop-types ctx)]
    (when-not loop-types
      (fail! "Recur used outside loop."
             {:expr expr}))
    (ensure-arity= (count loop-types)
                   (count (:args expr))
                   {:expr expr})
    (doseq [[expected-type arg] (map vector loop-types (:args expr))]
      (ensure-type= expected-type
                    (infer-expr-type arg (without-loop-types ctx) decls)
                    {:expr expr
                     :arg arg}))
    (result (bottom-type :recur) ctx)))

(defn- infer-try
  [expr ctx decls]
  (let [body-type (infer-expr-type (:body expr) ctx decls)
        catch-types (mapv (fn [catch]
                            (let [catch-ctx (bind-local ctx (:name catch) (:type catch))]
                              (infer-expr-type (:body catch) catch-ctx decls)))
                          (:catches expr))]
    (doseq [catch-type catch-types]
      (ensure-type= body-type catch-type {:expr expr}))
    (when-let [finally-expr (:finally expr)]
      (infer-expr-type finally-expr ctx decls))
    (result (reduce (fn [current catch-type]
                      (join-types current catch-type {:expr expr}))
                    body-type
                    catch-types)
            ctx)))

(defn- infer-raise
  [expr ctx decls]
  (infer-expr-type (:expr expr) ctx decls)
  (result (bottom-type :raise) ctx))

(defn- infer-primitive-unary
  [expr ctx decls expected-type return-type]
  (ensure-type= expected-type
                (infer-expr-type (:arg expr) ctx decls)
                {:expr expr
                 :arg (:arg expr)})
  (result return-type ctx))

(defn- infer-primitive-binary
  [expr ctx decls expected-type return-type]
  (ensure-arity= 2
                 (count (:args expr))
                 {:expr expr})
  (doseq [arg (:args expr)]
    (ensure-type= expected-type
                  (infer-expr-type arg ctx decls)
                  {:expr expr
                 :arg arg}))
  (result return-type ctx))

(defn- seq-element-type
  [type-expr]
  (cond
    (= 'StringSeq type-expr) 'String
    (and (seq? type-expr)
         (= 'Seq (first type-expr))
         (= 2 (count type-expr))) (second type-expr)
    :else nil))

(defn- map-value-type
  [type-expr]
  (when (and (seq? type-expr)
             (= 'Map (first type-expr))
             (= 3 (count type-expr))
             (= 'String (second type-expr)))
    (nth type-expr 2)))

(defn- ensure-seq-type
  [type-expr data]
  (or (seq-element-type type-expr)
      (fail! "Expected sequence type."
             (assoc data :actual type-expr))))

(defn- ensure-map-type
  [type-expr data]
  (or (map-value-type type-expr)
      (fail! "Expected canonical map type."
             (assoc data :actual type-expr))))

(defn- infer-seq-unary
  [expr ctx decls return-type]
  (ensure-seq-type (infer-expr-type (:arg expr) ctx decls)
                   {:expr expr
                    :arg (:arg expr)})
  (result return-type ctx))

(defn- infer-seq-element
  [expr ctx decls]
  (let [arg-type (infer-expr-type (:arg expr) ctx decls)]
    (result (ensure-seq-type arg-type
                             {:expr expr
                              :arg (:arg expr)})
            ctx)))

(defn- infer-seq-rest
  [expr ctx decls]
  (let [arg-type (infer-expr-type (:arg expr) ctx decls)]
    (result (list 'Seq
                  (ensure-seq-type arg-type
                                   {:expr expr
                                    :arg (:arg expr)}))
            ctx)))

(defn- infer-seq-concat
  [expr ctx decls]
  (ensure-arity= 2
                 (count (:args expr))
                 {:expr expr})
  (let [left-type (infer-expr-type (first (:args expr)) ctx decls)
        right-type (infer-expr-type (second (:args expr)) ctx decls)
        left-element (ensure-seq-type left-type
                                      {:expr expr
                                       :arg (first (:args expr))})
        right-element (ensure-seq-type right-type
                                       {:expr expr
                                        :arg (second (:args expr))})]
    (ensure-type= left-element right-element {:expr expr})
    (result (list 'Seq left-element) ctx)))

(defn- infer-seq-get
  [expr ctx decls]
  (ensure-arity= 2
                 (count (:args expr))
                 {:expr expr})
  (let [seq-type (infer-expr-type (first (:args expr)) ctx decls)]
    (ensure-type= 'Int
                  (infer-expr-type (second (:args expr)) ctx decls)
                  {:expr expr
                   :arg (second (:args expr))})
    (result (ensure-seq-type seq-type
                             {:expr expr
                              :arg (first (:args expr))})
            ctx)))

(defn- infer-map-empty
  [expr ctx]
  (result (list 'Map 'String (:value-type expr)) ctx))

(defn- infer-map-set
  [expr ctx decls]
  (ensure-arity= 3
                 (count (:args expr))
                 {:expr expr})
  (let [map-type (infer-expr-type (first (:args expr)) ctx decls)
        value-type (ensure-map-type map-type
                                    {:expr expr
                                     :arg (first (:args expr))})]
    (ensure-type= 'String
                  (infer-expr-type (second (:args expr)) ctx decls)
                  {:expr expr
                   :arg (second (:args expr))})
    (ensure-type= value-type
                  (infer-expr-type (nth (:args expr) 2) ctx decls)
                  {:expr expr
                   :arg (nth (:args expr) 2)})
    (result map-type ctx)))

(defn- infer-map-get
  [expr ctx decls]
  (ensure-arity= 2
                 (count (:args expr))
                 {:expr expr})
  (let [map-type (infer-expr-type (first (:args expr)) ctx decls)
        value-type (ensure-map-type map-type
                                    {:expr expr
                                     :arg (first (:args expr))})]
    (ensure-type= 'String
                  (infer-expr-type (second (:args expr)) ctx decls)
                  {:expr expr
                   :arg (second (:args expr))})
    (result (list 'Option value-type) ctx)))

(defn- infer-map-contains
  [expr ctx decls]
  (ensure-arity= 2
                 (count (:args expr))
                 {:expr expr})
  (ensure-map-type (infer-expr-type (first (:args expr)) ctx decls)
                   {:expr expr
                    :arg (first (:args expr))})
  (ensure-type= 'String
                (infer-expr-type (second (:args expr)) ctx decls)
                {:expr expr
                 :arg (second (:args expr))})
  (result 'Bool ctx))

(defn- infer-map-keys
  [expr ctx decls]
  (ensure-map-type (infer-expr-type (:arg expr) ctx decls)
                   {:expr expr
                    :arg (:arg expr)})
  (result '(Seq String) ctx))

(defn- infer-java-new
  [expr ctx decls]
  (doseq [arg (:args expr)]
    (infer-expr-type arg ctx decls))
  (result (list 'Java (:class-name expr)) ctx))

(defn- infer-java-call
  [expr ctx decls]
  (infer-expr-type (:target expr) ctx decls)
  (ensure-arity= (count (get-in expr [:signature :params]))
                 (count (:args expr))
                 {:expr expr})
  (doseq [[expected-type arg] (map vector (get-in expr [:signature :params]) (:args expr))]
    (let [actual-type (infer-expr-type arg ctx decls)]
      (when-not (java-types/assignable-type-expr? expected-type actual-type)
        (fail! "Type mismatch."
               {:expr expr
                :arg arg
                :expected expected-type
                :actual actual-type}))))
  (result (get-in expr [:signature :return-type]) ctx))

(defn- infer-java-static-call
  [expr ctx decls]
  (ensure-arity= (count (get-in expr [:signature :params]))
                 (count (:args expr))
                 {:expr expr})
  (doseq [[expected-type arg] (map vector (get-in expr [:signature :params]) (:args expr))]
    (let [actual-type (infer-expr-type arg ctx decls)]
      (when-not (java-types/assignable-type-expr? expected-type actual-type)
        (fail! "Type mismatch."
               {:expr expr
                :arg arg
                :expected expected-type
                :actual actual-type}))))
  (result (get-in expr [:signature :return-type]) ctx))

(defn- infer-java-get-field
  [expr ctx decls]
  (infer-expr-type (:target expr) ctx decls)
  (result (:field-type expr) ctx))

(defn- infer-java-static-get-field
  [expr ctx _decls]
  (result (:field-type expr) ctx))

(defn- ensure-assignable-field-type!
  [expr actual-type]
  (when-not (java-types/assignable-type-expr? (:field-type expr) actual-type)
    (fail! "Type mismatch."
           {:expr expr
            :expected (:field-type expr)
            :actual actual-type})))

(defn- infer-java-set-field
  [expr ctx decls]
  (infer-expr-type (:target expr) ctx decls)
  (let [actual-type (infer-expr-type (:expr expr) ctx decls)]
    (ensure-assignable-field-type! expr actual-type))
  (result 'Unit ctx))

(defn- infer-java-static-set-field
  [expr ctx decls]
  (let [actual-type (infer-expr-type (:expr expr) ctx decls)]
    (ensure-assignable-field-type! expr actual-type))
  (result 'Unit ctx))

(def ^:private expr-inferers
  {:local (fn [expr ctx _decls] (infer-local expr ctx))
  :call infer-call
  :record-get infer-record-get
   :construct infer-construct
   :variant infer-variant
   :if infer-if
   :match infer-match
   :lambda infer-lambda
   :let infer-let
   :seq infer-seq
   :var infer-var
   :set infer-set
   :loop infer-loop
   :recur infer-recur
   :try infer-try
   :raise infer-raise
   :int-add (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Int))
   :int-sub (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Int))
   :int-mul (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Int))
   :int-div (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Int))
   :int-mod (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Int))
   :int-eq (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Bool))
   :int-lt (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Bool))
   :int-le (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Bool))
   :int-gt (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Bool))
   :int-ge (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Bool))
   :float-add (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Float))
   :float-sub (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Float))
   :float-mul (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Float))
   :float-div (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Float))
   :float-eq (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Bool))
   :float-lt (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Bool))
   :float-le (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Bool))
   :float-gt (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Bool))
   :float-ge (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Float 'Bool))
   :double-add (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Double))
   :double-sub (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Double))
   :double-mul (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Double))
   :double-div (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Double))
   :double-eq (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Bool))
   :double-lt (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Bool))
   :double-le (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Bool))
   :double-gt (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Bool))
   :double-ge (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Double 'Bool))
   :bool-eq (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Bool 'Bool))
   :int-ne (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Int 'Bool))
   :string-eq (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'String 'Bool))
   :string-concat (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'String 'String))
   :string-split-on (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'String '(Seq String)))
   :string-char-at (fn [expr ctx decls]
                     (ensure-arity= 2
                                    (count (:args expr))
                                    {:expr expr})
                     (ensure-type= 'String
                                   (infer-expr-type (first (:args expr)) ctx decls)
                                   {:expr expr
                                    :arg (first (:args expr))})
                     (ensure-type= 'Int
                                   (infer-expr-type (second (:args expr)) ctx decls)
                                   {:expr expr
                                    :arg (second (:args expr))})
                     (result 'String ctx))
   :string-substring (fn [expr ctx decls]
                       (ensure-arity= 3
                                      (count (:args expr))
                                      {:expr expr})
                       (ensure-type= 'String
                                     (infer-expr-type (first (:args expr)) ctx decls)
                                     {:expr expr
                                      :arg (first (:args expr))})
                       (doseq [arg (rest (:args expr))]
                         (ensure-type= 'Int
                                       (infer-expr-type arg ctx decls)
                                       {:expr expr
                                        :arg arg}))
                       (result 'String ctx))
   :int->string (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Int 'String))
   :int->float (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Int 'Float))
   :int->double (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Int 'Double))
   :float->double (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Float 'Double))
   :double->float (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Double 'Float))
   :json-parse (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'String 'Interchange))
   :json-write (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Interchange 'String))
   :string->int (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'String 'Int))
   :string-length (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'String 'Int))
   :string-trim (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'String 'String))
   :string-empty? (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'String 'Bool))
   :seq-empty? (fn [expr ctx decls] (infer-seq-unary expr ctx decls 'Bool))
   :seq-length (fn [expr ctx decls] (infer-seq-unary expr ctx decls 'Int))
   :seq-first (fn [expr ctx decls] (infer-seq-element expr ctx decls))
   :seq-rest (fn [expr ctx decls] (infer-seq-rest expr ctx decls))
   :seq-concat (fn [expr ctx decls] (infer-seq-concat expr ctx decls))
   :seq-get (fn [expr ctx decls] (infer-seq-get expr ctx decls))
   :map-empty (fn [expr ctx _decls] (infer-map-empty expr ctx))
   :map-set (fn [expr ctx decls] (infer-map-set expr ctx decls))
   :map-get (fn [expr ctx decls] (infer-map-get expr ctx decls))
   :map-contains? (fn [expr ctx decls] (infer-map-contains expr ctx decls))
   :map-keys (fn [expr ctx decls] (infer-map-keys expr ctx decls))
   :io-read-line (fn [_expr ctx _decls] (result 'String ctx))
   :io-print (fn [expr ctx decls]
               (ensure-type= 'String
                             (infer-expr-type (:arg expr) ctx decls)
                             {:expr expr
                              :arg (:arg expr)})
               (result 'Unit ctx))
   :io-println (fn [expr ctx decls]
                 (ensure-type= 'String
                               (infer-expr-type (:arg expr) ctx decls)
                               {:expr expr
                                :arg (:arg expr)})
                 (result 'Unit ctx))
   :bool-not (fn [expr ctx decls] (infer-primitive-unary expr ctx decls 'Bool 'Bool))
   :bool-and (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Bool 'Bool))
   :bool-or (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Bool 'Bool))
   :java-new infer-java-new
   :java-call infer-java-call
   :java-static-call infer-java-static-call
   :java-get-field infer-java-get-field
   :java-set-field infer-java-set-field
   :java-static-get-field infer-java-static-get-field
   :java-static-set-field infer-java-static-set-field})

(defn infer-expr
  [expr ctx decls]
  (if-let [type-expr (literal-type expr)]
    (result type-expr ctx)
    (if-let [inferer (get expr-inferers (:op expr))]
      (inferer expr ctx decls)
      (fail! "Unsupported type inference."
             {:expr expr}))))

(defn infer-expr-type
  [expr ctx decls]
  (:type (infer-expr expr ctx decls)))

(defn- check-bool-contract
  [contract ctx decls context]
  (when-not (= 'Bool (infer-expr-type contract ctx decls))
    (fail! "Expected Bool."
           context)))

(defn- passthrough-invariant-ctx
  [_decl ctx]
  ctx)

(defn- invariant-ctx
  [decl ctx]
  (let [op (:op decl)
        fallback-ctx (passthrough-invariant-ctx decl ctx)]
    (or (when (= :data op)
          (reduce (fn [acc field]
                    (bind-local acc (:name field) (:type field)))
                  ctx
                  (:fields decl)))
        (when (= :union op)
          (bind-local ctx 'self (:name decl)))
        fallback-ctx)))

(defn- check-invariants
  [decl ctx decls]
  (doseq [invariant (:invariants decl)]
    (check-bool-contract invariant
                         (invariant-ctx decl ctx)
                         decls
                         {:decl (:name decl)
                          :invariant invariant})))

(defn- check-fn-decl
  [decl module decls]
  (let [base-ctx (make-ctx (module-fn-env module decls)
                           {:available-modules (:available-modules module)
                            :imported-decls (imported-interfaces/imported-decls module)})
        ctx (reduce (fn [acc param]
                      (bind-local acc (:name param) (:type param)))
                    base-ctx
                    (:params decl))
        body-type (infer-expr-type (:body decl) ctx decls)]
    (doseq [contract (concat (:requires decl) (:ensures decl))]
      (check-bool-contract contract
                           ctx
                           decls
                           {:fn (:name decl)
                            :contract contract}))
    (ensure-type= (:return-type decl)
                  body-type
                  {:fn (:name decl)})))

(defn check-module
  [module]
  (let [decls (decl-map module)
        imported-decls (imported-interfaces/imported-decls module)
        ctx (make-ctx (module-fn-env module decls)
                      {:available-modules (:available-modules module)
                       :imported-decls imported-decls})]
    (doseq [decl (:decls module)]
      (case (:op decl)
        :fn (check-fn-decl decl module decls)
        :data (check-invariants decl ctx decls)
        :union (check-invariants decl ctx decls)
        nil))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T10:20:15.316576-05:00", :module-hash "1798608639", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "1214713234"} {:id "form/1/declare", :kind "declare", :line 6, :end-line 6, :hash "-750848269"} {:id "form/2/declare", :kind "declare", :line 7, :end-line 7, :hash "83426057"} {:id "defn-/fail!", :kind "defn-", :line 9, :end-line 11, :hash "765867407"} {:id "defn-/decl-map", :kind "defn-", :line 13, :end-line 21, :hash "244840831"} {:id "defn-/fn-type", :kind "defn-", :line 23, :end-line 28, :hash "1095575499"} {:id "defn-/bottom-type", :kind "defn-", :line 30, :end-line 33, :hash "475588588"} {:id "defn-/bottom-type?", :kind "defn-", :line 35, :end-line 37, :hash "1715044112"} {:id "defn-/result", :kind "defn-", :line 39, :end-line 42, :hash "-1781274230"} {:id "defn-/local-type", :kind "defn-", :line 44, :end-line 46, :hash "-1632824710"} {:id "defn-/mutable-local?", :kind "defn-", :line 48, :end-line 50, :hash "1438220795"} {:id "defn-/bind-local", :kind "defn-", :line 52, :end-line 54, :hash "1658319653"} {:id "defn-/bind-mutable", :kind "defn-", :line 56, :end-line 60, :hash "-1396184064"} {:id "defn-/with-loop-types", :kind "defn-", :line 62, :end-line 64, :hash "208229580"} {:id "defn-/without-loop-types", :kind "defn-", :line 66, :end-line 68, :hash "932574932"} {:id "defn-/fn-decl-type", :kind "defn-", :line 70, :end-line 74, :hash "1335497367"} {:id "defn-/module-fn-env", :kind "defn-", :line 76, :end-line 87, :hash "-1474536557"} {:id "defn-/make-ctx", :kind "defn-", :line 89, :end-line 96, :hash "-1280194419"} {:id "defn-/literal-type", :kind "defn-", :line 98, :end-line 109, :hash "-2129463170"} {:id "defn-/compatible-types?", :kind "defn-", :line 111, :end-line 115, :hash "1551296287"} {:id "defn-/ensure-type=", :kind "defn-", :line 117, :end-line 121, :hash "1746399400"} {:id "defn-/ensure-arity=", :kind "defn-", :line 123, :end-line 127, :hash "266695403"} {:id "defn-/join-types", :kind "defn-", :line 129, :end-line 135, :hash "-1353034608"} {:id "defn-/enum-pattern?", :kind "defn-", :line 137, :end-line 139, :hash "-31853100"} {:id "defn-/bind-literal-pattern", :kind "defn-", :line 141, :end-line 147, :hash "-2136209369"} {:id "defn-/bind-binder-pattern", :kind "defn-", :line 149, :end-line 153, :hash "1161467246"} {:id "defn-/bind-pattern", :kind "defn-", :line 155, :end-line 163, :hash "-1814155925"} {:id "defn-/infer-local", :kind "defn-", :line 165, :end-line 171, :hash "195910838"} {:id "defn-/infer-record-get", :kind "defn-", :line 173, :end-line 186, :hash "-361239966"} {:id "defn-/imported-runtime-available?", :kind "defn-", :line 188, :end-line 193, :hash "-1783818705"} {:id "defn-/ensure-constructible-type!", :kind "defn-", :line 195, :end-line 199, :hash "908869040"} {:id "defn-/infer-construct", :kind "defn-", :line 201, :end-line 216, :hash "-1360397945"} {:id "defn-/infer-variant", :kind "defn-", :line 218, :end-line 243, :hash "-1236364359"} {:id "defn-/infer-if", :kind "defn-", :line 245, :end-line 253, :hash "-2003732048"} {:id "defn-/infer-match", :kind "defn-", :line 255, :end-line 269, :hash "262906490"} {:id "defn-/infer-call", :kind "defn-", :line 271, :end-line 285, :hash "-1175533470"} {:id "defn-/infer-lambda", :kind "defn-", :line 287, :end-line 300, :hash "-582701779"} {:id "defn-/infer-binding", :kind "defn-", :line 302, :end-line 306, :hash "737212167"} {:id "defn-/infer-let", :kind "defn-", :line 308, :end-line 315, :hash "-310092863"} {:id "defn-/infer-seq", :kind "defn-", :line 317, :end-line 322, :hash "-2028016492"} {:id "defn-/infer-var", :kind "defn-", :line 324, :end-line 330, :hash "325909027"} {:id "defn-/infer-set", :kind "defn-", :line 332, :end-line 340, :hash "1875378186"} {:id "defn-/infer-loop", :kind "defn-", :line 342, :end-line 357, :hash "867251552"} {:id "defn-/infer-recur", :kind "defn-", :line 359, :end-line 373, :hash "-1380623872"} {:id "defn-/infer-try", :kind "defn-", :line 375, :end-line 390, :hash "-481723416"} {:id "defn-/infer-raise", :kind "defn-", :line 392, :end-line 395, :hash "708997410"} {:id "defn-/infer-primitive-unary", :kind "defn-", :line 397, :end-line 403, :hash "300239028"} {:id "defn-/infer-primitive-binary", :kind "defn-", :line 405, :end-line 415, :hash "-679783036"} {:id "defn-/seq-element-type", :kind "defn-", :line 417, :end-line 424, :hash "-948667098"} {:id "defn-/map-value-type", :kind "defn-", :line 426, :end-line 432, :hash "1293096732"} {:id "defn-/ensure-seq-type", :kind "defn-", :line 434, :end-line 438, :hash "1156209343"} {:id "defn-/ensure-map-type", :kind "defn-", :line 440, :end-line 444, :hash "1186104024"} {:id "defn-/infer-seq-unary", :kind "defn-", :line 446, :end-line 451, :hash "-2137238691"} {:id "defn-/infer-seq-element", :kind "defn-", :line 453, :end-line 459, :hash "-1943390793"} {:id "defn-/infer-seq-rest", :kind "defn-", :line 461, :end-line 468, :hash "-1159837909"} {:id "defn-/infer-seq-concat", :kind "defn-", :line 470, :end-line 484, :hash "1566324222"} {:id "defn-/infer-seq-get", :kind "defn-", :line 486, :end-line 499, :hash "662784337"} {:id "defn-/infer-map-empty", :kind "defn-", :line 501, :end-line 503, :hash "-313109806"} {:id "defn-/infer-map-set", :kind "defn-", :line 505, :end-line 522, :hash "-627309182"} {:id "defn-/infer-map-get", :kind "defn-", :line 524, :end-line 537, :hash "1449919953"} {:id "defn-/infer-map-contains", :kind "defn-", :line 539, :end-line 551, :hash "831295168"} {:id "defn-/infer-map-keys", :kind "defn-", :line 553, :end-line 558, :hash "48903908"} {:id "defn-/infer-java-new", :kind "defn-", :line 560, :end-line 564, :hash "-676160678"} {:id "defn-/infer-java-call", :kind "defn-", :line 566, :end-line 580, :hash "1132722729"} {:id "defn-/infer-java-static-call", :kind "defn-", :line 582, :end-line 595, :hash "-1746811078"} {:id "defn-/infer-java-get-field", :kind "defn-", :line 597, :end-line 600, :hash "-146759388"} {:id "defn-/infer-java-static-get-field", :kind "defn-", :line 602, :end-line 604, :hash "595849367"} {:id "defn-/ensure-assignable-field-type!", :kind "defn-", :line 606, :end-line 612, :hash "-97045908"} {:id "defn-/infer-java-set-field", :kind "defn-", :line 614, :end-line 619, :hash "277510053"} {:id "defn-/infer-java-static-set-field", :kind "defn-", :line 621, :end-line 625, :hash "-808600186"} {:id "def/expr-inferers", :kind "def", :line 627, :end-line 748, :hash "-1075319976"} {:id "defn/infer-expr", :kind "defn", :line 750, :end-line 757, :hash "-1459178635"} {:id "defn/infer-expr-type", :kind "defn", :line 759, :end-line 761, :hash "1740972133"} {:id "defn-/check-bool-contract", :kind "defn-", :line 763, :end-line 767, :hash "1265459629"} {:id "defn-/passthrough-invariant-ctx", :kind "defn-", :line 769, :end-line 771, :hash "-1920191979"} {:id "defn-/invariant-ctx", :kind "defn-", :line 773, :end-line 784, :hash "1279370076"} {:id "defn-/check-invariants", :kind "defn-", :line 786, :end-line 793, :hash "-1390776170"} {:id "defn-/check-fn-decl", :kind "defn-", :line 795, :end-line 813, :hash "575604567"} {:id "defn/check-module", :kind "defn", :line 815, :end-line 828, :hash "1200879305"}]}
;; clj-mutate-manifest-end
