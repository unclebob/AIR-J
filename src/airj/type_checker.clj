(ns airj.type-checker
  (:require [airj.imported-interfaces :as imported-interfaces]
            [airj.java-types :as java-types]
            [airj.patterns :as patterns]))

(declare infer-expr)
(declare infer-expr-type)

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- decl-map
  [module]
  (into {} (map (juxt :name identity) (:decls module))))

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
  [locals]
  {:locals locals
   :mutable #{}})

(defn- literal-type
  [expr]
  (cond
    (true? expr) 'Bool
    (false? expr) 'Bool
    (integer? expr) 'Int
    (string? expr) 'String
    :else nil))

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
        target-decl (get decls target-type)
        result-type (and target-decl (patterns/field-type target-decl (:field expr)))]
    (when-not target-decl
      (fail! "Expected record type."
             {:type target-type}))
    (result (or result-type
                (fail! "Unknown field."
                       {:type target-type
                        :field (:field expr)}))
            ctx)))

(defn- infer-construct
  [expr ctx decls]
  (let [decl (get decls (:type expr))]
    (when-not decl
      (fail! "Unknown constructed type."
             {:type (:type expr)}))
    (ensure-arity= (count (:fields decl))
                   (count (:args expr))
                   {:type (:type expr)})
    (doseq [[field arg] (map vector (:fields decl) (:args expr))]
      (ensure-type= (:type field)
                    (infer-expr-type arg ctx decls)
                    {:type (:type expr)
                     :field (:name field)}))
    (result (:type expr) ctx)))

(defn- infer-variant
  [expr ctx decls]
  (let [decl (get decls (:type expr))]
    (when-not decl
      (fail! "Unknown constructed type."
             {:type (:type expr)}))
    (when-not (= :union (:op decl))
      (fail! "Expected union type."
             {:type (:type expr)}))
    (let [variant (patterns/union-variant decl (:name expr))]
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

(defn- infer-java-set-field
  [expr ctx decls]
  (infer-expr-type (:target expr) ctx decls)
  (let [actual-type (infer-expr-type (:expr expr) ctx decls)]
    (when-not (java-types/assignable-type-expr? (:field-type expr) actual-type)
      (fail! "Type mismatch."
             {:expr expr
              :expected (:field-type expr)
              :actual actual-type})))
  (result 'Unit ctx))

(defn- infer-java-static-set-field
  [expr ctx decls]
  (let [actual-type (infer-expr-type (:expr expr) ctx decls)]
    (when-not (java-types/assignable-type-expr? (:field-type expr) actual-type)
      (fail! "Type mismatch."
             {:expr expr
              :expected (:field-type expr)
              :actual actual-type})))
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
   :bool-eq (fn [expr ctx decls] (infer-primitive-binary expr ctx decls 'Bool 'Bool))
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

(defn- invariant-ctx
  [decl ctx]
  (case (:op decl)
    :data (reduce (fn [acc field]
                    (bind-local acc (:name field) (:type field)))
                  ctx
                  (:fields decl))
    :union (bind-local ctx 'self (:name decl))
    ctx))

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
  (let [ctx (reduce (fn [acc param]
                      (bind-local acc (:name param) (:type param)))
                    (make-ctx (module-fn-env module decls))
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
        ctx (make-ctx (module-fn-env module decls))]
    (doseq [decl (:decls module)]
      (case (:op decl)
        :fn (check-fn-decl decl module decls)
        :data (check-invariants decl ctx decls)
        :union (check-invariants decl ctx decls)
        nil))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T09:23:13.94814-05:00", :module-hash "584761644", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "1214713234"} {:id "form/1/declare", :kind "declare", :line 6, :end-line 6, :hash "-750848269"} {:id "form/2/declare", :kind "declare", :line 7, :end-line 7, :hash "83426057"} {:id "defn-/fail!", :kind "defn-", :line 9, :end-line 11, :hash "879938479"} {:id "defn-/decl-map", :kind "defn-", :line 13, :end-line 15, :hash "1732448350"} {:id "defn-/fn-type", :kind "defn-", :line 17, :end-line 22, :hash "1095575499"} {:id "defn-/bottom-type", :kind "defn-", :line 24, :end-line 27, :hash "475588588"} {:id "defn-/bottom-type?", :kind "defn-", :line 29, :end-line 31, :hash "1715044112"} {:id "defn-/result", :kind "defn-", :line 33, :end-line 36, :hash "-1781274230"} {:id "defn-/local-type", :kind "defn-", :line 38, :end-line 40, :hash "-1632824710"} {:id "defn-/mutable-local?", :kind "defn-", :line 42, :end-line 44, :hash "1438220795"} {:id "defn-/bind-local", :kind "defn-", :line 46, :end-line 48, :hash "1658319653"} {:id "defn-/bind-mutable", :kind "defn-", :line 50, :end-line 54, :hash "-1396184064"} {:id "defn-/with-loop-types", :kind "defn-", :line 56, :end-line 58, :hash "208229580"} {:id "defn-/without-loop-types", :kind "defn-", :line 60, :end-line 62, :hash "932574932"} {:id "defn-/fn-decl-type", :kind "defn-", :line 64, :end-line 68, :hash "1335497367"} {:id "defn-/module-fn-env", :kind "defn-", :line 70, :end-line 81, :hash "1452456309"} {:id "defn-/make-ctx", :kind "defn-", :line 83, :end-line 86, :hash "-1593732056"} {:id "defn-/literal-type", :kind "defn-", :line 88, :end-line 95, :hash "1446793260"} {:id "defn-/compatible-types?", :kind "defn-", :line 97, :end-line 101, :hash "1551296287"} {:id "defn-/ensure-type=", :kind "defn-", :line 103, :end-line 107, :hash "1746399400"} {:id "defn-/ensure-arity=", :kind "defn-", :line 109, :end-line 113, :hash "266695403"} {:id "defn-/join-types", :kind "defn-", :line 115, :end-line 121, :hash "-1353034608"} {:id "defn-/enum-pattern?", :kind "defn-", :line 123, :end-line 125, :hash "-31853100"} {:id "defn-/bind-literal-pattern", :kind "defn-", :line 127, :end-line 133, :hash "-2136209369"} {:id "defn-/bind-binder-pattern", :kind "defn-", :line 135, :end-line 139, :hash "1161467246"} {:id "defn-/bind-pattern", :kind "defn-", :line 141, :end-line 149, :hash "-1814155925"} {:id "defn-/infer-local", :kind "defn-", :line 151, :end-line 157, :hash "195910838"} {:id "defn-/infer-record-get", :kind "defn-", :line 159, :end-line 171, :hash "-1783299120"} {:id "defn-/infer-construct", :kind "defn-", :line 173, :end-line 187, :hash "805710868"} {:id "defn-/infer-variant", :kind "defn-", :line 189, :end-line 213, :hash "-1586441720"} {:id "defn-/infer-if", :kind "defn-", :line 215, :end-line 223, :hash "-2003732048"} {:id "defn-/infer-match", :kind "defn-", :line 225, :end-line 239, :hash "262906490"} {:id "defn-/infer-call", :kind "defn-", :line 241, :end-line 255, :hash "-1175533470"} {:id "defn-/infer-lambda", :kind "defn-", :line 257, :end-line 270, :hash "-582701779"} {:id "defn-/infer-binding", :kind "defn-", :line 272, :end-line 276, :hash "737212167"} {:id "defn-/infer-let", :kind "defn-", :line 278, :end-line 285, :hash "-310092863"} {:id "defn-/infer-seq", :kind "defn-", :line 287, :end-line 292, :hash "-2028016492"} {:id "defn-/infer-var", :kind "defn-", :line 294, :end-line 300, :hash "325909027"} {:id "defn-/infer-set", :kind "defn-", :line 302, :end-line 310, :hash "1875378186"} {:id "defn-/infer-loop", :kind "defn-", :line 312, :end-line 327, :hash "867251552"} {:id "defn-/infer-recur", :kind "defn-", :line 329, :end-line 343, :hash "-1380623872"} {:id "defn-/infer-try", :kind "defn-", :line 345, :end-line 360, :hash "-481723416"} {:id "defn-/infer-raise", :kind "defn-", :line 362, :end-line 365, :hash "708997410"} {:id "defn-/infer-primitive-unary", :kind "defn-", :line 367, :end-line 373, :hash "300239028"} {:id "defn-/infer-primitive-binary", :kind "defn-", :line 375, :end-line 385, :hash "-679783036"} {:id "defn-/infer-java-new", :kind "defn-", :line 387, :end-line 391, :hash "-676160678"} {:id "defn-/infer-java-call", :kind "defn-", :line 393, :end-line 407, :hash "1132722729"} {:id "defn-/infer-java-static-call", :kind "defn-", :line 409, :end-line 422, :hash "-1746811078"} {:id "defn-/infer-java-get-field", :kind "defn-", :line 424, :end-line 427, :hash "-146759388"} {:id "defn-/infer-java-static-get-field", :kind "defn-", :line 429, :end-line 431, :hash "595849367"} {:id "defn-/infer-java-set-field", :kind "defn-", :line 433, :end-line 442, :hash "407102745"} {:id "defn-/infer-java-static-set-field", :kind "defn-", :line 444, :end-line 452, :hash "-1094123431"} {:id "def/expr-inferers", :kind "def", :line 454, :end-line 491, :hash "823611375"} {:id "defn/infer-expr", :kind "defn", :line 493, :end-line 500, :hash "-1459178635"} {:id "defn/infer-expr-type", :kind "defn", :line 502, :end-line 504, :hash "1740972133"} {:id "defn-/check-bool-contract", :kind "defn-", :line 506, :end-line 510, :hash "1265459629"} {:id "defn-/invariant-ctx", :kind "defn-", :line 512, :end-line 520, :hash "-1916527066"} {:id "defn-/check-invariants", :kind "defn-", :line 522, :end-line 529, :hash "-1390776170"} {:id "defn-/check-fn-decl", :kind "defn-", :line 531, :end-line 546, :hash "-1386237267"} {:id "defn/check-module", :kind "defn", :line 548, :end-line 558, :hash "1585922195"}]}
;; clj-mutate-manifest-end
