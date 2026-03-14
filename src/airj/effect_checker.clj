(ns airj.effect-checker
  (:require [airj.expr-walker :as expr-walker]
            [airj.imported-interfaces :as imported-interfaces]
            [airj.java-effects :as java-effects]
            [airj.java-members :as java-members]
            [airj.java-types :as java-types]
            [airj.patterns :as patterns]
            [airj.type-checker :as type-checker]
            [clojure.set :as set]))

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :effect-check))))

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

(defn- module-fn-env
  [module decls]
  (merge
   (into {}
         (map (fn [decl]
                [(:name decl)
                 (fn-type (map :type (:params decl))
                          (:return-type decl)
                          (:effects decl))])
              (filter #(= :fn (:op %)) (vals decls))))
   (into {}
         (keep (fn [[symbol {:keys [decl]}]]
                 (when (= :fn (:op decl))
                   [symbol
                    (fn-type (map :type (:params decl))
                             (:return-type decl)
                             (:effects decl))])))
         (imported-interfaces/imported-decls module))))

(defn- ensure-subset
  [actual expected data]
  (let [actual-set (set actual)
        expected-set (set expected)
        undeclared (set/difference actual-set expected-set)]
    (when (seq undeclared)
      (fail! "Undeclared effects."
             (assoc data :undeclared (sort undeclared))))))

(defn- make-ctx
  [module decls]
  {:locals (module-fn-env module decls)
   :call-effects
   (merge
    (into {}
          (map (fn [decl]
                 [(:name decl) (set (:effects decl))])
               (filter #(= :fn (:op %)) (vals decls))))
    (into {}
          (keep (fn [[symbol {:keys [decl]}]]
                  (when (= :fn (:op decl))
                    [symbol (set (:effects decl))])))
          (imported-interfaces/imported-decls module)))})

(defn- bind-call-effects
  [ctx name effects]
  (assoc-in ctx [:call-effects name] (set effects)))

(defn- bind-local
  [ctx name type-expr]
  (assoc-in ctx [:locals name] type-expr))

(defn- bind-pattern-local
  [ctx pattern target-type _decls]
  (bind-local ctx (:name pattern) target-type))

(defn- bind-pattern-locals
  [ctx pattern target-type decls]
  (patterns/bind-pattern ctx
                         pattern
                         target-type
                         decls
                         {:bind-binder bind-pattern-local
                          :bind-literal (fn [current _pattern _target-type] current)
                          :fail! fail!}))

(declare callable-effects)

(defn- local-callable-effects
  [expr ctx decls]
  (cond
    (contains? (:call-effects ctx) (:name expr))
    (get-in ctx [:call-effects (:name expr)])

    (= :fn (:op (get decls (:name expr))))
    (set (:effects (get decls (:name expr))))

    :else nil))

(defn- if-callable-effects
  [expr ctx decls]
  (let [then-effects (callable-effects (:then expr) ctx decls)
        else-effects (callable-effects (:else expr) ctx decls)]
    (when (and then-effects else-effects (= then-effects else-effects))
      then-effects)))

(defn- callable-effects
  [expr ctx decls]
  (cond
    (= :local (:op expr))
    (local-callable-effects expr ctx decls)

    (= :lambda (:op expr))
    (set (:effects expr))

    (= :if (:op expr))
    (if-callable-effects expr ctx decls)

    (= :seq (:op expr))
    (callable-effects (last (:exprs expr)) ctx decls)

    :else nil))

(defn- binding-call-effects
  [binding ctx decls]
  (callable-effects (:expr binding) ctx decls))

(defn- effect-union
  [exprs ctx walk]
  (apply set/union #{} (map #(walk % ctx) exprs)))

(defn- call-effects
  [expr ctx decls]
  (or (callable-effects (:callee expr) ctx decls)
      (fail! "Unsupported call effect inference."
             {:expr expr})))

(defn- lambda-effects
  [expr ctx walk]
  (let [lambda-ctx (reduce (fn [current param]
                             (bind-local current (:name param) (:type param)))
                           ctx
                           (:params expr))
        body-effects (walk (:body expr) lambda-ctx)]
    (ensure-subset body-effects
                   (:effects expr)
                   {:lambda expr})
    #{}))

(defn- try-effects
  [expr ctx walk]
  (set/union (walk (:body expr) ctx)
             (apply set/union
                    #{}
                    (map (fn [catch]
                           (walk (:body catch)
                                 (bind-local ctx (:name catch) (:type catch))))
                         (:catches expr)))
             (if-let [finally-expr (:finally expr)]
               (walk finally-expr ctx)
               #{})))

(defn- binding-ctx
  [ctx binding decls]
  (let [binding-type (type-checker/infer-expr-type (:expr binding) ctx decls)
        local-ctx (bind-local ctx (:name binding) binding-type)]
    (if-let [bound-effects (binding-call-effects binding ctx decls)]
      (bind-call-effects local-ctx (:name binding) bound-effects)
      local-ctx)))

(defn- let-effects
  [expr ctx decls walk]
  (let [{:keys [effects ctx]}
        (reduce (fn [acc binding]
                  (let [binding-effects (walk (:expr binding) (:ctx acc))
                        updated-ctx (binding-ctx (:ctx acc) binding decls)]
                    {:effects (set/union (:effects acc) binding-effects)
                     :ctx updated-ctx}))
                {:effects #{}
                 :ctx ctx}
                (:bindings expr))]
    (set/union effects
               (walk (:body expr) ctx))))

(defn- seq-effects
  [expr ctx walk]
  (:effects
   (reduce (fn [{:keys [effects ctx]} part]
             (let [part-effects (walk part ctx)
                   updated-ctx (if (= :var (:op part))
                                 (bind-local ctx (:name part) (:type part))
                                 ctx)]
               {:effects (set/union effects part-effects)
                :ctx updated-ctx}))
           {:effects #{}
            :ctx ctx}
           (:exprs expr))))

(defn- loop-effects
  [expr ctx decls walk]
  (let [{:keys [effects ctx]}
        (reduce (fn [acc binding]
                  (let [binding-effects (walk (:expr binding) (:ctx acc))
                        updated-ctx (binding-ctx (:ctx acc) binding decls)]
                    {:effects (set/union (:effects acc) binding-effects)
                     :ctx updated-ctx}))
                {:effects #{}
                 :ctx ctx}
                (:bindings expr))]
    (set/union effects
               (walk (:body expr) ctx))))

(defn- expr-effect-handlers
  [decls]
  {:call (fn [expr ctx walk]
           (set/union (walk (:callee expr) ctx)
                      (effect-union (:args expr) ctx walk)
                      (call-effects expr ctx decls)))
   :int-add (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-sub (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-mul (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-div (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-mod (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-eq (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-lt (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-le (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-gt (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-ge (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-add (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-sub (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-mul (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-div (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-eq (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-lt (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-le (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-gt (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :float-ge (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-add (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-sub (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-mul (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-div (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-eq (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-lt (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-le (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-gt (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :double-ge (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :bool-eq (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int-ne (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :string-eq (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :string-concat (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :string-split-on (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :string-char-at (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :string-substring (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :int->string (fn [expr ctx walk] (walk (:arg expr) ctx))
   :int->float (fn [expr ctx walk] (walk (:arg expr) ctx))
   :int->double (fn [expr ctx walk] (walk (:arg expr) ctx))
   :float->double (fn [expr ctx walk] (walk (:arg expr) ctx))
   :double->float (fn [expr ctx walk] (walk (:arg expr) ctx))
   :string->int (fn [expr ctx walk]
                  (set/union #{'Foreign.Throw}
                             (walk (:arg expr) ctx)))
   :string-length (fn [expr ctx walk] (walk (:arg expr) ctx))
   :string-trim (fn [expr ctx walk] (walk (:arg expr) ctx))
   :string-empty? (fn [expr ctx walk] (walk (:arg expr) ctx))
   :seq-empty? (fn [expr ctx walk] (walk (:arg expr) ctx))
   :seq-length (fn [expr ctx walk] (walk (:arg expr) ctx))
   :seq-first (fn [expr ctx walk] (walk (:arg expr) ctx))
   :seq-get (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :io-read-line (fn [_expr _ctx _walk]
                   #{'Stdin.Read 'Foreign.Throw})
   :io-print (fn [expr ctx walk]
               (set/union #{'Stdout.Write}
                          (walk (:arg expr) ctx)))
   :io-println (fn [expr ctx walk]
                 (set/union #{'Stdout.Write}
                            (walk (:arg expr) ctx)))
   :bool-not (fn [expr ctx walk] (walk (:arg expr) ctx))
   :bool-and (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :bool-or (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :construct (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :variant (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :record-get (fn [expr ctx walk] (walk (:target expr) ctx))
   :if (fn [expr ctx walk]
         (set/union (walk (:test expr) ctx)
                    (walk (:then expr) ctx)
                    (walk (:else expr) ctx)))
   :match (fn [expr ctx walk]
            (let [target-effects (walk (:target expr) ctx)
                  target-type (type-checker/infer-expr-type (:target expr) ctx decls)]
              (set/union target-effects
                         (apply set/union
                                #{}
                                (map (fn [case]
                                       (walk (:body case)
                                             (bind-pattern-locals ctx
                                                                  (:pattern case)
                                                                  target-type
                                                                  decls)))
                                     (:cases expr))))))
   :let (fn [expr ctx walk] (let-effects expr ctx decls walk))
   :seq (fn [expr ctx walk] (seq-effects expr ctx walk))
   :lambda lambda-effects
   :try try-effects
   :var (fn [expr ctx walk]
          (set/union #{'State.Write}
                     (walk (:init expr) ctx)))
   :set (fn [expr ctx walk]
          (set/union #{'State.Write}
                     (walk (:expr expr) ctx)))
   :loop (fn [expr ctx walk] (loop-effects expr ctx decls walk))
   :recur (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :raise (fn [expr ctx walk]
            (set/union #{'Foreign.Throw}
                       (walk (:expr expr) ctx)))
   :java-new (fn [expr ctx walk]
               (let [arg-effects (effect-union (:args expr) ctx walk)
                     arg-types (mapv #(java-types/resolve-type (type-checker/infer-expr-type % ctx decls))
                                     (:args expr))
                     member (java-members/resolve-constructor (:class-name expr) arg-types)]
                 (set/union (java-effects/member-effects member)
                            arg-effects)))
   :java-call (fn [expr ctx walk]
                (let [target-effects (walk (:target expr) ctx)
                      arg-effects (effect-union (:args expr) ctx walk)
                      receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
                      member (java-members/resolve-instance-method receiver-type
                                                                   (:member-id expr)
                                                                   (:signature expr))]
                  (set/union (java-effects/member-effects member)
                             target-effects
                             arg-effects)))
   :java-static-call (fn [expr ctx walk]
                       (let [arg-effects (effect-union (:args expr) ctx walk)
                             member (java-members/resolve-static-method (:class-name expr)
                                                                        (:member-id expr)
                                                                        (:signature expr))]
                         (set/union (java-effects/member-effects member)
                                    arg-effects)))
   :java-get-field (fn [expr ctx walk]
                     (let [target-effects (walk (:target expr) ctx)
                           receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
                           member (java-members/resolve-instance-field receiver-type
                                                                      (:field-name expr)
                                                                      (:field-type expr))]
                       (set/union (java-effects/member-effects member)
                                  target-effects)))
   :java-set-field (fn [expr ctx walk]
                     (let [target-effects (walk (:target expr) ctx)
                           value-effects (walk (:expr expr) ctx)
                           receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
                           member (java-members/resolve-instance-field receiver-type
                                                                      (:field-name expr)
                                                                      (:field-type expr))]
                       (set/union (java-effects/interop-effects expr member)
                                  target-effects
                                  value-effects)))
   :java-static-get-field (fn [expr _ctx _walk]
                            (java-effects/member-effects
                             (java-members/resolve-static-field (:class-name expr)
                                                               (:field-name expr)
                                                               (:field-type expr))))
   :java-static-set-field (fn [expr ctx walk]
                            (let [value-effects (walk (:expr expr) ctx)
                                  member (java-members/resolve-static-field (:class-name expr)
                                                                            (:field-name expr)
                                                                            (:field-type expr))]
                              (set/union (java-effects/interop-effects expr member)
                                         value-effects)))})

(defn expr-effects*
  [expr ctx decls]
  (or (expr-walker/walk-expr expr ctx (expr-effect-handlers decls))
      #{}))

(defn expr-effects
  [expr decls]
  (expr-effects* expr {:call-effects {}}
                 decls))

(defn- check-contracts
  [decl ctx decls]
  (doseq [contract (concat (:requires decl) (:ensures decl))]
    (when (seq (expr-effects* contract ctx decls))
      (fail! "Contracts must be pure."
             {:fn (:name decl)
              :contract contract}))))

(defn- invariant-ctx
  [decl ctx]
  (case (:op decl)
    :data (reduce (fn [current field]
                    (bind-local current (:name field) (:type field)))
                  ctx
                  (:fields decl))
    :union (bind-local ctx 'self (:name decl))
    ctx))

(defn- check-invariants
  [decl ctx decls]
  (doseq [invariant (:invariants decl)]
    (when (seq (expr-effects* invariant (invariant-ctx decl ctx) decls))
      (fail! "Invariants must be pure."
             {:decl (:name decl)
              :invariant invariant}))))

(defn- check-fn-decl
  [decl ctx decls]
  (let [fn-ctx (reduce (fn [current param]
                         (bind-local current (:name param) (:type param)))
                       ctx
                       (:params decl))]
    (check-contracts decl fn-ctx decls)
    (ensure-subset (expr-effects* (:body decl) fn-ctx decls)
                 (:effects decl)
                 {:fn (:name decl)})))

(defn check-module
  [module]
  (let [decls (decl-map module)
        ctx (make-ctx module decls)]
    (doseq [decl (:decls module)]
      (case (:op decl)
        :fn (check-fn-decl decl ctx decls)
        :data (check-invariants decl ctx decls)
        :union (check-invariants decl ctx decls)
        nil))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T22:30:20.478247-05:00", :module-hash "539663119", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 9, :hash "-1748354227"} {:id "defn-/fail!", :kind "defn-", :line 11, :end-line 13, :hash "1619150228"} {:id "defn-/decl-map", :kind "defn-", :line 15, :end-line 23, :hash "244840831"} {:id "defn-/fn-type", :kind "defn-", :line 25, :end-line 30, :hash "1095575499"} {:id "defn-/module-fn-env", :kind "defn-", :line 32, :end-line 49, :hash "812924858"} {:id "defn-/ensure-subset", :kind "defn-", :line 51, :end-line 58, :hash "736755596"} {:id "defn-/make-ctx", :kind "defn-", :line 60, :end-line 73, :hash "-1033196036"} {:id "defn-/bind-call-effects", :kind "defn-", :line 75, :end-line 77, :hash "-198491742"} {:id "defn-/bind-local", :kind "defn-", :line 79, :end-line 81, :hash "1658319653"} {:id "defn-/bind-pattern-local", :kind "defn-", :line 83, :end-line 85, :hash "-357983377"} {:id "defn-/bind-pattern-locals", :kind "defn-", :line 87, :end-line 95, :hash "-954573492"} {:id "form/11/declare", :kind "declare", :line 97, :end-line 97, :hash "1302116001"} {:id "defn-/local-callable-effects", :kind "defn-", :line 99, :end-line 108, :hash "75363154"} {:id "defn-/if-callable-effects", :kind "defn-", :line 110, :end-line 115, :hash "765082213"} {:id "defn-/callable-effects", :kind "defn-", :line 117, :end-line 132, :hash "138662341"} {:id "defn-/binding-call-effects", :kind "defn-", :line 134, :end-line 136, :hash "1627316679"} {:id "defn-/effect-union", :kind "defn-", :line 138, :end-line 140, :hash "25526879"} {:id "defn-/call-effects", :kind "defn-", :line 142, :end-line 146, :hash "899417580"} {:id "defn-/lambda-effects", :kind "defn-", :line 148, :end-line 158, :hash "-660105978"} {:id "defn-/try-effects", :kind "defn-", :line 160, :end-line 171, :hash "-700469701"} {:id "defn-/binding-ctx", :kind "defn-", :line 173, :end-line 179, :hash "542892397"} {:id "defn-/let-effects", :kind "defn-", :line 181, :end-line 193, :hash "-1106430720"} {:id "defn-/seq-effects", :kind "defn-", :line 195, :end-line 207, :hash "-1853656406"} {:id "defn-/loop-effects", :kind "defn-", :line 209, :end-line 221, :hash "1753178605"} {:id "defn-/expr-effect-handlers", :kind "defn-", :line 223, :end-line 378, :hash "-1415835408"} {:id "defn/expr-effects*", :kind "defn", :line 380, :end-line 383, :hash "1640622703"} {:id "defn/expr-effects", :kind "defn", :line 385, :end-line 388, :hash "-425404032"} {:id "defn-/check-contracts", :kind "defn-", :line 390, :end-line 396, :hash "-190924871"} {:id "defn-/invariant-ctx", :kind "defn-", :line 398, :end-line 406, :hash "-1341369256"} {:id "defn-/check-invariants", :kind "defn-", :line 408, :end-line 414, :hash "-1140560710"} {:id "defn-/check-fn-decl", :kind "defn-", :line 416, :end-line 425, :hash "254737222"} {:id "defn/check-module", :kind "defn", :line 427, :end-line 437, :hash "1481191546"}]}
;; clj-mutate-manifest-end
