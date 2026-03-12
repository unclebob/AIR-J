(ns airj.effect-checker
  (:require [airj.expr-walker :as expr-walker]
            [airj.imported-interfaces :as imported-interfaces]
            [airj.java-effects :as java-effects]
            [airj.java-members :as java-members]
            [airj.java-types :as java-types]
            [airj.type-checker :as type-checker]
            [clojure.set :as set]))

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- decl-map
  [module]
  (into {} (map (juxt :name identity) (:decls module))))

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
  {:call-effects
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
  (let [body-effects (walk (:body expr) ctx)]
    (ensure-subset body-effects
                   (:effects expr)
                   {:lambda expr})
    #{}))

(defn- try-effects
  [expr ctx walk]
  (set/union (walk (:body expr) ctx)
             (effect-union (map :body (:catches expr)) ctx walk)
             (if-let [finally-expr (:finally expr)]
               (walk finally-expr ctx)
               #{})))

(defn- let-effects
  [expr ctx decls walk]
  (let [{:keys [effects ctx]}
        (reduce (fn [acc binding]
                  (let [binding-effects (walk (:expr binding) (:ctx acc))
                        updated-ctx (if-let [bound-effects (binding-call-effects binding (:ctx acc) decls)]
                                      (bind-call-effects (:ctx acc) (:name binding) bound-effects)
                                      (:ctx acc))]
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
   :construct (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :variant (fn [expr ctx walk] (effect-union (:args expr) ctx walk))
   :record-get (fn [expr ctx walk] (walk (:target expr) ctx))
   :if (fn [expr ctx walk]
         (set/union (walk (:test expr) ctx)
                    (walk (:then expr) ctx)
                    (walk (:else expr) ctx)))
   :match (fn [expr ctx walk]
            (set/union (walk (:target expr) ctx)
                       (effect-union (map :body (:cases expr)) ctx walk)))
   :let (fn [expr ctx walk] (let-effects expr ctx decls walk))
   :seq (fn [expr ctx walk] (effect-union (:exprs expr) ctx walk))
   :lambda lambda-effects
   :try try-effects
   :var (fn [expr ctx walk]
          (set/union #{'State.Write}
                     (walk (:init expr) ctx)))
   :set (fn [expr ctx walk]
          (set/union #{'State.Write}
                     (walk (:expr expr) ctx)))
   :loop (fn [expr ctx walk]
           (set/union (effect-union (map :expr (:bindings expr)) ctx walk)
                      (walk (:body expr) ctx)))
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

(defn- check-fn-decl
  [decl ctx decls]
  (check-contracts decl ctx decls)
  (ensure-subset (expr-effects* (:body decl) ctx decls)
                 (:effects decl)
                 {:fn (:name decl)}))

(defn check-module
  [module]
  (let [decls (decl-map module)
        ctx (make-ctx module decls)]
    (doseq [decl (:decls module)]
      (when (= :fn (:op decl))
        (check-fn-decl decl ctx decls)))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T12:51:14.690254-05:00", :module-hash "29862318", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 8, :hash "-1684993418"} {:id "defn-/fail!", :kind "defn-", :line 10, :end-line 12, :hash "879938479"} {:id "defn-/decl-map", :kind "defn-", :line 14, :end-line 16, :hash "1732448350"} {:id "defn-/ensure-subset", :kind "defn-", :line 18, :end-line 25, :hash "736755596"} {:id "defn-/make-ctx", :kind "defn-", :line 27, :end-line 39, :hash "-572541941"} {:id "defn-/bind-call-effects", :kind "defn-", :line 41, :end-line 43, :hash "-198491742"} {:id "form/6/declare", :kind "declare", :line 45, :end-line 45, :hash "1302116001"} {:id "defn-/local-callable-effects", :kind "defn-", :line 47, :end-line 56, :hash "75363154"} {:id "defn-/if-callable-effects", :kind "defn-", :line 58, :end-line 63, :hash "765082213"} {:id "defn-/callable-effects", :kind "defn-", :line 65, :end-line 80, :hash "138662341"} {:id "defn-/binding-call-effects", :kind "defn-", :line 82, :end-line 84, :hash "1627316679"} {:id "defn-/effect-union", :kind "defn-", :line 86, :end-line 88, :hash "-1296346923"} {:id "defn-/call-effects", :kind "defn-", :line 90, :end-line 94, :hash "899417580"} {:id "defn-/lambda-effects", :kind "defn-", :line 96, :end-line 102, :hash "1258837400"} {:id "defn-/try-effects", :kind "defn-", :line 104, :end-line 110, :hash "-1431839658"} {:id "defn-/let-effects", :kind "defn-", :line 112, :end-line 126, :hash "-1072935542"} {:id "defn-/expr-effect-handlers", :kind "defn-", :line 128, :end-line 202, :hash "2031043611"} {:id "defn/expr-effects*", :kind "defn", :line 204, :end-line 207, :hash "1640622703"} {:id "defn/expr-effects", :kind "defn", :line 209, :end-line 212, :hash "-425404032"} {:id "defn-/check-contracts", :kind "defn-", :line 214, :end-line 220, :hash "-190924871"} {:id "defn-/check-fn-decl", :kind "defn-", :line 222, :end-line 227, :hash "1943037450"} {:id "defn/check-module", :kind "defn", :line 229, :end-line 236, :hash "2040875463"}]}
;; clj-mutate-manifest-end
