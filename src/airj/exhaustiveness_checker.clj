(ns airj.exhaustiveness-checker
  (:require [airj.expr-walker :as expr-walker]
            [airj.imported-interfaces :as imported-interfaces]
            [airj.patterns :as patterns]
            [airj.type-checker :as type-checker]))

(declare bind-pattern)
(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :exhaustiveness-check))))

(defn- decl-map
  [module]
  (merge (into {} (map (juxt :name identity) (:decls module)))
         (into {}
               (map (fn [[name {:keys [decl]}]]
                      [name decl]))
               (imported-interfaces/imported-decls module))))

(defn- fn-type
  [params return-type effects]
  {:op :fn-type
   :params (vec params)
   :return-type return-type
   :effects (vec effects)})

(defn- module-fn-env
  [decls]
  (into {}
        (map (fn [decl]
               [(:name decl)
                (fn-type (map :type (:params decl))
                         (:return-type decl)
                         (:effects decl))])
             (filter #(= :fn (:op %)) (vals decls)))))

(defn- make-ctx
  [decls]
  {:locals (module-fn-env decls)
   :mutable #{}})

(defn- enum-pattern?
  [pattern target-type decls]
  (patterns/enum-pattern? pattern target-type decls))

(defn- exhaustive-pattern?
  [pattern target-type decls]
  (or (= :wildcard-pattern (:op pattern))
      (and (= :binder-pattern (:op pattern))
           (not (enum-pattern? pattern target-type decls)))))

(defn- bool-pattern-cover
  [pattern]
  (when (= :literal-pattern (:op pattern))
    (:literal pattern)))

(defn- enum-pattern-cover
  [pattern target-type decls]
  (when (enum-pattern? pattern target-type decls)
    (:name pattern)))

(defn- union-pattern-cover
  [pattern]
  (when (= :union-pattern (:op pattern))
    (:name pattern)))

(defn- pattern-cover
  [pattern target-type decls]
  (let [decl (patterns/decl-for-type decls target-type)]
    (cond
      (exhaustive-pattern? pattern target-type decls) :all
      (= 'Bool target-type) (bool-pattern-cover pattern)
      (= :enum (:op decl)) (enum-pattern-cover pattern target-type decls)
      (= :union (:op decl)) (union-pattern-cover pattern)
      :else nil)))

(defn- bool-exhaustive?
  [covers]
  (and (contains? covers true)
       (contains? covers false)))

(defn- enum-exhaustive?
  [decl covers]
  (every? covers (:variants decl)))

(defn- union-exhaustive?
  [decl covers]
  (every? covers (map :name (:variants decl))))

(defn- supported-exhaustive-type?
  [target-type decl]
  (or (= 'Bool target-type)
      (= :enum (:op decl))
      (= :union (:op decl))))

(defn- exhaustive-covers?
  [target-type decl covers]
  (or (contains? covers :all)
      (and (= 'Bool target-type) (bool-exhaustive? covers))
      (and (= :enum (:op decl)) (enum-exhaustive? decl covers))
      (and (= :union (:op decl)) (union-exhaustive? decl covers))))

(defn- check-match
  [expr ctx decls walk]
  (let [target-type (type-checker/infer-expr-type (:target expr) ctx decls)
        covers (set (keep #(pattern-cover (:pattern %) target-type decls)
                          (:cases expr)))
        decl (patterns/decl-for-type decls target-type)]
    (when (and (supported-exhaustive-type? target-type decl)
               (not (exhaustive-covers? target-type decl covers)))
      (fail! "Non-exhaustive match."
             {:expr expr
              :type target-type}))
    (doseq [case (:cases expr)]
      (walk (:body case)
            (bind-pattern ctx (:pattern case) target-type decls)))))

(defn- bind-literal-pattern
  [ctx _pattern _target-type]
  ctx)

(defn- bind-binder-pattern
  [ctx pattern target-type decls]
  (if (enum-pattern? pattern target-type decls)
    ctx
    (assoc-in ctx [:locals (:name pattern)] target-type)))

(defn bind-pattern
  [ctx pattern target-type decls]
  (patterns/bind-pattern ctx
                         pattern
                         target-type
                         decls
                         {:bind-binder bind-binder-pattern
                          :bind-literal bind-literal-pattern
                          :fail! fail!}))

(defn- extend-let-ctx
  [ctx bindings decls]
  (reduce (fn [acc binding]
            (assoc-in acc
                      [:locals (:name binding)]
                      (type-checker/infer-expr-type (:expr binding) acc decls)))
          ctx
          bindings))

(defn- expr-checkers
  [decls]
  {:call (fn [expr ctx walk]
           (walk (:callee expr) ctx)
           (doseq [arg (:args expr)]
             (walk arg ctx)))
   :int-add (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-sub (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-mul (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-div (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-mod (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-eq (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-lt (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-le (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-gt (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-ge (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :float-add (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :float-sub (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :float-mul (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :float-div (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :float-eq (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :float-lt (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :float-le (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :float-gt (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :float-ge (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :double-add (fn [expr ctx walk]
                 (doseq [arg (:args expr)]
                   (walk arg ctx)))
   :double-sub (fn [expr ctx walk]
                 (doseq [arg (:args expr)]
                   (walk arg ctx)))
   :double-mul (fn [expr ctx walk]
                 (doseq [arg (:args expr)]
                   (walk arg ctx)))
   :double-div (fn [expr ctx walk]
                 (doseq [arg (:args expr)]
                   (walk arg ctx)))
   :double-eq (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :double-lt (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :double-le (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :double-gt (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :double-ge (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :bool-eq (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-ne (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :string-eq (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :string-concat (fn [expr ctx walk]
                    (doseq [arg (:args expr)]
                      (walk arg ctx)))
   :string-split-on (fn [expr ctx walk]
                      (doseq [arg (:args expr)]
                        (walk arg ctx)))
   :string-char-at (fn [expr ctx walk]
                     (doseq [arg (:args expr)]
                       (walk arg ctx)))
   :string-substring (fn [expr ctx walk]
                       (doseq [arg (:args expr)]
                         (walk arg ctx)))
   :int->string (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :int->float (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :int->double (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :float->double (fn [expr ctx walk]
                    (walk (:arg expr) ctx))
   :double->float (fn [expr ctx walk]
                    (walk (:arg expr) ctx))
   :string->int (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :string-length (fn [expr ctx walk]
                    (walk (:arg expr) ctx))
   :string-trim (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :string-empty? (fn [expr ctx walk]
                    (walk (:arg expr) ctx))
   :seq-empty? (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :seq-length (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :seq-first (fn [expr ctx walk]
                (walk (:arg expr) ctx))
   :seq-get (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :io-read-line (fn [_expr _ctx _walk] nil)
   :io-print (fn [expr ctx walk]
               (walk (:arg expr) ctx))
   :bool-not (fn [expr ctx walk]
               (walk (:arg expr) ctx))
   :bool-and (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :bool-or (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :io-println (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :construct (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :variant (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :record-get (fn [expr ctx walk]
                 (walk (:target expr) ctx))
   :if (fn [expr ctx walk]
         (walk (:test expr) ctx)
         (walk (:then expr) ctx)
         (walk (:else expr) ctx))
   :match (fn [expr ctx walk] (check-match expr ctx decls walk))
   :let (fn [expr ctx walk]
          (doseq [binding (:bindings expr)]
            (walk (:expr binding) ctx))
          (walk (:body expr)
                (extend-let-ctx ctx (:bindings expr) decls)))
   :seq (fn [expr ctx walk]
          (reduce (fn [current-ctx part]
                    (walk part current-ctx)
                    (if (= :var (:op part))
                      (assoc-in current-ctx
                                [:locals (:name part)]
                                (type-checker/infer-expr-type part current-ctx decls))
                      current-ctx))
                  ctx
                  (:exprs expr))
          nil)
   :lambda (fn [expr ctx walk]
             (let [lambda-ctx (reduce (fn [acc param]
                                        (assoc-in acc [:locals (:name param)] (:type param)))
                                      ctx
                                      (:params expr))]
               (walk (:body expr) lambda-ctx)))
   :try (fn [expr ctx walk]
          (walk (:body expr) ctx)
          (doseq [catch (:catches expr)]
            (walk (:body catch)
                  (assoc-in ctx [:locals (:name catch)] (:type catch))))
          (when-let [finally-expr (:finally expr)]
            (walk finally-expr ctx)))
   :var (fn [expr ctx walk]
          (walk (:init expr) ctx))
   :set (fn [expr ctx walk]
          (walk (:expr expr) ctx))
   :loop (fn [expr ctx walk]
           (let [loop-ctx (extend-let-ctx ctx (:bindings expr) decls)]
             (doseq [binding (:bindings expr)]
               (walk (:expr binding) ctx))
             (walk (:body expr) loop-ctx)))
   :recur (fn [expr ctx walk]
            (doseq [arg (:args expr)]
              (walk arg ctx)))
   :raise (fn [expr ctx walk]
            (walk (:expr expr) ctx))
   :java-new (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :java-call (fn [expr ctx walk]
                (walk (:target expr) ctx)
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :java-static-call (fn [expr ctx walk]
                       (doseq [arg (:args expr)]
                         (walk arg ctx)))
   :java-get-field (fn [expr ctx walk]
                     (walk (:target expr) ctx))
   :java-set-field (fn [expr ctx walk]
                     (walk (:target expr) ctx)
                     (walk (:expr expr) ctx))
   :java-static-get-field (fn [_expr _ctx _walk] nil)
   :java-static-set-field (fn [expr ctx walk]
                            (walk (:expr expr) ctx))})

(defn check-expr
  [expr ctx decls]
  (expr-walker/walk-expr expr ctx (expr-checkers decls)))

(defn- check-fn-decl
  [decl decls]
  (let [ctx (reduce (fn [acc param]
                      (assoc-in acc [:locals (:name param)] (:type param)))
                    (make-ctx decls)
                    (:params decl))]
    (check-expr (:body decl) ctx decls)))

(defn check-module
  [module]
  (let [decls (decl-map module)]
    (doseq [decl (:decls module)]
      (when (= :fn (:op decl))
        (check-fn-decl decl decls)))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T22:30:44.380146-05:00", :module-hash "1849569700", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "160254700"} {:id "form/1/declare", :kind "declare", :line 7, :end-line 7, :hash "569079974"} {:id "defn-/fail!", :kind "defn-", :line 8, :end-line 10, :hash "-696871237"} {:id "defn-/decl-map", :kind "defn-", :line 12, :end-line 18, :hash "2024128403"} {:id "defn-/fn-type", :kind "defn-", :line 20, :end-line 25, :hash "1095575499"} {:id "defn-/module-fn-env", :kind "defn-", :line 27, :end-line 35, :hash "-2096891579"} {:id "defn-/make-ctx", :kind "defn-", :line 37, :end-line 40, :hash "-261742596"} {:id "defn-/enum-pattern?", :kind "defn-", :line 42, :end-line 44, :hash "-31853100"} {:id "defn-/exhaustive-pattern?", :kind "defn-", :line 46, :end-line 50, :hash "1946202862"} {:id "defn-/bool-pattern-cover", :kind "defn-", :line 52, :end-line 55, :hash "-1417694324"} {:id "defn-/enum-pattern-cover", :kind "defn-", :line 57, :end-line 60, :hash "643520114"} {:id "defn-/union-pattern-cover", :kind "defn-", :line 62, :end-line 65, :hash "-150644182"} {:id "defn-/pattern-cover", :kind "defn-", :line 67, :end-line 75, :hash "558010568"} {:id "defn-/bool-exhaustive?", :kind "defn-", :line 77, :end-line 80, :hash "1512031583"} {:id "defn-/enum-exhaustive?", :kind "defn-", :line 82, :end-line 84, :hash "482651886"} {:id "defn-/union-exhaustive?", :kind "defn-", :line 86, :end-line 88, :hash "-972502524"} {:id "defn-/supported-exhaustive-type?", :kind "defn-", :line 90, :end-line 94, :hash "1140424323"} {:id "defn-/exhaustive-covers?", :kind "defn-", :line 96, :end-line 101, :hash "-416121724"} {:id "defn-/check-match", :kind "defn-", :line 103, :end-line 116, :hash "430509134"} {:id "defn-/bind-literal-pattern", :kind "defn-", :line 118, :end-line 120, :hash "-896453871"} {:id "defn-/bind-binder-pattern", :kind "defn-", :line 122, :end-line 126, :hash "868154269"} {:id "defn/bind-pattern", :kind "defn", :line 128, :end-line 136, :hash "-488411314"} {:id "defn-/extend-let-ctx", :kind "defn-", :line 138, :end-line 145, :hash "-1584068064"} {:id "defn-/expr-checkers", :kind "defn-", :line 147, :end-line 371, :hash "-2013158576"} {:id "defn/check-expr", :kind "defn", :line 373, :end-line 375, :hash "1259683610"} {:id "defn-/check-fn-decl", :kind "defn-", :line 377, :end-line 383, :hash "-640781828"} {:id "defn/check-module", :kind "defn", :line 385, :end-line 391, :hash "1272077568"}]}
;; clj-mutate-manifest-end
