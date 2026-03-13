(ns airj.resolver
  (:require [airj.expr-walker :as expr-walker]
            [airj.imported-interfaces :as imported-interfaces]
            [clojure.set :as set]))

(declare pattern-binders)

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :resolve))))

(defn- declared-symbols
  [module]
  (set (map :name (:decls module))))

(defn- declared-function-symbols
  [module]
  (set (map :name (filter #(= :fn (:op %)) (:decls module)))))

(defn- imported-function-symbols
  [module]
  (->> (imported-interfaces/imported-decls module)
       (keep (fn [[symbol {:keys [decl]}]]
               (when (= :fn (:op decl))
                 symbol)))
       set))

(defn- imported-airj-symbols
  [module]
  (->> (:imports module)
       (filter #(= :airj-import (:op %)))
       (mapcat (fn [import]
                 (map (fn [symbol]
                        [symbol (:module import)])
                      (:symbols import))))))

(defn- check-imports
  [module]
  (let [declared (declared-symbols module)]
    (reduce (fn [seen [symbol imported-module]]
              (when (contains? declared symbol)
                (fail! "Imported symbol conflicts with local declaration."
                       {:module (:name module)
                        :symbol symbol
                        :imported-module imported-module}))
              (when-let [prior-module (get seen symbol)]
                (fail! "Duplicate imported symbol."
                       {:module (:name module)
                        :symbol symbol
                        :imported-module imported-module
                        :prior-module prior-module}))
              (assoc seen symbol imported-module))
            {}
            (imported-airj-symbols module))))

(defn- check-exports
  [module]
  (let [declared (declared-symbols module)]
    (doseq [export (:exports module)]
      (when-not (contains? declared export)
        (fail! "Unresolved export."
               {:module (:name module)
                :export export})))))

(defn- union-pattern-binders
  [pattern]
  (apply set/union #{} (map pattern-binders (:args pattern))))

(defn- record-pattern-binders
  [pattern]
  (apply set/union #{} (map (comp pattern-binders :pattern) (:fields pattern))))

(def ^:private pattern-binder-handlers
  {:wildcard-pattern (constantly #{})
   :literal-pattern (constantly #{})
   :binder-pattern (fn [pattern] #{(:name pattern)})
   :union-pattern union-pattern-binders
   :record-pattern record-pattern-binders})

(defn pattern-binders
  [pattern]
  (if-let [handler (get pattern-binder-handlers (:op pattern))]
    (handler pattern)
    #{}))

(defn- resolve-case
  [case env walk]
  (let [binder-names (pattern-binders (:pattern case))]
    (walk (:body case) (set/union env binder-names))))

(defn- resolve-expr-seq
  [exprs env walk]
  (reduce (fn [current-env expr]
            (walk expr current-env)
            (if (= :var (:op expr))
              (conj current-env (:name expr))
              current-env))
          env
          exprs)
  nil)

(defn- resolve-local-expr
  [expr env _walk]
  (when-not (contains? env (:name expr))
    (fail! "Unresolved local."
           {:name (:name expr)})))

(defn- resolve-call-expr
  [expr env walk]
  (walk (:callee expr) env)
  (resolve-expr-seq (:args expr) env walk))

(defn- resolve-construct-expr
  [expr env walk]
  (resolve-expr-seq (:args expr) env walk))

(defn- resolve-record-get-expr
  [expr env walk]
  (walk (:target expr) env))

(defn- resolve-if-expr
  [expr env walk]
  (walk (:test expr) env)
  (walk (:then expr) env)
  (walk (:else expr) env))

(defn- resolve-match-expr
  [expr env walk]
  (walk (:target expr) env)
  (doseq [case (:cases expr)]
    (resolve-case case env walk)))

(defn- resolve-scoped-bindings
  [bindings env walk]
  (reduce (fn [names binding]
            (do
              (walk (:expr binding) (set/union env names))
              (conj names (:name binding))))
          #{}
          bindings))

(defn- resolve-let-expr
  [expr env walk]
  (let [binding-names (resolve-scoped-bindings (:bindings expr) env walk)]
    (walk (:body expr) (set/union env binding-names))))

(defn- resolve-lambda-expr
  [expr env walk]
  (walk (:body expr)
        (set/union env (set (map :name (:params expr))))))

(defn- resolve-try-expr
  [expr env walk]
  (walk (:body expr) env)
  (doseq [catch (:catches expr)]
    (walk (:body catch) (conj env (:name catch))))
  (when-let [finally (:finally expr)]
    (walk finally env)))

(defn- resolve-var-expr
  [expr env walk]
  (walk (:init expr) env))

(defn- resolve-set-expr
  [expr env walk]
  (walk (:expr expr) env))

(def ^:private expr-resolvers
  {:local resolve-local-expr
   :call resolve-call-expr
   :int-add (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-sub (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-mul (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-div (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-mod (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-eq (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-lt (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-le (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-gt (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-ge (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :bool-eq (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int-ne (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :string-eq (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :string-concat (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :string-split-on (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :string-char-at (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :string-substring (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :int->string (fn [expr env walk] (walk (:arg expr) env))
   :string->int (fn [expr env walk] (walk (:arg expr) env))
   :string-length (fn [expr env walk] (walk (:arg expr) env))
   :string-trim (fn [expr env walk] (walk (:arg expr) env))
   :string-empty? (fn [expr env walk] (walk (:arg expr) env))
   :seq-empty? (fn [expr env walk] (walk (:arg expr) env))
   :seq-length (fn [expr env walk] (walk (:arg expr) env))
   :seq-first (fn [expr env walk] (walk (:arg expr) env))
   :seq-get (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :io-read-line (fn [_expr _env _walk] nil)
   :io-print (fn [expr env walk] (walk (:arg expr) env))
   :bool-not (fn [expr env walk] (walk (:arg expr) env))
   :bool-and (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :bool-or (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :io-println (fn [expr env walk] (walk (:arg expr) env))
   :construct resolve-construct-expr
   :variant resolve-construct-expr
   :record-get resolve-record-get-expr
   :if resolve-if-expr
   :match resolve-match-expr
   :let resolve-let-expr
   :seq (fn [expr env walk] (resolve-expr-seq (:exprs expr) env walk))
   :lambda resolve-lambda-expr
   :try resolve-try-expr
   :var resolve-var-expr
   :set resolve-set-expr
   :loop resolve-let-expr
   :recur (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :raise (fn [expr env walk] (walk (:expr expr) env))
   :java-new (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :java-call (fn [expr env walk]
                (walk (:target expr) env)
                (resolve-expr-seq (:args expr) env walk))
   :java-static-call (fn [expr env walk] (resolve-expr-seq (:args expr) env walk))
   :java-get-field (fn [expr env walk] (walk (:target expr) env))
   :java-set-field (fn [expr env walk]
                     (walk (:target expr) env)
                     (walk (:expr expr) env))
   :java-static-get-field (fn [_expr _env _walk] nil)
   :java-static-set-field (fn [expr env walk]
                            (walk (:expr expr) env))})

(defn resolve-expr
  [expr env]
  (expr-walker/walk-expr expr env expr-resolvers))

(defn- resolve-fn-decl
  [decl module-fns]
  (resolve-expr (:body decl)
                (set/union module-fns
                           (set (map :name (:params decl))))))

(defn resolve-module
  [module]
  (let [module-fns (set/union (declared-function-symbols module)
                              (imported-function-symbols module))]
    (check-imports module)
    (check-exports module)
    (doseq [decl (:decls module)]
      (when (= :fn (:op decl))
        (resolve-fn-decl decl module-fns))))
  module)

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T14:46:14.594355-05:00", :module-hash "-1874849798", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "956527207"} {:id "form/1/declare", :kind "declare", :line 6, :end-line 6, :hash "1701706515"} {:id "defn-/fail!", :kind "defn-", :line 8, :end-line 10, :hash "1487409999"} {:id "defn-/declared-symbols", :kind "defn-", :line 12, :end-line 14, :hash "1928601396"} {:id "defn-/declared-function-symbols", :kind "defn-", :line 16, :end-line 18, :hash "943514345"} {:id "defn-/imported-function-symbols", :kind "defn-", :line 20, :end-line 26, :hash "-337988883"} {:id "defn-/imported-airj-symbols", :kind "defn-", :line 28, :end-line 35, :hash "-1921326262"} {:id "defn-/check-imports", :kind "defn-", :line 37, :end-line 54, :hash "281404990"} {:id "defn-/check-exports", :kind "defn-", :line 56, :end-line 63, :hash "-822881886"} {:id "defn-/union-pattern-binders", :kind "defn-", :line 65, :end-line 67, :hash "411632936"} {:id "defn-/record-pattern-binders", :kind "defn-", :line 69, :end-line 71, :hash "-1442506129"} {:id "def/pattern-binder-handlers", :kind "def", :line 73, :end-line 78, :hash "1765640651"} {:id "defn/pattern-binders", :kind "defn", :line 80, :end-line 84, :hash "1177151671"} {:id "defn-/resolve-case", :kind "defn-", :line 86, :end-line 89, :hash "1606382642"} {:id "defn-/resolve-expr-seq", :kind "defn-", :line 91, :end-line 100, :hash "1630856682"} {:id "defn-/resolve-local-expr", :kind "defn-", :line 102, :end-line 106, :hash "112051099"} {:id "defn-/resolve-call-expr", :kind "defn-", :line 108, :end-line 111, :hash "1407574929"} {:id "defn-/resolve-construct-expr", :kind "defn-", :line 113, :end-line 115, :hash "-697969647"} {:id "defn-/resolve-record-get-expr", :kind "defn-", :line 117, :end-line 119, :hash "565073959"} {:id "defn-/resolve-if-expr", :kind "defn-", :line 121, :end-line 125, :hash "1449895441"} {:id "defn-/resolve-match-expr", :kind "defn-", :line 127, :end-line 131, :hash "-1909690062"} {:id "defn-/resolve-scoped-bindings", :kind "defn-", :line 133, :end-line 140, :hash "824488877"} {:id "defn-/resolve-let-expr", :kind "defn-", :line 142, :end-line 145, :hash "-649793452"} {:id "defn-/resolve-lambda-expr", :kind "defn-", :line 147, :end-line 150, :hash "1331153257"} {:id "defn-/resolve-try-expr", :kind "defn-", :line 152, :end-line 158, :hash "1855874410"} {:id "defn-/resolve-var-expr", :kind "defn-", :line 160, :end-line 162, :hash "-1302370245"} {:id "defn-/resolve-set-expr", :kind "defn-", :line 164, :end-line 166, :hash "80905257"} {:id "def/expr-resolvers", :kind "def", :line 168, :end-line 228, :hash "922557491"} {:id "defn/resolve-expr", :kind "defn", :line 230, :end-line 232, :hash "-214132923"} {:id "defn-/resolve-fn-decl", :kind "defn-", :line 234, :end-line 238, :hash "-1978958904"} {:id "defn/resolve-module", :kind "defn", :line 240, :end-line 249, :hash "-1647712934"}]}
;; clj-mutate-manifest-end
