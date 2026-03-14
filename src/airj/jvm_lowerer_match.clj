(ns airj.jvm-lowerer-match)

(defn- declared-type-root
  [type-expr]
  (if (seq? type-expr)
    (first type-expr)
    type-expr))

(defn- union-pattern-bindings
  [target-expr target-type pattern ctx {:keys [union-variant union-variant-class-name runtime-union-variant-class-name lower-type runtime-union-variant-field-jvm-types]}]
  (let [variant (union-variant ctx target-type (:name pattern))
        variant-class-name (if runtime-union-variant-class-name
                             (runtime-union-variant-class-name ctx
                                                               target-type
                                                               (:name pattern))
                             (union-variant-class-name (:module-name ctx)
                                                       (declared-type-root target-type)
                                                       (:name pattern)))
        runtime-field-types (if runtime-union-variant-field-jvm-types
                              (runtime-union-variant-field-jvm-types ctx
                                                                     target-type
                                                                     (:name pattern))
                              (mapv #(lower-type (:type %)
                                                 ctx)
                                    (:fields variant)))]
    (->> (map vector (:fields variant) runtime-field-types (:args pattern))
         (keep (fn [[field runtime-field-type arg-pattern]]
                 (when (= :binder-pattern (:op arg-pattern))
                   {:name (:name arg-pattern)
                    :expr {:op :jvm-variant-field
                           :target target-expr
                           :class-name variant-class-name
                           :field (:name field)
                           :field-jvm-type runtime-field-type
                           :jvm-type (lower-type (:type field) ctx)}})))
         vec)))

(def ^:private match-test-handlers
  {:literal-pattern (fn [pattern target-expr _ _ {:keys [lower-literal]}]
                      {:op :jvm-literal-test
                       :target target-expr
                       :literal (lower-literal (:literal pattern))})
   :wildcard-pattern (fn [_ _ _ _ _]
                       {:op :jvm-always-true})
   :binder-pattern (fn [_ _ _ _ _]
                     {:op :jvm-always-true})
   :union-pattern (fn [pattern target-expr target-type ctx {:keys [union-variant-class-name runtime-union-variant-class-name]}]
                    (let [class-name (if runtime-union-variant-class-name
                                       (runtime-union-variant-class-name ctx
                                                                         target-type
                                                                         (:name pattern))
                                       (union-variant-class-name (:module-name ctx)
                                                                 (declared-type-root target-type)
                                                                 (:name pattern)))]
                      {:op :jvm-instance-of
                       :target target-expr
                       :class-name class-name}))})

(defn- lower-match-test
  [pattern target-expr target-type ctx helpers]
  (if-let [handler (get match-test-handlers (:op pattern))]
    (handler pattern target-expr target-type ctx helpers)
    ((:fail! helpers) "Unsupported lowered match pattern."
                      {:pattern pattern})))

(def ^:private match-bindings-handlers
  {:wildcard-pattern (fn [_ _ _ _ _]
                       [])
   :literal-pattern (fn [_ _ _ _ _]
                      [])
   :binder-pattern (fn [pattern target-expr _ _ _]
                     [{:name (:name pattern)
                       :expr target-expr}])
   :union-pattern (fn [pattern target-expr target-type ctx helpers]
                    (union-pattern-bindings target-expr target-type pattern ctx helpers))})

(defn- lower-match-bindings
  [target-expr target-type pattern ctx helpers]
  (if-let [handler (get match-bindings-handlers (:op pattern))]
    (handler pattern target-expr target-type ctx helpers)
    ((:fail! helpers) "Unsupported lowered match bindings."
                      {:pattern pattern})))

(defn- bind-union-pattern-locals
  [ctx target-type pattern {:keys [bind-local union-variant]}]
  (reduce (fn [acc [field arg-pattern]]
            (if (= :binder-pattern (:op arg-pattern))
              (bind-local acc (:name arg-pattern) (:type field))
              acc))
          ctx
          (map vector
               (:fields (union-variant ctx target-type (:name pattern)))
               (:args pattern))))

(def ^:private pattern-locals-handlers
  {:wildcard-pattern (fn [ctx _ _ _]
                       ctx)
   :literal-pattern (fn [ctx _ _ _]
                      ctx)
   :binder-pattern (fn [ctx target-type pattern {:keys [bind-local]}]
                     (bind-local ctx (:name pattern) target-type))
   :union-pattern bind-union-pattern-locals})

(defn- bind-pattern-locals
  [ctx target-type pattern helpers]
  (if-let [handler (get pattern-locals-handlers (:op pattern))]
    (handler ctx target-type pattern helpers)
    ((:fail! helpers) "Unsupported lowered pattern locals."
                      {:pattern pattern})))

(defn- lower-match-case
  [case target-expr target-type ctx helpers]
  (let [pattern (:pattern case)
        body-ctx (bind-pattern-locals ctx target-type pattern helpers)]
    {:test (lower-match-test pattern target-expr target-type ctx helpers)
     :bindings (lower-match-bindings target-expr target-type pattern ctx helpers)
     :body ((:lower-expr helpers) (:body case) body-ctx)}))

(defn lower-match
  [expr ctx helpers]
  (let [target-expr ((:lower-expr helpers) (:target expr) ctx)
        target-type ((:infer-type helpers) (:target expr) ctx)]
    {:op :jvm-match
     :target target-expr
     :cases (mapv #(lower-match-case % target-expr target-type ctx helpers) (:cases expr))
     :jvm-type ((:lower-expr-type helpers)
                ((:infer-type helpers) expr ctx)
                ctx)}))

(defn infer-match-type
  [expr ctx helpers]
  (let [target-type ((:infer-type helpers) (:target expr) ctx)
        case-types (mapv (fn [case]
                           ((:infer-type helpers)
                            (:body case)
                            (bind-pattern-locals ctx
                                                 target-type
                                                 (:pattern case)
                                                 helpers)))
                         (:cases expr))]
    (reduce (fn [current case-type]
              ((:join-branch-types helpers)
               current
               case-type
               {:expr expr}))
            (first case-types)
            (rest case-types))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T06:57:14.458167-05:00", :module-hash "-924026007", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-1354362744"} {:id "defn-/declared-type-root", :kind "defn-", :line 3, :end-line 7, :hash "-1385718868"} {:id "defn-/union-pattern-bindings", :kind "defn-", :line 9, :end-line 36, :hash "-1640229960"} {:id "def/match-test-handlers", :kind "def", :line 38, :end-line 57, :hash "784899974"} {:id "defn-/lower-match-test", :kind "defn-", :line 59, :end-line 64, :hash "-1940850032"} {:id "def/match-bindings-handlers", :kind "def", :line 66, :end-line 75, :hash "1924692997"} {:id "defn-/lower-match-bindings", :kind "defn-", :line 77, :end-line 82, :hash "1676543692"} {:id "defn-/bind-union-pattern-locals", :kind "defn-", :line 84, :end-line 93, :hash "-732885566"} {:id "def/pattern-locals-handlers", :kind "def", :line 95, :end-line 102, :hash "-1926538894"} {:id "defn-/bind-pattern-locals", :kind "defn-", :line 104, :end-line 109, :hash "-1123562195"} {:id "defn-/lower-match-case", :kind "defn-", :line 111, :end-line 117, :hash "643897326"} {:id "defn/lower-match", :kind "defn", :line 119, :end-line 128, :hash "-31791995"} {:id "defn/infer-match-type", :kind "defn", :line 130, :end-line 147, :hash "-1349003900"}]}
;; clj-mutate-manifest-end
