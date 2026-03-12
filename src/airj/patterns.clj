;; mutation-tested: 2026-03-11
(ns airj.patterns)

(declare bind-pattern)

(defn decl-for-type
  [decls type-expr]
  (get decls type-expr))

(defn field-type
  [decl field-name]
  (some (fn [field]
          (when (= field-name (:name field))
            (:type field)))
        (:fields decl)))

(defn union-variant
  [decl variant-name]
  (some #(when (= (:name %) variant-name) %) (:variants decl)))

(defn enum-variant?
  [decl variant-name]
  (boolean (some #{variant-name} (:variants decl))))

(defn enum-pattern?
  [pattern target-type decls]
  (let [decl (decl-for-type decls target-type)]
    (and (= :binder-pattern (:op pattern))
         (= :enum (:op decl))
         (enum-variant? decl (:name pattern)))))

(defn- options
  [bind-binder bind-literal fail!]
  {:bind-binder bind-binder
   :bind-literal bind-literal
   :fail! fail!})

(defn- bind-union-pattern
  [ctx pattern target-type decls opts]
  (let [decl (decl-for-type decls target-type)]
    (when-not (= :union (:op decl))
      ((:fail! opts) "Expected union type."
                    {:pattern pattern
                     :type target-type}))
    (let [variant (union-variant decl (:name pattern))
          expected-count (count (:fields variant))
          actual-count (count (:args pattern))]
      (when-not variant
        ((:fail! opts) "Unknown variant."
                      {:pattern pattern
                       :type target-type}))
      (when-not (= expected-count actual-count)
        ((:fail! opts) "Arity mismatch."
                      {:pattern pattern
                       :type target-type
                       :expected expected-count
                       :actual actual-count}))
      (reduce (fn [acc [field nested-pattern]]
                (bind-pattern acc
                              nested-pattern
                              (:type field)
                              decls
                              opts))
              ctx
              (map vector (:fields variant) (:args pattern))))))

(defn- bind-record-field
  [ctx record-decl field-pattern decls opts target-type]
  (let [matched-type (field-type record-decl (:name field-pattern))]
    (when-not matched-type
      ((:fail! opts) "Unknown field."
                    {:type target-type
                     :field (:name field-pattern)}))
    (bind-pattern ctx
                  (:pattern field-pattern)
                  matched-type
                  decls
                  opts)))

(defn- bind-record-pattern
  [ctx pattern target-type decls opts]
  (when-not (= (:type pattern) target-type)
    ((:fail! opts) "Type mismatch."
                  {:pattern pattern
                   :expected target-type
                   :actual (:type pattern)}))
  (let [record-decl (decl-for-type decls target-type)]
    (when-not (= :data (:op record-decl))
      ((:fail! opts) "Expected record type."
                    {:pattern pattern
                     :type target-type}))
    (reduce (fn [acc field-pattern]
              (bind-record-field acc record-decl field-pattern decls opts target-type))
            ctx
            (:fields pattern))))

(defn bind-pattern
  [ctx pattern target-type decls {:keys [bind-binder bind-literal fail!]}]
  (let [opts (options bind-binder bind-literal fail!)]
    (case (:op pattern)
      :wildcard-pattern ctx
      :literal-pattern (bind-literal ctx pattern target-type)
      :binder-pattern (bind-binder ctx pattern target-type decls)
      :union-pattern (bind-union-pattern ctx pattern target-type decls opts)
      :record-pattern (bind-record-pattern ctx pattern target-type decls opts)
      (fail! "Unsupported pattern."
             {:pattern pattern}))))
