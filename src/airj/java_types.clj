;; mutation-tested: 2026-03-11
(ns airj.java-types)

(def ^:private primitive-types
  {'Bool Boolean/TYPE
   'Int Integer/TYPE
   'Unit Void/TYPE})

(defn load-class
  [class-name]
  (Class/forName (str class-name)))

(defn java-type-expr?
  [type-expr]
  (and (seq? type-expr)
       (= "Java" (name (first type-expr)))))

(defn resolve-type
  [type-expr]
  (cond
    (contains? primitive-types type-expr) (get primitive-types type-expr)
    (= 'String type-expr) java.lang.String
    (java-type-expr? type-expr) (load-class (second type-expr))
    :else nil))

(defn assignable-type-expr?
  [expected-type actual-type]
  (let [expected (resolve-type expected-type)
        actual (resolve-type actual-type)]
    (cond
      (or (nil? expected) (nil? actual)) (= expected-type actual-type)
      (or (.isPrimitive expected) (.isPrimitive actual)) (= expected actual)
      :else (.isAssignableFrom expected actual))))
