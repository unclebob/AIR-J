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
    (= 'StringSeq type-expr) (load-class "[Ljava.lang.String;")
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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T13:47:44.682392-05:00", :module-hash "1723544774", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-220394871"} {:id "def/primitive-types", :kind "def", :line 3, :end-line 6, :hash "-819992007"} {:id "defn/load-class", :kind "defn", :line 8, :end-line 10, :hash "1222057517"} {:id "defn/java-type-expr?", :kind "defn", :line 12, :end-line 15, :hash "-2087947263"} {:id "defn/resolve-type", :kind "defn", :line 17, :end-line 24, :hash "1752987975"} {:id "defn/assignable-type-expr?", :kind "defn", :line 26, :end-line 33, :hash "648140632"}]}
;; clj-mutate-manifest-end
