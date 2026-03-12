(ns example.int-stack)

(defrecord Stack [items])

(defn empty-stack []
  (->Stack '()))

(defn is-empty-stack [stack]
  (empty? (:items stack)))

(defn push [stack value]
  (->Stack (cons value (:items stack))))

(defn peek [stack]
  (if (empty? (:items stack))
    [:err "empty-stack"]
    [:ok (first (:items stack))]))

(defn pop [stack]
  (if (empty? (:items stack))
    [:empty]
    [:popped (first (:items stack)) (->Stack (rest (:items stack)))]))
