(ns example.int-stack-contracts)

(defrecord Stack [items])

(defn stack? [value]
  (instance? Stack value))

(defn empty-stack []
  {:post [(stack? %)
          (empty? (:items %))]}
  (->Stack '()))

(defn is-empty-stack [stack]
  {:pre [(stack? stack)]
   :post [(boolean? %)
          (= % (empty? (:items stack)))]}
  (empty? (:items stack)))

(defn size [stack]
  {:pre [(stack? stack)]
   :post [(int? %)
          (= % (count (:items stack)))]}
  (count (:items stack)))

(defn push [stack value]
  {:pre [(stack? stack)
         (int? value)]
   :post [(stack? %)
          (= (size %) (inc (size stack)))
          (= (first (:items %)) value)]}
  (->Stack (cons value (:items stack))))

(defn peek [stack]
  {:pre [(stack? stack)]
   :post [(or (= % [:err "empty-stack"])
              (= % [:ok (first (:items stack))]))]}
  (if (empty? (:items stack))
    [:err "empty-stack"]
    [:ok (first (:items stack))]))

(defn pop [stack]
  {:pre [(stack? stack)]
   :post [(if (empty? (:items stack))
            (= % [:empty])
            (and (= (first %) :popped)
                 (= (second %) (first (:items stack)))
                 (stack? (nth % 2))
                 (= (size (nth % 2)) (dec (size stack)))))]}
  (if (empty? (:items stack))
    [:empty]
    [:popped (first (:items stack)) (->Stack (rest (:items stack)))]))
