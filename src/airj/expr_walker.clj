;; mutation-tested: 2026-03-11
(ns airj.expr-walker)

(defn walk-expr
  [expr ctx handlers]
  (when (map? expr)
    (when-let [handler (get handlers (:op expr))]
      (handler expr
               ctx
               (fn
                 ([child]
                  (walk-expr child ctx handlers))
                 ([child child-ctx]
                  (walk-expr child child-ctx handlers)))))))
