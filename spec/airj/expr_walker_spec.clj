(ns airj.expr-walker-spec
  (:require [airj.expr-walker :as sut]
            [speclj.core :refer :all]))

(describe "expression walker"
  (it "ignores non-expression values"
    (should-be-nil (sut/walk-expr 7 {} {})))

  (it "dispatches to handlers and recurses with explicit child context"
    (let [visited (atom [])
          handlers {:seq (fn [expr ctx walk]
                           (swap! visited conj [(:op expr) ctx])
                           (doseq [part (:exprs expr)]
                             (walk part (update ctx :depth inc))))
                  :local (fn [expr ctx _walk]
                           (swap! visited conj [(:name expr) ctx]))}]
      (sut/walk-expr {:op :seq
                      :exprs [{:op :local :name 'x}
                              {:op :local :name 'y}]}
                     {:depth 0}
                     handlers)
      (should= [[:seq {:depth 0}]
                ['x {:depth 1}]
                ['y {:depth 1}]]
               @visited))))
