(ns airj.java-types-spec
  (:require [airj.java-types :as sut]
            [speclj.core :refer :all]))

(describe "java type mapping"
  (it "recognizes Java type expressions by head symbol name"
    (should (sut/java-type-expr? '(Java java.lang.String)))
    (should (sut/java-type-expr? '(airj.type-checker/Java java.lang.String)))
    (should-not (sut/java-type-expr? 'String)))

  (it "allows reference assignability for Java interop types"
    (should (sut/assignable-type-expr? '(Java java.lang.Object)
                                       '(Java java.lang.StringBuilder)))
    (should-not (sut/assignable-type-expr? '(Java java.lang.StringBuilder)
                                           '(Java java.lang.Object))))

  (it "keeps primitive compatibility exact"
    (should (sut/assignable-type-expr? 'Int 'Int))
    (should-not (sut/assignable-type-expr? '(Java java.lang.Object) 'Int))))
