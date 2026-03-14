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
    (should (sut/assignable-type-expr? '(Seq String)
                                       '(Seq String)))
    (should (sut/assignable-type-expr? '(Java java.util.Map)
                                       '(Map String Int)))
    (should-not (sut/assignable-type-expr? '(Java java.lang.StringBuilder)
                                           '(Java java.lang.Object))))

  (it "keeps primitive compatibility exact"
    (should (sut/assignable-type-expr? 'Int 'Int))
    (should= (Class/forName "[B")
             (sut/resolve-type 'Bytes))
    (should= java.util.List
             (sut/resolve-type '(Seq String)))
    (should= java.util.Map
             (sut/resolve-type '(Map String Int)))
    (should-not (sut/assignable-type-expr? '(Java java.lang.Object) 'Int))))
