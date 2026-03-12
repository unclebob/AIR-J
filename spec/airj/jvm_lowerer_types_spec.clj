(ns airj.jvm-lowerer-types-spec
  (:require [airj.jvm-lowerer-types :as sut]
            [speclj.core :refer :all]))

(describe "JVM lowerer types"
  (it "lowers AIR-J and declared JVM types"
    (let [ctx {:module-name 'example/types
               :decls {'Result {:op :union
                                :name 'Result
                                :variants []}}}]
      (should= :int
               (sut/lower-type 'Int ctx))
      (should= "java/lang/String"
               (sut/lower-type 'String ctx))
      (should= "java/lang/StringBuilder"
               (sut/lower-type '(Java java.lang.StringBuilder) ctx))
      (should= "example/types$Result"
               (sut/lower-type 'Result ctx))))

  (it "infers call types from imported function declarations"
    (let [ctx {:module-name 'example/use
               :decls {}
               :imported-decls {'tick {:module 'alpha/math
                                       :decl {:op :fn
                                              :name 'tick
                                              :params [{:name 'x :type 'Int}]
                                              :return-type 'Int
                                              :effects ['Clock.Read]}}}
               :locals {}}
          expr {:op :call
                :callee {:op :local :name 'tick}
                :args [1]}]
      (should= 'Int
               (sut/infer-type expr ctx)))))
