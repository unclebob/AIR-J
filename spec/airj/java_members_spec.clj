(ns airj.java-members-spec
  (:require [airj.java-members :as sut]
            [speclj.core :refer :all]))

(describe "java member resolution"
  (it "resolves static methods to metadata"
    (let [meta (sut/resolve-static-method 'java.lang.Math
                                          'abs
                                          {:params ['Int]
                                           :return-type 'Int})]
      (should= {:kind :method
                :class-name 'java.lang.Math
                :member-id 'abs
                :static? true
                :parameter-types [Integer/TYPE]
                :return-type Integer/TYPE}
               (select-keys meta [:kind :class-name :member-id :static? :parameter-types :return-type]))))

  (it "resolves instance methods to metadata"
    (let [meta (sut/resolve-instance-method '(Java java.lang.StringBuilder)
                                            'length
                                            {:params []
                                             :return-type 'Int})]
      (should= {:kind :method
                :class-name 'java.lang.StringBuilder
                :member-id 'length
                :static? false
                :parameter-types []
                :return-type Integer/TYPE}
               (select-keys meta [:kind :class-name :member-id :static? :parameter-types :return-type]))))

  (it "resolves fields to metadata"
    (let [meta (sut/resolve-instance-field '(Java java.awt.Point)
                                           'x
                                           'Int)]
      (should= {:kind :field
                :class-name 'java.awt.Point
                :field-name 'x
                :static? false
                :field-type Integer/TYPE}
               (select-keys meta [:kind :class-name :field-name :static? :field-type])))))
