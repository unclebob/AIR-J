(ns airj.java-effects-spec
  (:require [airj.java-effects :as sut]
            [speclj.core :refer :all]))

(describe "java interop effect policy"
  (it "treats known pure static members as effect-free"
    (should= #{}
             (sut/member-effects
              {:kind :method
               :class-name 'java.lang.Math
               :member-id 'abs
               :static? true
               :parameter-types [Integer/TYPE]
               :return-type Integer/TYPE})))

  (it "treats known pure instance members as effect-free"
    (should= #{}
             (sut/member-effects
              {:kind :method
               :class-name 'java.lang.StringBuilder
               :member-id 'length
               :static? false
               :parameter-types []
               :return-type Integer/TYPE})))

  (it "uses conservative defaults for unknown foreign calls"
    (should= #{'Foreign.Throw}
             (sut/member-effects
              {:kind :method
               :class-name 'java.lang.Integer
               :member-id 'valueOf
               :static? true
               :parameter-types [java.lang.String]
               :return-type java.lang.Integer})))

  (it "treats field writes as state writes"
    (should= #{'State.Write}
             (sut/interop-effects
              {:op :java-set-field
               :field-name 'x
               :field-type 'Int}
              {:kind :field
               :class-name 'java.awt.Point
               :field-name 'x
               :static? false
               :field-type Integer/TYPE})))

  (it "treats constructors and unknown instance calls conservatively"
    (should= #{'Foreign.Throw}
             (sut/interop-effects
              {:op :java-new}
              {:kind :constructor}))
    (should= #{'Foreign.Throw}
             (sut/interop-effects
              {:op :java-call}
              {:kind :method
               :class-name 'java.lang.Integer
               :member-id 'valueOf
               :static? false
               :parameter-types [java.lang.String]
               :return-type java.lang.Integer})))

  (it "treats field reads and unknown expressions as effect-free"
    (should= #{}
             (sut/interop-effects
              {:op :java-get-field}
              {:kind :field
               :class-name 'java.awt.Point
               :field-name 'x
               :static? false
               :field-type Integer/TYPE}))
    (should= #{}
             (sut/interop-effects
              {:op :bogus}
              {:kind :method}))))
