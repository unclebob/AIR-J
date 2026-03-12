(ns airj.jvm-closures-spec
  (:require [airj.jvm-closures :as sut]
            [speclj.core :refer :all]))

(describe "free-locals"
  (it "collects free locals in first-use order"
    (should= ['base 'y]
             (sut/free-locals
              {:op :if
               :test {:op :local :name 'base}
               :then {:op :call
                      :callee {:op :local :name 'f}
                      :args [{:op :local :name 'y}
                             {:op :local :name 'base}]}
               :else 0}
              #{'f})))

  (it "excludes names bound by let"
    (should= ['outside]
             (sut/free-locals
              {:op :let
               :bindings [{:name 'inside
                           :expr {:op :local :name 'outside}}]
               :body {:op :if
                      :test true
                      :then {:op :local :name 'inside}
                      :else 0}}
              #{})))

  (it "does not descend into nested lambdas"
    (should= []
             (sut/free-locals
              {:op :lambda
               :params [{:name 'x :type 'Int}]
               :return-type 'Int
               :effects []
               :body {:op :local :name 'outside}}
              #{}))))
