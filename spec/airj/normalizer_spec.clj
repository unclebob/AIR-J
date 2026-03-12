(ns airj.normalizer-spec
  (:require [airj.normalizer :as sut]
            [speclj.core :refer :all]))

(describe "normalize-module"
  (it "sorts imports exports and effect lists"
    (let [module {:name 'example/normalize
                  :imports [{:op :java-import
                             :class-name 'java.time.Instant}
                            {:op :airj-import
                             :module 'zeta/mod
                             :symbols ['z]}
                            {:op :airj-import
                             :module 'alpha/mod
                             :symbols ['a]}]
                  :exports ['zeta 'alpha]
                  :decls [{:op :fn
                           :name 'f
                           :params [{:name 'x :type 'Int}]
                           :return-type 'Int
                           :effects ['State.Write 'Clock.Read]
                           :requires [true]
                           :ensures [true]
                           :body {:op :local :name 'x}}]}
          normalized (sut/normalize-module module)]
      (should= {:name 'example/normalize
                :imports [{:op :airj-import
                           :module 'alpha/mod
                           :symbols ['a]}
                          {:op :java-import
                           :class-name 'java.time.Instant}
                          {:op :airj-import
                           :module 'zeta/mod
                           :symbols ['z]}
                          ]
                :exports ['alpha 'zeta]
                :decls [{:op :fn
                         :name 'f
                         :params [{:name 'x :type 'Int}]
                         :return-type 'Int
                         :effects ['Clock.Read 'State.Write]
                         :requires [true]
                         :ensures [true]
                         :body {:op :local :name 'x}}]}
               normalized))))

  (it "orders declarations by types before functions"
    (let [module {:name 'example/ordering
                  :imports []
                  :exports []
                  :decls [{:op :fn
                           :name 'run
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body 0}
                          {:op :union
                           :name 'Result
                           :type-params []
                           :invariants [true]
                           :variants []}
                          {:op :data
                           :name 'State
                           :type-params []
                           :invariants [true]
                           :fields []}
                          {:op :enum
                           :name 'Mode
                            :variants ['Ready]}]}
          normalized (sut/normalize-module module)]
      (should= [:data :enum :union :fn]
               (mapv :op (:decls normalized)))))

  (it "orders same-kind declarations by name"
    (let [module {:name 'example/names
                  :imports []
                  :exports []
                  :decls [{:op :enum :name 'Zed :variants []}
                          {:op :data :name 'Beta :type-params [] :invariants [] :fields []}
                          {:op :data :name 'Alpha :type-params [] :invariants [] :fields []}
                          {:op :enum :name 'Able :variants []}]}
          normalized (sut/normalize-module module)]
      (should= ['Alpha 'Beta 'Able 'Zed]
               (mapv :name (:decls normalized)))))
