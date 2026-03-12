(ns airj.imported-interfaces-spec
  (:require [airj.imported-interfaces :as sut]
            [speclj.core :refer :all]))

(describe "imported-decls"
  (it "resolves imported declarations from supplied interfaces"
    (let [module {:name 'example/use-import
                  :imports [{:op :airj-import
                             :module 'alpha/math
                             :symbols ['add 'Counter]}]
                  :interfaces {'alpha/math {:name 'alpha/math
                                            :imports []
                                            :exports ['add 'Counter]
                                            :decls [{:op :fn
                                                     :name 'add
                                                     :params [{:name 'x :type 'Int}]
                                                     :return-type 'Int
                                                     :effects ['Clock.Read]}
                                                    {:op :data
                                                     :name 'Counter
                                                     :type-params []
                                                     :fields [{:name 'value
                                                               :type 'Int}]}]}}}]
      (should= {'add {:module 'alpha/math
                      :decl {:op :fn
                             :name 'add
                             :params [{:name 'x :type 'Int}]
                             :return-type 'Int
                             :effects ['Clock.Read]}}
                'Counter {:module 'alpha/math
                           :decl {:op :data
                                  :name 'Counter
                                  :type-params []
                                  :fields [{:name 'value
                                            :type 'Int}]}}}
               (sut/imported-decls module))))

  (it "rejects missing interfaces for imported modules"
    (should-throw clojure.lang.ExceptionInfo
                  "Missing AIR-J interface."
                  (sut/imported-decls {:name 'example/missing
                                       :imports [{:op :airj-import
                                                  :module 'alpha/math
                                                  :symbols ['add]}]
                                       :interfaces {}})))

  (it "rejects imported symbols not exported by the interface"
    (should-throw clojure.lang.ExceptionInfo
                  "Unresolved imported symbol."
                  (sut/imported-decls {:name 'example/missing-symbol
                                       :imports [{:op :airj-import
                                                  :module 'alpha/math
                                                  :symbols ['sub]}]
                                       :interfaces {'alpha/math {:name 'alpha/math
                                                                 :imports []
                                                                 :exports ['add]
                                                                 :decls [{:op :fn
                                                                          :name 'add
                                                                          :params []
                                                                          :return-type 'Int
                                                                          :effects []}]}}}))))
