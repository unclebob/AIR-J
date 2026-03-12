(ns airj.interface-sources-spec
  (:require [airj.interface-sources :as sut]
            [speclj.core :refer :all]))

(describe "interface-sources"
  (it "derives exported interfaces from AIR-J source text"
    (let [sources {'alpha/math "(module alpha/math
                                  (imports)
                                  (export tick)
                                  (fn tick
                                    (params (x Int))
                                    (returns Int)
                                    (effects (Clock.Read))
                                    (requires true)
                                    (ensures true)
                                    (local x)))"}]
      (should= {'alpha/math {:name 'alpha/math
                             :imports []
                             :exports ['tick]
                             :decls [{:op :fn
                                      :name 'tick
                                      :params [{:name 'x :type 'Int}]
                                      :return-type 'Int
                                      :effects ['Clock.Read]}]}}
               (sut/sources->interfaces sources)))))
