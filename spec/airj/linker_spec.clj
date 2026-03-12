(ns airj.linker-spec
  (:require [airj.linker :as sut]
            [speclj.core :refer :all]))

(describe "linker"
  (it "attaches imported AIR-J interfaces through explicit compiler options"
    (let [module {:name 'example/use
                  :imports [{:op :airj-import
                             :module 'alpha/math
                             :symbols ['tick]}]
                  :exports []
                  :decls []}
          interfaces {'alpha/math {:name 'alpha/math
                                   :imports []
                                   :exports ['tick]
                                   :decls [{:op :fn
                                            :name 'tick
                                            :params [{:name 'x :type 'Int}]
                                            :return-type 'Int
                                            :effects ['Clock.Read]}]}}]
      (should= interfaces
               (:interfaces (sut/link-module module {:interfaces interfaces}))))))
