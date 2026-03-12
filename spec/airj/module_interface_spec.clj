(ns airj.module-interface-spec
  (:require [airj.module-interface :as sut]
            [speclj.core :refer :all]))

(describe "extract-interface"
  (it "keeps exported declarations and trims unexported ones"
    (let [module {:name 'example/interface
                  :imports [{:op :airj-import
                             :module 'alpha/math
                             :symbols ['add]}]
                  :exports ['Stack 'Result 'main]
                  :decls [{:op :data
                           :name 'Stack
                           :type-params []
                           :invariants [{:op :call
                                         :callee {:op :local :name 'valid-stack?}
                                         :args []}]
                           :fields [{:name 'items
                                     :type '(List Int)}]}
                          {:op :union
                           :name 'Result
                           :type-params []
                           :invariants [true]
                           :variants [{:name 'Ok
                                       :fields [{:name 'value
                                                 :type 'Int}]}
                                      {:name 'Err
                                       :fields []}]}
                          {:op :fn
                           :name 'main
                           :params [{:name 'args
                                     :type '(Java "[Ljava.lang.String;")}]
                           :return-type 'Int
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body 0}
                          {:op :fn
                           :name 'helper
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body 1}]}]
      (should= {:name 'example/interface
                :imports [{:op :airj-import
                           :module 'alpha/math
                           :symbols ['add]}]
                :exports ['Stack 'Result 'main]
                :decls [{:op :data
                         :name 'Stack
                         :type-params []
                         :invariants [{:op :call
                                       :callee {:op :local
                                                :name 'valid-stack?}
                                       :args []}]
                         :fields [{:name 'items
                                   :type '(List Int)}]}
                        {:op :union
                         :name 'Result
                         :type-params []
                         :invariants [true]
                         :variants [{:name 'Ok
                                     :fields [{:name 'value
                                               :type 'Int}]}
                                    {:name 'Err
                                     :fields []}]}
                        {:op :fn
                         :name 'main
                         :params [{:name 'args
                                   :type '(Java "[Ljava.lang.String;")}]
                         :return-type 'Int
                         :effects ['Foreign.Throw]}]}
               (sut/extract-interface module)))))
