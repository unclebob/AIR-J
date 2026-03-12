(ns airj.resolver-spec
  (:require [airj.resolver :as sut]
            [speclj.core :refer :all]))

(describe "resolve-module"
  (it "accepts modules with valid exports and local references"
    (let [module {:name 'example/resolve
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'state :type 'State}]
                           :return-type 'Result
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :loop
                                  :bindings [{:name 'current
                                              :expr {:op :local :name 'state}}]
                                  :body {:op :match
                                         :target {:op :local :name 'current}
                                         :cases [{:pattern {:op :union-pattern
                                                            :name 'Running
                                                            :args [{:op :binder-pattern
                                                                    :name 'next}]}
                                                  :body {:op :recur
                                                         :args [{:op :local :name 'next}]}}
                                                 {:pattern {:op :wildcard-pattern}
                                                  :body {:op :local :name 'current}}]}}}]}
          resolved (sut/resolve-module module)]
      (should= module resolved)))

  (it "rejects exports that are not declared"
    (should-throw clojure.lang.ExceptionInfo
                  "Unresolved export."
                  (sut/resolve-module {:name 'example/bad-export
                                       :imports []
                                       :exports ['missing]
                                       :decls []})))

  (it "rejects unresolved local references"
    (should-throw clojure.lang.ExceptionInfo
                  "Unresolved local."
                  (sut/resolve-module {:name 'example/bad-local
                                       :imports []
                                       :exports ['program]
                                       :decls [{:op :fn
                                                :name 'program
                                                :params []
                                                :return-type 'Int
                                                :effects []
                                                :requires [true]
                                                :ensures [true]
                                                :body {:op :local :name 'missing}}]})))

  (it "accepts references to same-module functions"
    (let [module {:name 'example/direct-call
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'tick
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body 1}
                          {:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                          :body {:op :call
                                  :callee {:op :local :name 'tick}
                                  :args []}}]}]
      (should= module (sut/resolve-module module))))

  (it "accepts references to imported functions from supplied interfaces"
    (let [module {:name 'example/imported-call
                  :imports [{:op :airj-import
                             :module 'alpha/math
                             :symbols ['tick]}]
                  :interfaces {'alpha/math {:name 'alpha/math
                                            :imports []
                                            :exports ['tick]
                                            :decls [{:op :fn
                                                     :name 'tick
                                                     :params []
                                                     :return-type 'Int
                                                     :effects []}]}}
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :call
                                  :callee {:op :local :name 'tick}
                                  :args []}}]}]
      (should= module (sut/resolve-module module))))

  (it "accepts mutable locals referenced later in a seq and nested lambda"
    (let [body {:op :seq
                :exprs [{:op :var
                         :name 'base
                         :type 'Int
                         :init 5}
                        {:op :let
                         :bindings [{:name 'f
                                     :expr {:op :lambda
                                            :params [{:name 'y :type 'Int}]
                                            :return-type 'Int
                                            :effects ['State.Write]
                                            :body {:op :local
                                                   :name 'base}}}]
                         :body {:op :seq
                                :exprs [{:op :set
                                         :name 'base
                                         :expr 9}
                                        {:op :call
                                         :callee {:op :local
                                                  :name 'f}
                                         :args [0]}]}}]}
          module {:name 'example/mutable-capture
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body body}]}]
      (should= module (sut/resolve-module module)))))

  (it "rejects duplicate imported AIR-J symbols"
    (should-throw clojure.lang.ExceptionInfo
                  "Duplicate imported symbol."
                  (sut/resolve-module {:name 'example/duplicate-import
                                       :imports [{:op :airj-import
                                                  :module 'alpha/math
                                                  :symbols ['add]}
                                                 {:op :airj-import
                                                  :module 'beta/math
                                                  :symbols ['add]}]
                                       :exports []
                                       :decls []})))

  (it "rejects imported symbols that conflict with local declarations"
    (should-throw clojure.lang.ExceptionInfo
                  "Imported symbol conflicts with local declaration."
                  (sut/resolve-module {:name 'example/import-conflict
                                       :imports [{:op :airj-import
                                                  :module 'alpha/math
                                                  :symbols ['program]}]
                                       :exports ['program]
                                       :decls [{:op :fn
                                                :name 'program
                                                :params []
                                                :return-type 'Int
                                                :effects []
                                                :requires [true]
                                                :ensures [true]
                                                :body 0}]})))
