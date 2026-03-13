(ns airj.effect-checker-spec
  (:require [airj.effect-checker :as sut]
            [speclj.core :refer :all]))

(describe "check-module"
  (it "accepts modules whose function bodies stay within declared effects"
    (let [module {:name 'example/effects
                  :imports []
                  :exports ['bump]
                  :decls [{:op :fn
                           :name 'bump
                           :params []
                           :return-type 'Int
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :var
                                           :name 'counter
                                           :type 'Int
                                           :init 0}
                                          {:op :set
                                           :name 'counter
                                           :expr 1}
                                          1]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects undeclared body effects"
    (should-throw clojure.lang.ExceptionInfo
                  "Undeclared effects."
                  (sut/check-module
                   {:name 'example/missing-effect
                    :imports []
                    :exports ['bump]
                    :decls [{:op :fn
                             :name 'bump
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :var
                                    :name 'counter
                                    :type 'Int
                                    :init 0}}]})))

  (it "rejects effectful contracts"
    (should-throw clojure.lang.ExceptionInfo
                  "Contracts must be pure."
                  (sut/check-module
                   {:name 'example/contract-effect
                    :imports []
                    :exports ['bump]
                    :decls [{:op :fn
                             :name 'bump
                             :params []
                             :return-type 'Int
                             :effects ['State.Write]
                             :requires [{:op :var
                                         :name 'counter
                                         :type 'Int
                                         :init 0}]
                             :ensures [true]
                             :body 1}]})))

  (it "rejects effectful invariants"
    (should-throw clojure.lang.ExceptionInfo
                  "Invariants must be pure."
                  (sut/check-module
                   {:name 'example/invariant-effect
                    :imports []
                    :exports []
                    :decls [{:op :data
                             :name 'Response
                             :type-params []
                             :invariants [{:op :var
                                           :name 'counter
                                           :type 'Int
                                           :init 0}]
                             :fields [{:name 'status :type 'Int}]}]})))

  (it "uses declared callee effects for direct same-module calls"
    (let [module {:name 'example/call-effects
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'tick
                           :params []
                           :return-type 'Int
                           :effects ['Clock.Read]
                           :requires [true]
                           :ensures [true]
                           :body 1}
                          {:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects ['Clock.Read]
                           :requires [true]
                           :ensures [true]
                           :body {:op :call
                                  :callee {:op :local :name 'tick}
                                  :args []}}]}]
      (should= module (sut/check-module module))))

  (it "uses imported function effects from supplied interfaces"
    (let [module {:name 'example/imported-effects
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
                                                     :effects ['Clock.Read]}]}}
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects ['Clock.Read]
                           :requires [true]
                           :ensures [true]
                           :body {:op :call
                                  :callee {:op :local :name 'tick}
                                  :args []}}]}]
      (should= module (sut/check-module module))))

  (it "accepts Java interop over function parameters"
    (let [module {:name 'example/java-param
                  :imports [{:op :java-import
                             :class-name 'java.lang.StringBuilder}]
                  :exports ['helper]
                  :decls [{:op :fn
                           :name 'helper
                           :params [{:name 'acc
                                     :type 'String}]
                           :return-type 'String
                           :effects ['Foreign.Throw 'State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-call
                                  :target {:op :java-new
                                           :class-name 'java.lang.StringBuilder
                                           :type-args []
                                           :args [{:op :local
                                                   :name 'acc}]}
                                  :member-id 'toString
                                  :signature {:params []
                                              :return-type 'String}
                                  :args []}}]}]
      (should= module (sut/check-module module))))

  (it "accepts Java interop over loop-bound locals"
    (let [module {:name 'example/java-loop
                  :imports [{:op :java-import
                             :class-name 'java.lang.Math}]
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :loop
                                  :bindings [{:name 'n
                                              :expr 1}]
                                  :body {:op :java-static-call
                                         :class-name 'java.lang.Math
                                         :member-id 'abs
                                         :signature {:params ['Int]
                                                     :return-type 'Int}
                                         :args [{:op :local
                                                 :name 'n}]}}}]}]
      (should= module (sut/check-module module))))

  (it "accepts loop bindings that call same-module functions"
    (let [module {:name 'example/fn-loop
                  :imports []
                  :exports ['main]
                  :decls [{:op :fn
                           :name 'arg0
                           :params [{:name 'args
                                     :type '(Java "[Ljava.lang.String;")}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body 1}
                          {:op :fn
                           :name 'main
                           :params [{:name 'args
                                     :type '(Java "[Ljava.lang.String;")}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :loop
                                  :bindings [{:name 'n
                                              :expr {:op :call
                                                     :callee {:op :local
                                                              :name 'arg0}
                                                     :args [{:op :local
                                                             :name 'args}]}}]
                                  :body {:op :local
                                         :name 'n}}}]}]
      (should= module (sut/check-module module))))

  (it "accepts static Java field access and printing effects"
    (let [module {:name 'example/java-static-field
                  :imports [{:op :java-import
                             :class-name 'java.lang.System}
                            {:op :java-import
                             :class-name 'java.io.PrintStream}]
                  :exports ['main]
                  :decls [{:op :fn
                           :name 'main
                           :params []
                           :return-type 'Unit
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-call
                                  :target {:op :java-static-get-field
                                           :class-name 'java.lang.System
                                           :field-name 'out
                                           :field-type '(Java java.io.PrintStream)}
                                  :member-id 'println
                                  :signature {:params ['String]
                                              :return-type 'Unit}
                                  :args ["hello"]}}]}]
      (should= module (sut/check-module module))))

  (it "treats primitive operators as pure while including operand effects"
    (let [module {:name 'example/operator-effects
                  :imports [{:op :java-import
                             :class-name 'java.lang.Math}]
                  :exports ['compute]
                  :decls [{:op :fn
                           :name 'compute
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Int
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :int-add
                                  :args [{:op :java-static-call
                                          :class-name 'java.lang.Math
                                          :member-id 'abs
                                          :signature {:params ['Int]
                                                      :return-type 'Int}
                                          :args [{:op :local :name 'value}]}
                                         1]}}]}]
      (should= module (sut/check-module module))))

  (it "treats comparison operators and boolean equality as pure"
    (let [module {:name 'example/compare-effects
                  :imports [{:op :java-import
                             :class-name 'java.lang.Math}]
                  :exports ['compare]
                  :decls [{:op :fn
                           :name 'compare
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Bool
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :bool-eq
                                  :args [{:op :int-ge
                                          :args [{:op :java-static-call
                                                  :class-name 'java.lang.Math
                                                  :member-id 'abs
                                                  :signature {:params ['Int]
                                                              :return-type 'Int}
                                                  :args [{:op :local :name 'value}]}
                                                 1]}
                                         {:op :int-le
                                          :args [{:op :local :name 'value}
                                                 10]}]}}]}]
      (should= module (sut/check-module module)))))

(describe "expr-effects"
  (it "treats lambda creation as pure but checks the lambda body against declared effects"
    (should= #{}
             (sut/expr-effects
              {:op :lambda
               :params []
               :return-type 'Int
               :effects ['State.Write]
               :body {:op :var
                      :name 'counter
                      :type 'Int
                      :init 0}}
              {})))

  (it "accumulates effects from try body catches and finally"
    (should= #{'State.Write 'Foreign.Throw}
             (sut/expr-effects
              {:op :try
               :body {:op :var
                      :name 'counter
                      :type 'Int
                      :init 0}
               :catches [{:type 'Error
                          :name 'ex
                          :body {:op :raise
                                 :expr "boom"}}]
               :finally {:op :set
                         :name 'counter
                         :expr 1}}
              {})))

  (it "rejects unsupported dynamic call effect inference"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported call effect inference."
                  (sut/expr-effects
                   {:op :call
                    :callee {:op :local :name 'f}
                    :args []}
                   {})))

  (it "uses let-bound lambda effects for direct local calls"
    (should= #{'Clock.Read}
             (sut/expr-effects
              {:op :let
               :bindings [{:name 'tick
                           :expr {:op :lambda
                                  :params []
                                  :return-type 'Int
                                  :effects ['Clock.Read]
                                  :body 1}}]
               :body {:op :call
                      :callee {:op :local :name 'tick}
                      :args []}}
              {}))))

  (it "uses matching branch effects for local calls on lambda-valued if expressions"
    (should= #{'Clock.Read}
             (sut/expr-effects
              {:op :let
               :bindings [{:name 'tick
                           :expr {:op :if
                                  :test true
                                  :then {:op :lambda
                                         :params []
                                         :return-type 'Int
                                         :effects ['Clock.Read]
                                         :body 1}
                                  :else {:op :lambda
                                         :params []
                                         :return-type 'Int
                                         :effects ['Clock.Read]
                                         :body 2}}}]
               :body {:op :call
                      :callee {:op :local :name 'tick}
                      :args []}}
              {})))

  (it "treats known pure Java interop members as effect-free"
    (should= #{}
             (sut/expr-effects
              {:op :seq
               :exprs [{:op :java-static-call
                        :class-name 'java.lang.Math
                        :member-id 'abs
                        :signature {:params ['Int]
                                    :return-type 'Int}
                        :args [1]}
                       {:op :java-call
                        :target {:op :java-new
                                 :class-name 'java.lang.StringBuilder
                                 :type-args []
                                 :args ["abc"]}
                        :member-id 'length
                        :signature {:params []
                                    :return-type 'Int}
                        :args []}]}
              {})))

  (it "uses conservative foreign throw effects for unknown Java calls"
    (should= #{'Foreign.Throw}
             (sut/expr-effects
              {:op :java-static-call
               :class-name 'java.lang.Integer
               :member-id 'valueOf
               :signature {:params ['String]
                           :return-type '(Java java.lang.Integer)}
               :args ["42"]}
              {})))
