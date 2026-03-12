(ns airj.type-checker-spec
  (:require [airj.type-checker :as sut]
            [speclj.core :refer :all]))

(describe "check-module"
  (it "accepts a well-typed module"
    (let [module {:name 'example/types
                  :imports []
                  :exports ['status]
                  :decls [{:op :data
                           :name 'Response
                           :type-params []
                           :invariants []
                           :fields [{:name 'status :type 'Int}
                                    {:name 'body :type 'String}]}
                          {:op :fn
                           :name 'status
                           :params [{:name 'response :type 'Response}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :if
                                  :test true
                                  :then {:op :record-get
                                         :target {:op :local :name 'response}
                                         :field 'status}
                                  :else 0}}]}]
      (should= module (sut/check-module module))))

  (it "rejects mismatched function return types"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-return
                    :imports []
                    :exports ['status]
                    :decls [{:op :fn
                             :name 'status
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body "nope"}]})))

  (it "rejects if expressions with non-boolean tests"
    (should-throw clojure.lang.ExceptionInfo
                  "Expected Bool."
                  (sut/check-module
                   {:name 'example/bad-if
                    :imports []
                    :exports ['f]
                    :decls [{:op :fn
                             :name 'f
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :if
                                    :test 1
                                    :then 2
                                    :else 3}}]})))

  (it "rejects record-get on unknown fields"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown field."
                  (sut/check-module
                   {:name 'example/bad-field
                    :imports []
                    :exports ['f]
                    :decls [{:op :data
                             :name 'Response
                             :type-params []
                             :invariants []
                             :fields [{:name 'status :type 'Int}]}
                            {:op :fn
                             :name 'f
                             :params [{:name 'response :type 'Response}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :record-get
                                    :target {:op :local :name 'response}
                                    :field 'missing}}]})))

  (it "rejects locals without declared types"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown local type."
                  (sut/check-module
                   {:name 'example/bad-local
                    :imports []
                    :exports ['f]
                    :decls [{:op :fn
                             :name 'f
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :local :name 'missing}}]})))

  (it "rejects record-get on non-record values"
    (should-throw clojure.lang.ExceptionInfo
                  "Expected record type."
                  (sut/check-module
                   {:name 'example/non-record
                    :imports []
                    :exports ['f]
                    :decls [{:op :fn
                             :name 'f
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :record-get
                                    :target 1
                                    :field 'status}}]})))

  (it "rejects construction of unknown types"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown constructed type."
                  (sut/check-module
                   {:name 'example/bad-construct
                    :imports []
                    :exports ['f]
                    :decls [{:op :fn
                             :name 'f
                             :params []
                             :return-type 'Missing
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :construct
                                    :type 'Missing
                                    :args []}}]})))

  (it "rejects non-boolean contracts"
    (should-throw clojure.lang.ExceptionInfo
                  "Expected Bool."
                  (sut/check-module
                   {:name 'example/bad-contract
                    :imports []
                    :exports ['f]
                    :decls [{:op :fn
                             :name 'f
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [1]
                             :ensures [true]
                             :body 1}]})))

  (it "accepts direct same-module function calls"
    (let [module {:name 'example/direct-call
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'tick
                           :params [{:name 'x :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :local :name 'x}}
                          {:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                            :body {:op :call
                                   :callee {:op :local :name 'tick}
                                   :args [1]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts calls to imported functions from supplied interfaces"
    (let [module {:name 'example/imported-call
                  :imports [{:op :airj-import
                             :module 'alpha/math
                             :symbols ['tick]}]
                  :interfaces {'alpha/math {:name 'alpha/math
                                            :imports []
                                            :exports ['tick]
                                            :decls [{:op :fn
                                                     :name 'tick
                                                     :params [{:name 'x :type 'Int}]
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
                                  :args [1]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects calls with wrong arity"
    (should-throw clojure.lang.ExceptionInfo
                  "Arity mismatch."
                  (sut/check-module
                   {:name 'example/bad-call-arity
                    :imports []
                    :exports ['program]
                    :decls [{:op :fn
                             :name 'tick
                             :params [{:name 'x :type 'Int}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :local :name 'x}}
                            {:op :fn
                             :name 'program
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :call
                                    :callee {:op :local :name 'tick}
                                    :args []}}]})))

  (it "rejects calls on non-function values"
    (should-throw clojure.lang.ExceptionInfo
                  "Expected function type."
                  (sut/check-module
                   {:name 'example/non-callable
                    :imports []
                    :exports ['program]
                    :decls [{:op :fn
                             :name 'program
                             :params [{:name 'x :type 'Int}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :call
                                    :callee {:op :local :name 'x}
                                    :args [1]}}]})))

  (it "accepts let seq var set and try expressions"
    (let [body {:op :let
                :bindings [{:name 'f
                            :expr {:op :lambda
                                   :params [{:name 'y :type 'Int}]
                                   :return-type 'Int
                                   :effects []
                                   :body {:op :local :name 'y}}}]
                :body {:op :seq
                       :exprs [{:op :var
                                :name 'counter
                                :type 'Int
                                :init 0}
                               {:op :set
                                :name 'counter
                                :expr {:op :call
                                       :callee {:op :local :name 'f}
                                       :args [1]}}
                               {:op :try
                                :body {:op :local :name 'counter}
                                :catches [{:type 'Error
                                           :name 'ex
                                           :body 2}]
                                :finally 3}]}}
          module {:name 'example/stateful
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
      (should= module (sut/check-module module))))

  (it "rejects set on immutable bindings"
    (should-throw clojure.lang.ExceptionInfo
                  "Expected mutable binding."
                  (sut/check-module
                   {:name 'example/immutable-set
                    :imports []
                    :exports ['program]
                    :decls [{:op :fn
                             :name 'program
                             :params [{:name 'x :type 'Int}]
                             :return-type 'Unit
                             :effects ['State.Write]
                             :requires [true]
                             :ensures [true]
                             :body {:op :set
                                    :name 'x
                                    :expr 1}}]})))

  (it "rejects set with the wrong assigned type"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-set
                    :imports []
                    :exports ['program]
                    :decls [{:op :fn
                             :name 'program
                             :params []
                             :return-type 'String
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
                                             :expr "nope"}
                                            "done"]}}]})))

  (it "accepts loop and recur with matching binder types"
    (let [module {:name 'example/loop
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :loop
                                  :bindings [{:name 'current
                                              :expr 1}]
                                  :body {:op :if
                                         :test true
                                         :then {:op :recur
                                                :args [2]}
                                         :else {:op :local
                                                :name 'current}}}}]}]
      (should= module (sut/check-module module))))

  (it "rejects recur with the wrong arity"
    (should-throw clojure.lang.ExceptionInfo
                  "Arity mismatch."
                  (sut/check-module
                   {:name 'example/bad-recur-arity
                    :imports []
                    :exports ['program]
                    :decls [{:op :fn
                             :name 'program
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :loop
                                    :bindings [{:name 'current
                                                :expr 1}]
                                    :body {:op :recur
                                           :args []}}}]})))

  (it "rejects try expressions with incompatible catch types"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-try
                    :imports []
                    :exports ['program]
                    :decls [{:op :fn
                             :name 'program
                             :params []
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :try
                                    :body 1
                                    :catches [{:type 'Error
                                               :name 'ex
                                               :body "nope"}]
                                    :finally 2}}]})))

  (it "accepts exhaustive enum matches"
    (let [module {:name 'example/enum-match
                  :imports []
                  :exports ['describe]
                  :decls [{:op :enum
                           :name 'Color
                           :variants ['Red 'Blue]}
                          {:op :fn
                           :name 'describe
                           :params [{:name 'color :type 'Color}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'color}
                                  :cases [{:pattern {:op :binder-pattern
                                                     :name 'Red}
                                           :body 1}
                                          {:pattern {:op :binder-pattern
                                                     :name 'Blue}
                                           :body 2}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts exhaustive union matches with typed binders"
    (let [module {:name 'example/union-match
                  :imports []
                  :exports ['render]
                  :decls [{:op :union
                           :name 'Response
                           :type-params []
                           :invariants []
                           :variants [{:name 'Ok
                                       :fields [{:name 'value :type 'Int}]}
                                      {:name 'Error
                                       :fields [{:name 'message :type 'String}]}]}
                          {:op :fn
                           :name 'render
                           :params [{:name 'response :type 'Response}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'response}
                                  :cases [{:pattern {:op :union-pattern
                                                     :name 'Ok
                                                     :args [{:op :binder-pattern
                                                             :name 'value}]}
                                           :body {:op :local :name 'value}}
                                          {:pattern {:op :union-pattern
                                                     :name 'Error
                                                     :args [{:op :binder-pattern
                                                             :name 'message}]}
                                           :body 0}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts record patterns against the matching record type"
    (let [module {:name 'example/record-match
                  :imports []
                  :exports ['status]
                  :decls [{:op :data
                           :name 'Response
                           :type-params []
                           :invariants []
                           :fields [{:name 'status :type 'Int}
                                    {:name 'body :type 'String}]}
                          {:op :fn
                           :name 'status
                           :params [{:name 'response :type 'Response}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'response}
                                  :cases [{:pattern {:op :record-pattern
                                                     :type 'Response
                                                     :fields [{:name 'status
                                                               :pattern {:op :binder-pattern
                                                                         :name 'code}}]}
                                           :body {:op :local :name 'code}}]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects union patterns with the wrong payload arity"
    (should-throw clojure.lang.ExceptionInfo
                  "Arity mismatch."
                  (sut/check-module
                   {:name 'example/bad-union-arity
                    :imports []
                    :exports ['render]
                    :decls [{:op :union
                             :name 'Response
                             :type-params []
                             :invariants []
                             :variants [{:name 'Ok
                                         :fields [{:name 'value :type 'Int}]}]}
                            {:op :fn
                             :name 'render
                             :params [{:name 'response :type 'Response}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :match
                                    :target {:op :local :name 'response}
                                    :cases [{:pattern {:op :union-pattern
                                                       :name 'Ok
                                                       :args []}
                                             :body 1}]}}]})))

  (it "accepts non-exhaustive enum matches when they are otherwise well typed"
    (let [module {:name 'example/non-exhaustive-enum
                  :imports []
                  :exports ['describe]
                  :decls [{:op :enum
                           :name 'Color
                           :variants ['Red 'Blue]}
                          {:op :fn
                           :name 'describe
                           :params [{:name 'color :type 'Color}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'color}
                                  :cases [{:pattern {:op :binder-pattern
                                                     :name 'Red}
                                           :body 1}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts non-exhaustive boolean matches when they are otherwise well typed"
    (let [module {:name 'example/non-exhaustive-bool
                  :imports []
                  :exports ['describe]
                  :decls [{:op :fn
                           :name 'describe
                           :params [{:name 'flag :type 'Bool}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'flag}
                                  :cases [{:pattern {:op :literal-pattern
                                                     :literal true}
                                           :body 1}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts well-typed union variant construction"
    (let [module {:name 'example/variant
                  :imports []
                  :exports ['ok]
                  :decls [{:op :union
                           :name 'Response
                           :type-params []
                           :invariants []
                           :variants [{:name 'Ok
                                       :fields [{:name 'value :type 'Int}]}
                                      {:name 'Error
                                       :fields [{:name 'message :type 'String}]}]}
                          {:op :fn
                           :name 'ok
                           :params []
                           :return-type 'Response
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :variant
                                  :type 'Response
                                  :name 'Ok
                                  :args [1]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects unknown union variants"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown variant."
                  (sut/check-module
                   {:name 'example/bad-variant
                    :imports []
                    :exports ['oops]
                    :decls [{:op :union
                             :name 'Response
                             :type-params []
                             :invariants []
                             :variants [{:name 'Ok
                                         :fields [{:name 'value :type 'Int}]}]}
                            {:op :fn
                             :name 'oops
                             :params []
                             :return-type 'Response
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :variant
                                    :type 'Response
                                    :name 'Missing
                                    :args [1]}}]})))

  (it "rejects union variants with wrong payload types"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-variant-payload
                    :imports []
                    :exports ['oops]
                    :decls [{:op :union
                             :name 'Response
                             :type-params []
                             :invariants []
                             :variants [{:name 'Ok
                                         :fields [{:name 'value :type 'Int}]}]}
                            {:op :fn
                             :name 'oops
                             :params []
                             :return-type 'Response
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :variant
                                    :type 'Response
                                    :name 'Ok
                                    :args ["nope"]}}]})))

  (it "accepts assignable Java reference arguments in static calls"
    (let [module {:name 'example/java-assignable-call
                  :imports []
                  :exports ['render]
                  :decls [{:op :fn
                           :name 'render
                           :params []
                           :return-type 'String
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-static-call
                                  :class-name 'java.lang.String
                                  :member-id 'valueOf
                                  :signature {:params ['(Java java.lang.Object)]
                                              :return-type 'String}
                                  :args [{:op :java-new
                                          :class-name 'java.lang.StringBuilder
                                          :type-args []
                                          :args ["a"]}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts assignable Java reference values in field writes"
    (let [module {:name 'example/java-assignable-field
                  :imports []
                  :exports ['store]
                  :decls [{:op :fn
                           :name 'store
                           :params [{:name 'holder
                                     :type '(Java java.io.PrintStream)}]
                           :return-type 'Unit
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-set-field
                                  :target {:op :local :name 'holder}
                                  :field-name 'trouble
                                  :field-type '(Java java.io.OutputStream)
                                  :expr {:op :java-new
                                         :class-name 'java.io.ByteArrayOutputStream
                                         :type-args []
                                         :args []}}}]}]
      (should= module (sut/check-module module)))))
