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

  (it "rejects non-boolean data invariants"
    (should-throw clojure.lang.ExceptionInfo
                  "Expected Bool."
                  (sut/check-module
                   {:name 'example/bad-invariant
                    :imports []
                    :exports []
                    :decls [{:op :data
                             :name 'Response
                             :type-params []
                             :invariants [1]
                             :fields [{:name 'status :type 'Int}]}]})))

  (it "accepts data invariants that refer to fields as locals"
    (let [module {:name 'example/data-invariant
                  :imports []
                  :exports ['make]
                  :decls [{:op :data
                           :name 'Response
                           :type-params []
                           :invariants [{:op :local :name 'valid?}]
                           :fields [{:name 'valid? :type 'Bool}]}
                          {:op :fn
                           :name 'make
                           :params []
                           :return-type 'Response
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :construct
                                  :type 'Response
                                  :args [true]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts union invariants that inspect payloads via self"
    (let [module {:name 'example/union-invariant
                  :imports []
                  :exports ['ok]
                  :decls [{:op :union
                           :name 'Response
                           :type-params []
                           :invariants [{:op :match
                                         :target {:op :local :name 'self}
                                         :cases [{:pattern {:op :union-pattern
                                                            :name 'Ok
                                                            :args [{:op :binder-pattern
                                                                    :name 'value}]}
                                                  :body {:op :local :name 'value}}
                                                 {:pattern {:op :union-pattern
                                                            :name 'Error
                                                            :args []}
                                                  :body true}]}]
                           :variants [{:name 'Ok
                                       :fields [{:name 'value :type 'Bool}]}
                                      {:name 'Error
                                       :fields []}]}
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
                                  :args [true]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects union invariants that refer to payload names outside a match"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown local type."
                  (sut/check-module
                   {:name 'example/bad-union-invariant
                    :imports []
                    :exports []
                    :decls [{:op :union
                             :name 'Response
                             :type-params []
                             :invariants [{:op :local :name 'value}]
                             :variants [{:name 'Ok
                                         :fields [{:name 'value :type 'Int}]}]}]})))

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

  (it "accepts primitive operators with matching operand types"
    (let [body {:op :bool-or
                :args [{:op :bool-not
                        :arg {:op :local :name 'flag}}
                       {:op :bool-and
                        :args [{:op :int-lt
                                :args [{:op :int-add
                                        :args [{:op :local :name 'x}
                                               {:op :local :name 'y}]}
                                       {:op :int-mul
                                        :args [10
                                               {:op :int-sub
                                                :args [{:op :local :name 'y}
                                                       1]}]}]}
                               {:op :int-eq
                                :args [{:op :int-mod
                                        :args [{:op :local :name 'x}
                                               2]}
                                       {:op :int-div
                                        :args [{:op :local :name 'y}
                                               2]}]}]}]}
          module {:name 'example/operators
                  :imports []
                  :exports ['compute]
                  :decls [{:op :fn
                           :name 'compute
                           :params [{:name 'x :type 'Int}
                                    {:name 'y :type 'Int}
                                    {:name 'flag :type 'Bool}]
                           :return-type 'Bool
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body body}]}]
      (should= module (sut/check-module module))))

  (it "rejects primitive operators with mismatched operand types"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-operators
                    :imports []
                    :exports ['compute]
                    :decls [{:op :fn
                             :name 'compute
                             :params [{:name 'flag :type 'Bool}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :int-add
                                    :args [{:op :local :name 'flag}
                                           1]}}]})))

  (it "accepts additional comparisons and boolean equality"
    (let [module {:name 'example/more-operators
                  :imports []
                  :exports ['compare]
                  :decls [{:op :fn
                           :name 'compare
                           :params [{:name 'x :type 'Int}
                                    {:name 'y :type 'Int}
                                    {:name 'flag :type 'Bool}]
                           :return-type 'Bool
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :bool-eq
                                  :args [{:op :int-ge
                                          :args [{:op :local :name 'x}
                                                 {:op :local :name 'y}]}
                                         {:op :bool-or
                                          :args [{:op :local :name 'flag}
                                {:op :bool-and
                                  :args [{:op :int-gt
                                          :args [{:op :local :name 'y}
                                                 0]}
                                         {:op :int-le
                                          :args [{:op :local :name 'x}
                                                 {:op :local :name 'y}]}]}]}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts string equality int inequality conversion and stdout output"
    (let [module {:name 'example/io-ops
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'x :type 'Int}
                                    {:name 'label :type 'String}]
                           :return-type 'Bool
                           :effects ['Stdout.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :io-println
                                           :arg {:op :int->string
                                                 :arg {:op :local :name 'x}}}
                                          {:op :string-eq
                                           :args [{:op :local :name 'label}
                                                  "ok"]}
                                          {:op :int-ne
                                           :args [{:op :local :name 'x}
                                                  0]}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts stdin stdout string operations and fallible string conversion"
    (let [module {:name 'example/text-io
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects ['Foreign.Throw 'Stdin.Read 'Stdout.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :io-print
                                           :arg {:op :string-concat
                                                 :args [">"
                                                        {:op :io-read-line}]}}
                                          {:op :int-add
                                           :args [{:op :string-length
                                                   :arg "ab"}
                                                  {:op :string->int
                                                   :arg "7"}]}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts trimmed string splitting emptiness checks and sequence access"
    (let [module {:name 'example/text-seq
                  :imports []
                  :exports ['metric]
                  :decls [{:op :fn
                           :name 'metric
                           :params [{:name 'line :type 'String}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :if
                                  :test {:op :string-empty?
                                         :arg {:op :string-trim
                                               :arg {:op :local :name 'line}}}
                                  :then 0
                                  :else {:op :string-length
                                         :arg {:op :seq-get
                                               :args [{:op :string-split-on
                                                       :args [{:op :string-trim
                                                               :arg {:op :local :name 'line}}
                                                              ","]}
                                                      1]}}}}]}]
      (should= module (sut/check-module module))))

  (it "accepts floating-point literals operators conversions and interop signatures"
    (let [module {:name 'example/floating
                  :imports [{:op :java-import
                             :class-name 'java.lang.Math}]
                  :exports ['orbit-step]
                  :decls [{:op :fn
                           :name 'orbit-step
                           :params [{:name 'phase :type 'Double}
                                    {:name 'scale :type 'Float}]
                           :return-type 'Float
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :double->float
                                  :arg {:op :double-add
                                        :args [{:op :java-static-call
                                                :class-name 'java.lang.Math
                                                :member-id 'cos
                                                :signature {:params ['Double]
                                                            :return-type 'Double}
                                                :args [{:op :local :name 'phase}]}
                                               {:op :float->double
                                                :arg {:op :float-div
                                                      :args [{:op :local :name 'scale}
                                                             {:op :int->float
                                                              :arg 2}]}}]}}}]}]
      (should= module (sut/check-module module))))

  (it "accepts direct Float and Double literals"
    (let [module {:name 'example/literals
                  :imports []
                  :exports ['sample-float 'sample-double]
                  :decls [{:op :fn
                           :name 'sample-float
                           :params []
                           :return-type 'Float
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body (float 1.25)}
                          {:op :fn
                           :name 'sample-double
                           :params []
                           :return-type 'Double
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body 1.25}]}]
      (should= module (sut/check-module module))))

  (it "accepts direct sequence length checks over StringSeq"
    (let [module {:name 'example/text-seq-length
                  :imports []
                  :exports ['count-parts]
                  :decls [{:op :fn
                           :name 'count-parts
                           :params [{:name 'line :type 'String}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq-length
                                  :arg {:op :string-split-on
                                        :args [{:op :local :name 'line}
                                               ","]}}}]}]
      (should= module (sut/check-module module))))

  (it "accepts substring char-at and first/empty sequence primitives"
    (let [module {:name 'example/text-scan
                  :imports []
                  :exports ['token]
                  :decls [{:op :fn
                           :name 'token
                           :params [{:name 'line :type 'String}]
                           :return-type 'String
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :if
                                  :test {:op :seq-empty?
                                         :arg {:op :string-split-on
                                               :args [{:op :local :name 'line}
                                                      ","]}}
                                  :then ""
                                  :else {:op :string-char-at
                                         :args [{:op :string-substring
                                                 :args [{:op :seq-first
                                                         :arg {:op :string-split-on
                                                               :args [{:op :local :name 'line}
                                                                      ","]}}
                                                        1
                                                        3]}
                                                0]}}}]}]
      (should= module (sut/check-module module))))

  (it "accepts sequence rest concat and canonical map primitives"
    (let [module {:name 'example/data-core
                  :imports [{:op :airj-import
                             :module 'airj/core
                             :symbols ['Option]}]
                  :interfaces {'airj/core {:name 'airj/core
                                           :imports []
                                           :exports ['Option]
                                           :decls [{:op :union
                                                    :name 'Option
                                                    :type-params ['T]
                                                    :variants [{:name 'None
                                                                :fields []}
                                                               {:name 'Some
                                                                :fields [{:name 'value
                                                                          :type 'T}]}]}]}}
                  :exports ['summarize]
                  :decls [{:op :fn
                           :name 'summarize
                           :params [{:name 'items :type '(Seq String)}]
                           :return-type '(Option Int)
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :map-get
                                  :args [{:op :map-set
                                          :args [{:op :map-empty
                                                  :value-type 'Int}
                                                 "tail-size"
                                                 {:op :seq-length
                                                  :arg {:op :seq-concat
                                                        :args [{:op :seq-rest
                                                                :arg {:op :local :name 'items}}
                                                               {:op :string-split-on
                                                                :args ["z" ","]}]}}]}
                                         "tail-size"]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects map-set with a non-string key"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-map-key
                    :imports []
                    :exports ['build]
                    :decls [{:op :fn
                             :name 'build
                             :params []
                             :return-type '(Map String Int)
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :map-set
                                    :args [{:op :map-empty
                                            :value-type 'Int}
                                           1
                                           2]}}]})))

  (it "rejects string-char-at with a non-int index"
    (should-throw clojure.lang.ExceptionInfo
                  "Type mismatch."
                  (sut/check-module
                   {:name 'example/bad-char-at
                    :imports []
                    :exports ['token]
                    :decls [{:op :fn
                             :name 'token
                             :params [{:name 'line :type 'String}]
                             :return-type 'String
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :string-char-at
                                    :args [{:op :local :name 'line}
                                           "0"]}}]}))))

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
      (should= module (sut/check-module module))))

  (it "accepts static Java field access and assignable writes"
    (let [module {:name 'example/java-static-field
                  :imports []
                  :exports ['stream 'swap]
                  :decls [{:op :fn
                           :name 'stream
                           :params []
                           :return-type '(Java java.io.PrintStream)
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-static-get-field
                                  :class-name 'java.lang.System
                                  :field-name 'out
                                  :field-type '(Java java.io.PrintStream)}}
                          {:op :fn
                           :name 'swap
                           :params []
                           :return-type 'Unit
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-static-set-field
                                  :class-name 'java.lang.System
                                  :field-name 'out
                                  :field-type '(Java java.io.PrintStream)
                                  :expr {:op :java-new
                                         :class-name 'java.io.PrintStream
                                         :type-args []
                                         :args [{:op :java-new
                                                 :class-name 'java.io.ByteArrayOutputStream
                                                 :type-args []
                                                 :args []}]}}}]}]
      (should= module (sut/check-module module))))

  (it "infers types for static Java field writes directly"
    (let [expr {:op :java-static-set-field
                :class-name 'java.lang.System
                :field-name 'out
                :field-type '(Java java.io.PrintStream)
                :expr {:op :java-new
                       :class-name 'java.io.PrintStream
                       :type-args []
                       :args [{:op :java-new
                               :class-name 'java.io.ByteArrayOutputStream
                               :type-args []
                               :args []}]}}
          ctx {:locals {}
               :mutable #{}}]
      (should= 'Unit (sut/infer-expr-type expr ctx {}))))

  (it "rejects mismatched static Java field writes"
    (let [expr {:op :java-static-set-field
                :class-name 'java.lang.System
                :field-name 'out
                :field-type '(Java java.io.PrintStream)
                :expr "not-a-stream"}
          ctx {:locals {}
               :mutable #{}}]
      (should-throw clojure.lang.ExceptionInfo
                    #"Type mismatch"
                    (sut/infer-expr-type expr ctx {}))))

  (it "leaves invariant context unchanged for non-invariant declarations"
    (let [ctx {:env {'value 'String}
               :mutable #{}
               :loop-types nil}
          invariant-ctx (ns-resolve 'airj.type-checker 'invariant-ctx)]
      (should= ctx (invariant-ctx {:op :fn
                                   :name 'noop}
                                  ctx))))

  (it "returns the original context from the invariant passthrough helper"
    (let [ctx {:env {'value 'String}
               :mutable #{}
               :loop-types nil}
          passthrough-invariant-ctx (ns-resolve 'airj.type-checker 'passthrough-invariant-ctx)]
      (should= ctx (passthrough-invariant-ctx {:op :fn
                                               :name 'noop}
                                              ctx))))

  (it "checks invariants on non-data declarations with the existing context"
    (let [ctx {:locals {'value 'Bool}
               :mutable-locals {}
               :loop-types nil}
          check-invariants (ns-resolve 'airj.type-checker 'check-invariants)]
      (should-not-throw
        (check-invariants {:op :fn
                           :name 'noop
                           :invariants [{:op :local :name 'value}]}
                          ctx
                          {}))))

  (it "type-checks parameterized Result variants and match bindings"
    (let [module {:name 'example/result
                  :imports []
                  :exports ['unwrap]
                  :decls [{:op :union
                           :name 'Result
                           :type-params ['Ok 'Err]
                           :invariants []
                           :variants [{:name 'Ok
                                       :fields [{:name 'value
                                                 :type 'Ok}]}
                                      {:name 'Err
                                       :fields [{:name 'error
                                                 :type 'Err}]}]}
                          {:op :fn
                           :name 'unwrap
                           :params [{:name 'result
                                     :type '(Result Int String)}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'result}
                                  :cases [{:pattern {:op :union-pattern
                                                     :name 'Ok
                                                     :args [{:op :binder-pattern
                                                             :name 'value}]}
                                           :body {:op :local :name 'value}}
                                          {:pattern {:op :union-pattern
                                                     :name 'Err
                                                     :args [{:op :binder-pattern
                                                             :name 'message}]}
                                           :body 0}]}}]}]
      (should= module
               (sut/check-module module))))

  (it "type-checks canonical JSON interchange primitives"
    (let [module {:name 'example/json_type
                  :imports []
                  :exports ['roundtrip]
                  :decls [{:op :union
                           :name 'Interchange
                           :type-params []
                           :invariants []
                           :variants [{:name 'Null
                                       :fields []}
                                      {:name 'BoolValue
                                       :fields [{:name 'value
                                                 :type 'Bool}]}
                                      {:name 'IntValue
                                       :fields [{:name 'value
                                                 :type 'Int}]}
                                      {:name 'DoubleValue
                                       :fields [{:name 'value
                                                 :type 'Double}]}
                                      {:name 'StringValue
                                       :fields [{:name 'value
                                                 :type 'String}]}
                                      {:name 'SeqValue
                                       :fields [{:name 'value
                                                 :type '(Seq Interchange)}]}
                                      {:name 'MapValue
                                       :fields [{:name 'value
                                                 :type '(Map String Interchange)}]}]}
                          {:op :fn
                           :name 'roundtrip
                           :params [{:name 'text :type 'String}]
                           :return-type 'String
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :json-write
                                  :arg {:op :json-parse
                                        :arg {:op :local :name 'text}}}}]}]
      (should= module
               (sut/check-module module))))

  (it "rejects json-write on non-interchange values"
    (let [module {:name 'example/json_type_error
                  :imports []
                  :exports ['broken]
                  :decls [{:op :fn
                           :name 'broken
                           :params [{:name 'text :type 'String}]
                           :return-type 'String
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :json-write
                                  :arg {:op :local :name 'text}}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Type mismatch."
                    (sut/check-module module))))

  (it "type-checks canonical host environment and process primitives"
    (let [module {:name 'example/host_type
                  :imports []
                  :exports ['run]
                  :decls [{:op :union
                           :name 'Option
                           :type-params ['T]
                           :invariants []
                           :variants [{:name 'None
                                       :fields []}
                                      {:name 'Some
                                       :fields [{:name 'value
                                                 :type 'T}]}]}
                          {:op :data
                           :name 'ProcessResult
                           :type-params []
                           :invariants []
                           :fields [{:name 'exit-code :type 'Int}
                                    {:name 'stdout :type 'Bytes}
                                    {:name 'stderr :type 'Bytes}]}
                          {:op :fn
                           :name 'run
                           :params [{:name 'name :type 'String}
                                    {:name 'command :type '(Seq String)}
                                    {:name 'stdin :type 'Bytes}]
                           :return-type 'ProcessResult
                           :effects ['Env.Read 'Process.Run 'Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :env-get
                                           :arg {:op :local :name 'name}}
                                          {:op :process-run
                                           :args [{:op :local :name 'command}
                                                  {:op :local :name 'stdin}]}]}}]}]
      (should= module
               (sut/check-module module))))
