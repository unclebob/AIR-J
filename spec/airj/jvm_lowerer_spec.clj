(ns airj.jvm-lowerer-spec
  (:require [airj.jvm-lowerer :as sut]
            [speclj.core :refer :all]))

(describe "JVM lowering"
  (it "lowers module structure and interop expressions into a JVM plan"
    (let [module {:name 'example/interop
                  :imports []
                  :exports ['Response 'Status 'Result 'interop]
                  :decls [{:op :data
                           :name 'Response
                           :type-params []
                           :invariants []
                           :fields [{:name 'status :type 'Int}
                                    {:name 'body :type 'String}]}
                          {:op :enum
                           :name 'Status
                           :variants ['Ok 'Error]}
                          {:op :union
                           :name 'Result
                           :type-params []
                           :invariants []
                           :variants [{:name 'Done
                                       :fields [{:name 'value :type 'Int}]}
                                      {:name 'Failed
                                       :fields [{:name 'message :type 'String}]}]}
                          {:op :fn
                           :name 'interop
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Int
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-static-call
                                  :class-name 'java.lang.Math
                                  :member-id 'abs
                                  :signature {:params ['Int]
                                              :return-type 'Int}
                                  :args [{:op :local :name 'value}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/interop
                :internal-name "example/interop"
                :exports ['Response 'Status 'Result 'interop]
                :records [{:name 'Response
                           :class-name "example/interop$Response"
                           :fields [{:name 'status :jvm-type :int}
                                    {:name 'body :jvm-type "java/lang/String"}]}]
                :enums [{:name 'Status
                         :class-name "example/interop$Status"
                         :variants ['Ok 'Error]}]
                :unions [{:name 'Result
                          :base-class "example/interop$Result"
                          :variants [{:name 'Done
                                      :class-name "example/interop$Result$Done"
                                      :fields [{:name 'value :jvm-type :int}]}
                                     {:name 'Failed
                                      :class-name "example/interop$Result$Failed"
                                      :fields [{:name 'message :jvm-type "java/lang/String"}]}]}]
                :methods [{:name 'interop
                           :owner "example/interop"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects ['Foreign.Throw]
                           :body {:op :jvm-java-static-call
                                  :class-name "java/lang/Math"
                                  :member-id 'abs
                                  :parameter-types [:int]
                                  :return-type :int
                                  :jvm-type :int
                                  :args [{:op :jvm-local
                                          :name 'value
                                          :jvm-type :int}]}}]}
               (sut/lower-module module))))

  (it "lowers host-backed exported functions into instance bridge methods"
    (let [module {:name 'example/hosted
                  :host {:class-name 'java.util.ArrayList}
                  :imports [{:op :java-import
                             :class-name 'java.util.ArrayList}]
                  :exports ['snapshot]
                  :decls [{:op :fn
                           :name 'snapshot
                           :params [{:name 'self
                                     :type '(Java java.util.ArrayList)}]
                           :return-type 'Int
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-call
                                  :target {:op :local
                                           :name 'self}
                                  :member-id 'size
                                  :signature {:params []
                                              :return-type 'Int}
                                  :args []}}]}]
      (should= {:class-name "java/util/ArrayList"}
               (:host (sut/lower-module module)))
      (should= [{:name 'snapshot
                 :owner "example/hosted"
                 :params [{:name 'self
                           :jvm-type "java/util/ArrayList"}]
                 :return-type :int
                 :effects ['Foreign.Throw]
                 :body {:op :jvm-java-call
                        :target {:op :jvm-local
                                 :name 'self
                                 :jvm-type "java/util/ArrayList"}
                        :member-id 'size
                        :parameter-types []
                        :return-type :int
                        :args []
                        :jvm-type :int}}]
               (:methods (sut/lower-module module)))
      (should= [{:name 'snapshot
                 :owner "example/hosted"
                 :params []
                 :return-type :int
                 :target {:name 'snapshot
                          :owner "example/hosted"
                          :parameter-types ["java/util/ArrayList"]
                          :return-type :int}}]
               (:instance-methods (sut/lower-module module))))))

  (it "lowers instance java calls and fields into JVM plan nodes"
    (let [module {:name 'example/java_instance
                  :imports []
                  :exports ['interop]
                  :decls [{:op :fn
                           :name 'interop
                           :params []
                           :return-type 'Int
                           :effects ['State.Write 'Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :java-call
                                           :target {:op :java-new
                                                    :class-name 'java.lang.StringBuilder
                                                    :type-args []
                                                    :args ["a"]}
                                           :member-id 'append
                                           :signature {:params ['String]
                                                       :return-type '(Java java.lang.StringBuilder)}
                                           :args ["b"]}
                                          {:op :java-set-field
                                           :target {:op :java-new
                                                    :class-name 'java.awt.Point
                                                    :type-args []
                                                    :args [1 2]}
                                           :field-name 'x
                                           :field-type 'Int
                                           :expr 9}
                                          {:op :java-get-field
                                           :target {:op :java-new
                                                    :class-name 'java.awt.Point
                                                    :type-args []
                                                    :args [1 2]}
                                           :field-name 'x
                                           :field-type 'Int}]}}]}]
      (should= {:op :jvm-seq
                :exprs [{:op :jvm-java-call
                         :target {:op :jvm-java-new
                                  :class-name "java/lang/StringBuilder"
                                  :parameter-types ["java/lang/String"]
                                  :args [{:op :jvm-string
                                          :value "a"
                                          :jvm-type "java/lang/String"}]
                                  :jvm-type "java/lang/StringBuilder"}
                         :member-id 'append
                         :parameter-types ["java/lang/String"]
                         :return-type "java/lang/StringBuilder"
                         :args [{:op :jvm-string
                                 :value "b"
                                 :jvm-type "java/lang/String"}]
                         :jvm-type "java/lang/StringBuilder"}
                        {:op :jvm-java-set-field
                         :target {:op :jvm-java-new
                                  :class-name "java/awt/Point"
                                  :parameter-types [:int :int]
                                  :args [{:op :jvm-int
                                          :value 1
                                          :jvm-type :int}
                                         {:op :jvm-int
                                          :value 2
                                          :jvm-type :int}]
                                  :jvm-type "java/awt/Point"}
                         :field-name 'x
                         :field-type :int
                         :expr {:op :jvm-int
                                :value 9
                                :jvm-type :int}
                         :jvm-type :void}
                        {:op :jvm-java-get-field
                         :target {:op :jvm-java-new
                                  :class-name "java/awt/Point"
                                  :parameter-types [:int :int]
                                  :args [{:op :jvm-int
                                          :value 1
                                          :jvm-type :int}
                                         {:op :jvm-int
                                          :value 2
                                          :jvm-type :int}]
                                  :jvm-type "java/awt/Point"}
                         :field-name 'x
                         :field-type :int
                         :jvm-type :int}]
                :jvm-type :int}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers static Java fields into JVM plan nodes"
    (let [module {:name 'example/java_static_field
                  :imports []
                  :exports ['interop]
                  :decls [{:op :fn
                           :name 'interop
                           :params []
                           :return-type '(Java java.io.PrintStream)
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :java-static-set-field
                                           :class-name 'java.lang.System
                                           :field-name 'out
                                           :field-type '(Java java.io.PrintStream)
                                           :expr {:op :java-static-get-field
                                                  :class-name 'java.lang.System
                                                  :field-name 'out
                                                  :field-type '(Java java.io.PrintStream)}}
                                          {:op :java-static-get-field
                                           :class-name 'java.lang.System
                                           :field-name 'out
                                           :field-type '(Java java.io.PrintStream)}]}}]}]
      (should= {:op :jvm-seq
                :exprs [{:op :jvm-java-static-set-field
                         :class-name "java/lang/System"
                         :field-name 'out
                         :field-type "java/io/PrintStream"
                         :expr {:op :jvm-java-static-get-field
                                :class-name "java/lang/System"
                                :field-name 'out
                                :field-type "java/io/PrintStream"
                                :jvm-type "java/io/PrintStream"}
                         :jvm-type :void}
                        {:op :jvm-java-static-get-field
                         :class-name "java/lang/System"
                         :field-name 'out
                         :field-type "java/io/PrintStream"
                         :jvm-type "java/io/PrintStream"}]
                :jvm-type "java/io/PrintStream"}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers Bool Unit and Java types"
    (let [module {:name 'example/types
                  :imports []
                  :exports ['Flag 'holder]
                  :decls [{:op :data
                           :name 'Flag
                           :type-params []
                           :invariants []
                           :fields [{:name 'enabled :type 'Bool}
                                    {:name 'builder :type '(Java java.lang.StringBuilder)}]}
                          {:op :fn
                           :name 'holder
                           :params [{:name 'flag :type 'Bool}]
                           :return-type 'Unit
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :local :name 'flag}}]}]
      (should= {:op :jvm-module
                :module-name 'example/types
                :internal-name "example/types"
                :exports ['Flag 'holder]
                :records [{:name 'Flag
                           :class-name "example/types$Flag"
                           :fields [{:name 'enabled :jvm-type :boolean}
                                    {:name 'builder :jvm-type "java/lang/StringBuilder"}]}]
                :enums []
                :unions []
                :methods [{:name 'holder
                           :owner "example/types"
                           :params [{:name 'flag :jvm-type :boolean}]
                           :return-type :void
                           :effects []
                           :body {:op :jvm-local
                                  :name 'flag
                                  :jvm-type :boolean}}]}
               (sut/lower-module module))))

  (it "lowers primitive operators into JVM plan nodes"
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
      (should= {:op :jvm-bool-or
                :args [{:op :jvm-bool-not
                        :arg {:op :jvm-local
                              :name 'flag
                              :jvm-type :boolean}
                        :jvm-type :boolean}
                       {:op :jvm-bool-and
                        :args [{:op :jvm-int-lt
                                :args [{:op :jvm-int-add
                                        :args [{:op :jvm-local
                                                :name 'x
                                                :jvm-type :int}
                                               {:op :jvm-local
                                                :name 'y
                                                :jvm-type :int}]
                                        :jvm-type :int}
                                       {:op :jvm-int-mul
                                        :args [{:op :jvm-int
                                                :value 10
                                                :jvm-type :int}
                                               {:op :jvm-int-sub
                                                :args [{:op :jvm-local
                                                        :name 'y
                                                        :jvm-type :int}
                                                       {:op :jvm-int
                                                        :value 1
                                                        :jvm-type :int}]
                                                :jvm-type :int}]
                                        :jvm-type :int}]
                                :jvm-type :boolean}
                               {:op :jvm-int-eq
                                :args [{:op :jvm-int-mod
                                        :args [{:op :jvm-local
                                                :name 'x
                                                :jvm-type :int}
                                               {:op :jvm-int
                                                :value 2
                                                :jvm-type :int}]
                                        :jvm-type :int}
                                       {:op :jvm-int-div
                                        :args [{:op :jvm-local
                                                :name 'y
                                                :jvm-type :int}
                                               {:op :jvm-int
                                                :value 2
                                                :jvm-type :int}]
                                        :jvm-type :int}]
                                :jvm-type :boolean}]
                        :jvm-type :boolean}]
                :jvm-type :boolean}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers additional comparisons and boolean equality"
    (let [module {:name 'example/more-operators
                  :imports []
                  :exports ['compare]
                  :decls [{:op :fn
                           :name 'compare
                           :params [{:name 'x :type 'Int}
                                    {:name 'y :type 'Int}]
                           :return-type 'Bool
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :bool-eq
                                  :args [{:op :int-ge
                                          :args [{:op :local :name 'x}
                                                 {:op :local :name 'y}]}
                                         {:op :bool-and
                                          :args [{:op :int-gt
                                                  :args [{:op :local :name 'x}
                                                         0]}
                                                 {:op :int-le
                                                  :args [{:op :local :name 'x}
                                                         {:op :local :name 'y}]}]}]}}]}]
      (should= {:op :jvm-bool-eq
                :args [{:op :jvm-int-ge
                        :args [{:op :jvm-local
                                :name 'x
                                :jvm-type :int}
                               {:op :jvm-local
                                :name 'y
                                :jvm-type :int}]
                        :jvm-type :boolean}
                       {:op :jvm-bool-and
                        :args [{:op :jvm-int-gt
                                :args [{:op :jvm-local
                                        :name 'x
                                        :jvm-type :int}
                                       {:op :jvm-int
                                        :value 0
                                        :jvm-type :int}]
                                :jvm-type :boolean}
                               {:op :jvm-int-le
                                :args [{:op :jvm-local
                                        :name 'x
                                        :jvm-type :int}
                                       {:op :jvm-local
                                        :name 'y
                                        :jvm-type :int}]
                                :jvm-type :boolean}]
                        :jvm-type :boolean}]
                :jvm-type :boolean}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers floating-point literals operators and conversions into JVM plan nodes"
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
      (should= {:op :jvm-double->float
                :arg {:op :jvm-double-add
                      :args [{:op :jvm-java-static-call
                              :class-name "java/lang/Math"
                              :member-id 'cos
                              :parameter-types [:double]
                              :return-type :double
                              :args [{:op :jvm-local
                                      :name 'phase
                                      :jvm-type :double}]
                              :jvm-type :double}
                             {:op :jvm-float->double
                              :arg {:op :jvm-float-div
                                    :args [{:op :jvm-local
                                            :name 'scale
                                            :jvm-type :float}
                                           {:op :jvm-int->float
                                            :arg {:op :jvm-int
                                                  :value 2
                                                  :jvm-type :int}
                                            :jvm-type :float}]
                                    :jvm-type :float}
                              :jvm-type :double}]
                      :jvm-type :double}
                :jvm-type :float}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers conversion equality and stdout output into JVM plan nodes"
    (let [module {:name 'example/io_ops
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
      (should= {:op :jvm-seq
                :exprs [{:op :jvm-io-println
                         :arg {:op :jvm-int->string
                               :arg {:op :jvm-local
                                     :name 'x
                                     :jvm-type :int}
                               :jvm-type "java/lang/String"}
                         :jvm-type :void}
                        {:op :jvm-string-eq
                         :args [{:op :jvm-local
                                 :name 'label
                                 :jvm-type "java/lang/String"}
                                {:op :jvm-string
                                 :value "ok"
                                 :jvm-type "java/lang/String"}]
                         :jvm-type :boolean}
                        {:op :jvm-int-ne
                         :args [{:op :jvm-local
                                 :name 'x
                                 :jvm-type :int}
                                {:op :jvm-int
                                 :value 0
                                 :jvm-type :int}]
                         :jvm-type :boolean}]
                :jvm-type :boolean}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers stdin stdout string operations and fallible conversion into JVM plan nodes"
    (let [module {:name 'example/text_io
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
      (should= {:op :jvm-seq
                :exprs [{:op :jvm-io-print
                         :arg {:op :jvm-string-concat
                               :args [{:op :jvm-string
                                       :value ">"
                                       :jvm-type "java/lang/String"}
                                      {:op :jvm-io-read-line
                                       :jvm-type "java/lang/String"}]
                               :jvm-type "java/lang/String"}
                         :jvm-type :void}
                        {:op :jvm-int-add
                         :args [{:op :jvm-string-length
                                 :arg {:op :jvm-string
                                       :value "ab"
                                       :jvm-type "java/lang/String"}
                                 :jvm-type :int}
                                {:op :jvm-string->int
                                 :arg {:op :jvm-string
                                       :value "7"
                                       :jvm-type "java/lang/String"}
                                 :jvm-type :int}]
                         :jvm-type :int}]
                :jvm-type :int}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers string sequence primitives into JVM plan nodes"
    (let [module {:name 'example/text_seq
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
      (should= {:op :jvm-if
                :test {:op :jvm-string-empty
                       :arg {:op :jvm-string-trim
                             :arg {:op :jvm-local
                                   :name 'line
                                   :jvm-type "java/lang/String"}
                             :jvm-type "java/lang/String"}
                       :jvm-type :boolean}
                :then {:op :jvm-int
                       :value 0
                       :jvm-type :int}
                :else {:op :jvm-string-length
                       :arg {:op :jvm-seq-get
                             :args [{:op :jvm-string-split-on
                                     :args [{:op :jvm-string-trim
                                             :arg {:op :jvm-local
                                                   :name 'line
                                                   :jvm-type "java/lang/String"}
                                             :jvm-type "java/lang/String"}
                                            {:op :jvm-string
                                             :value ","
                                             :jvm-type "java/lang/String"}]
                                     :jvm-type "java/util/List"}
                                    {:op :jvm-int
                                     :value 1
                                     :jvm-type :int}]
                             :jvm-type "java/lang/String"}
                       :jvm-type :int}
                :jvm-type :int}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers substring char-at and first/empty sequence primitives into JVM plan nodes"
    (let [module {:name 'example/text_scan
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
      (should= {:op :jvm-if
                :test {:op :jvm-seq-empty
                       :arg {:op :jvm-string-split-on
                             :args [{:op :jvm-local
                                     :name 'line
                                     :jvm-type "java/lang/String"}
                                    {:op :jvm-string
                                     :value ","
                                     :jvm-type "java/lang/String"}]
                             :jvm-type "java/util/List"}
                       :jvm-type :boolean}
                :then {:op :jvm-string
                       :value ""
                       :jvm-type "java/lang/String"}
                :else {:op :jvm-string-char-at
                       :args [{:op :jvm-string-substring
                               :args [{:op :jvm-seq-first
                                       :arg {:op :jvm-string-split-on
                                             :args [{:op :jvm-local
                                                     :name 'line
                                                     :jvm-type "java/lang/String"}
                                                    {:op :jvm-string
                                                     :value ","
                                                     :jvm-type "java/lang/String"}]
                                             :jvm-type "java/util/List"}
                                       :jvm-type "java/lang/String"}
                                      {:op :jvm-int
                                       :value 1
                                       :jvm-type :int}
                                      {:op :jvm-int
                                       :value 3
                                       :jvm-type :int}]
                               :jvm-type "java/lang/String"}
                              {:op :jvm-int
                               :value 0
                               :jvm-type :int}]
                       :jvm-type "java/lang/String"}
                :jvm-type "java/lang/String"}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers sequence rest concat and canonical map primitives into JVM plan nodes"
    (let [module {:name 'example/data_core
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
                  :exports ['lookup]
                  :decls [{:op :fn
                           :name 'lookup
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
      (should= {:op :jvm-map-get
                :args [{:op :jvm-map-set
                        :args [{:op :jvm-map-empty
                                :value-jvm-type :int
                                :jvm-type "java/util/Map"}
                               {:op :jvm-string
                                :value "tail-size"
                                :jvm-type "java/lang/String"}
                               {:op :jvm-seq-length
                                :arg {:op :jvm-seq-concat
                                      :args [{:op :jvm-seq-rest
                                              :arg {:op :jvm-local
                                                    :name 'items
                                                    :jvm-type "java/util/List"}
                                              :jvm-type "java/util/List"}
                                             {:op :jvm-string-split-on
                                              :args [{:op :jvm-string
                                                      :value "z"
                                                      :jvm-type "java/lang/String"}
                                                     {:op :jvm-string
                                                      :value ","
                                                      :jvm-type "java/lang/String"}]
                                              :jvm-type "java/util/List"}]
                                      :jvm-type "java/util/List"}
                                :jvm-type :int}]
                        :value-jvm-type :int
                        :jvm-type "java/util/Map"}
                       {:op :jvm-string
                        :value "tail-size"
                        :jvm-type "java/lang/String"}]
                :none-class-name "airj/core$Option$None"
                :some-class-name "airj/core$Option$Some"
                :some-parameter-types ["java/lang/Object"]
                :value-jvm-type :int
                :jvm-type "airj/core$Option"}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "lowers imported function calls to the imported module owner"
    (let [module {:name 'example/imported
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
      (should= {:op :jvm-invoke-static
                :owner "alpha/math"
                :name 'tick
                :parameter-types [:int]
                :return-type :int
                :args [{:op :jvm-int
                        :value 1
                        :jvm-type :int}]
                :jvm-type :int}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))

  (it "rejects unsupported JVM types"
    (let [module {:name 'example/unsupported
                  :imports []
                  :exports []
                  :decls [{:op :data
                           :name 'Broken
                           :type-params []
                           :invariants []
                           :fields [{:name 'value :type 'Response}]}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unsupported JVM type."
                    (sut/lower-module module))))

  (it "lowers core record and control expressions into JVM plan nodes"
    (let [module {:name 'example/core
                  :imports []
                  :exports ['select-status]
                  :decls [{:op :data
                           :name 'Response
                           :type-params []
                           :invariants []
                           :fields [{:name 'status :type 'Int}
                                    {:name 'body :type 'String}]}
                          {:op :fn
                           :name 'select-status
                           :params [{:name 'flag :type 'Bool}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :construct
                                           :type 'Response
                                           :args [7 "ok"]}
                                          {:op :if
                                           :test {:op :local :name 'flag}
                                           :then {:op :record-get
                                                  :target {:op :construct
                                                           :type 'Response
                                                           :args [1 "yes"]}
                                                  :field 'status}
                                           :else {:op :record-get
                                                  :target {:op :construct
                                                           :type 'Response
                                                           :args [2 "no"]}
                                                  :field 'status}}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/core
                :internal-name "example/core"
                :exports ['select-status]
                :records [{:name 'Response
                           :class-name "example/core$Response"
                           :fields [{:name 'status :jvm-type :int}
                                    {:name 'body :jvm-type "java/lang/String"}]}]
                :enums []
                :unions []
                :methods [{:name 'select-status
                           :owner "example/core"
                           :params [{:name 'flag :jvm-type :boolean}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-construct
                                           :class-name "example/core$Response"
                                           :args [{:op :jvm-int
                                                   :value 7
                                                   :jvm-type :int}
                                                  {:op :jvm-string
                                                   :value "ok"
                                                   :jvm-type "java/lang/String"}]
                                           :jvm-type "example/core$Response"}
                                          {:op :jvm-if
                                           :test {:op :jvm-local
                                                  :name 'flag
                                                  :jvm-type :boolean}
                                           :then {:op :jvm-record-get
                                                  :target {:op :jvm-construct
                                                           :class-name "example/core$Response"
                                                           :args [{:op :jvm-int
                                                                   :value 1
                                                                   :jvm-type :int}
                                                                  {:op :jvm-string
                                                                   :value "yes"
                                                                   :jvm-type "java/lang/String"}]
                                                           :jvm-type "example/core$Response"}
                                                  :field 'status
                                                  :jvm-type :int}
                                           :else {:op :jvm-record-get
                                                  :target {:op :jvm-construct
                                                           :class-name "example/core$Response"
                                                           :args [{:op :jvm-int
                                                                   :value 2
                                                                   :jvm-type :int}
                                                                  {:op :jvm-string
                                                                   :value "no"
                                                                   :jvm-type "java/lang/String"}]
                                                           :jvm-type "example/core$Response"}
                                                  :field 'status
                                                  :jvm-type :int}
                                          :jvm-type :int}]
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers direct function calls and union variants"
    (let [module {:name 'example/calls
                  :imports []
                  :exports ['Result 'helper 'make-result]
                  :decls [{:op :union
                           :name 'Result
                           :type-params []
                           :invariants []
                           :variants [{:name 'Done
                                       :fields [{:name 'value :type 'Int}]}
                                      {:name 'Failed
                                       :fields [{:name 'message :type 'String}]}]}
                          {:op :fn
                           :name 'helper
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :local :name 'value}}
                          {:op :fn
                           :name 'make-result
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Result
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :variant
                                  :type 'Result
                                  :name 'Done
                                  :args [{:op :call
                                          :callee {:op :local :name 'helper}
                                          :args [{:op :local :name 'value}]}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/calls
                :internal-name "example/calls"
                :exports ['Result 'helper 'make-result]
                :records []
                :enums []
                :unions [{:name 'Result
                          :base-class "example/calls$Result"
                          :variants [{:name 'Done
                                      :class-name "example/calls$Result$Done"
                                      :fields [{:name 'value :jvm-type :int}]}
                                     {:name 'Failed
                                      :class-name "example/calls$Result$Failed"
                                      :fields [{:name 'message :jvm-type "java/lang/String"}]}]}]
                :methods [{:name 'helper
                           :owner "example/calls"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-local
                                  :name 'value
                                  :jvm-type :int}}
                          {:name 'make-result
                           :owner "example/calls"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type "example/calls$Result"
                           :effects []
                           :body {:op :jvm-variant
                                  :class-name "example/calls$Result$Done"
                                  :args [{:op :jvm-invoke-static
                                          :owner "example/calls"
                                          :name 'helper
                                          :parameter-types [:int]
                                          :return-type :int
                                          :args [{:op :jvm-local
                                                  :name 'value
                                                  :jvm-type :int}]
                                          :jvm-type :int}]
                                  :jvm-type "example/calls$Result"}}]}
               (sut/lower-module module))))

  (it "lowers direct local lambda calls by inlining their bodies"
    (let [module {:name 'example/lambdas
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'flag :type 'Bool}
                                    {:name 'x :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :let
                                  :bindings [{:name 'f
                                              :expr {:op :lambda
                                                     :params [{:name 'y :type 'Int}]
                                                     :return-type 'Int
                                                     :effects []
                                                     :body {:op :if
                                                            :test {:op :local :name 'flag}
                                                            :then {:op :local :name 'y}
                                                            :else 0}}}]
                                  :body {:op :call
                                         :callee {:op :local :name 'f}
                                         :args [{:op :local :name 'x}]}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/lambdas
                :internal-name "example/lambdas"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :methods [{:name 'program
                           :owner "example/lambdas"
                           :params [{:name 'flag :jvm-type :boolean}
                                    {:name 'x :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'y
                                              :expr {:op :jvm-local
                                                     :name 'x
                                                     :jvm-type :int}}]
                                  :body {:op :jvm-if
                                         :test {:op :jvm-local
                                                :name 'flag
                                                :jvm-type :boolean}
                                         :then {:op :jvm-local
                                                :name 'y
                                                :jvm-type :int}
                                         :else {:op :jvm-int
                                                :value 0
                                                :jvm-type :int}
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers direct literal lambda calls by inlining their bodies"
    (let [module {:name 'example/literal-lambdas
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
                                  :callee {:op :lambda
                                           :params [{:name 'y :type 'Int}]
                                           :return-type 'Int
                                           :effects []
                                           :body {:op :local :name 'y}}
                                  :args [{:op :local :name 'x}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/literal-lambdas
                :internal-name "example/literal-lambdas"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :methods [{:name 'program
                           :owner "example/literal-lambdas"
                           :params [{:name 'x :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'y
                                              :expr {:op :jvm-local
                                                     :name 'x
                                                     :jvm-type :int}}]
                                  :body {:op :jvm-local
                                         :name 'y
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers non-capturing lambda values into closure interfaces and classes"
    (let [module {:name 'example/closures
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'flag :type 'Bool}
                                    {:name 'x :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :let
                                  :bindings [{:name 'chooser
                                              :expr {:op :if
                                                     :test {:op :local :name 'flag}
                                                     :then {:op :lambda
                                                            :params [{:name 'y :type 'Int}]
                                                            :return-type 'Int
                                                            :effects []
                                                            :body {:op :local :name 'y}}
                                                     :else {:op :lambda
                                                            :params [{:name 'y :type 'Int}]
                                                            :return-type 'Int
                                                            :effects []
                                                            :body 0}}}]
                                  :body {:op :call
                                         :callee {:op :local :name 'chooser}
                                         :args [{:op :local :name 'x}]}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/closures
                :internal-name "example/closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/closures$Fn1$Int$Int"
                                      :method-name "apply"
                                      :params [:int]
                                      :return-type :int}]
                :closures [{:class-name "example/closures$Lambda$1"
                            :interface-name "example/closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'y
                                   :jvm-type :int}}
                           {:class-name "example/closures$Lambda$2"
                            :interface-name "example/closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-int
                                   :value 0
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/closures"
                           :params [{:name 'flag :jvm-type :boolean}
                                    {:name 'x :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'chooser
                                              :expr {:op :jvm-if
                                                     :test {:op :jvm-local
                                                            :name 'flag
                                                            :jvm-type :boolean}
                                                     :then {:op :jvm-closure-new
                                                            :class-name "example/closures$Lambda$1"
                                                            :args []
                                                            :jvm-type "example/closures$Fn1$Int$Int"}
                                                     :else {:op :jvm-closure-new
                                                            :class-name "example/closures$Lambda$2"
                                                            :args []
                                                            :jvm-type "example/closures$Fn1$Int$Int"}
                                                     :jvm-type "example/closures$Fn1$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'chooser
                                                  :jvm-type "example/closures$Fn1$Int$Int"}
                                         :interface-name "example/closures$Fn1$Int$Int"
                                         :method-name "apply"
                                         :parameter-types [:int]
                                         :return-type :int
                                         :args [{:op :jvm-local
                                                 :name 'x
                                                 :jvm-type :int}]
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers multi-arg lambda values into closure interfaces and classes"
    (let [module {:name 'example/multi-closures
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'x :type 'Int}
                                    {:name 'y :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :let
                                  :bindings [{:name 'chooser
                                              :expr {:op :if
                                                     :test true
                                                     :then {:op :lambda
                                                            :params [{:name 'a :type 'Int}
                                                                     {:name 'b :type 'Int}]
                                                            :return-type 'Int
                                                            :effects []
                                                            :body {:op :local :name 'a}}
                                                     :else {:op :lambda
                                                            :params [{:name 'a :type 'Int}
                                                                     {:name 'b :type 'Int}]
                                                            :return-type 'Int
                                                            :effects []
                                                            :body {:op :local :name 'b}}}}]
                                  :body {:op :call
                                         :callee {:op :local :name 'chooser}
                                         :args [{:op :local :name 'x}
                                                {:op :local :name 'y}]}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/multi-closures
                :internal-name "example/multi-closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/multi-closures$Fn2$Int$Int$Int"
                                      :method-name "apply"
                                      :params [:int :int]
                                      :return-type :int}]
                :closures [{:class-name "example/multi-closures$Lambda$1"
                            :interface-name "example/multi-closures$Fn2$Int$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'a :jvm-type :int}
                                     {:name 'b :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'a
                                   :jvm-type :int}}
                           {:class-name "example/multi-closures$Lambda$2"
                            :interface-name "example/multi-closures$Fn2$Int$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'a :jvm-type :int}
                                     {:name 'b :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'b
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/multi-closures"
                           :params [{:name 'x :jvm-type :int}
                                    {:name 'y :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'chooser
                                              :expr {:op :jvm-if
                                                     :test {:op :jvm-boolean
                                                            :value true
                                                            :jvm-type :boolean}
                                                     :then {:op :jvm-closure-new
                                                            :class-name "example/multi-closures$Lambda$1"
                                                            :args []
                                                            :jvm-type "example/multi-closures$Fn2$Int$Int$Int"}
                                                     :else {:op :jvm-closure-new
                                                            :class-name "example/multi-closures$Lambda$2"
                                                            :args []
                                                            :jvm-type "example/multi-closures$Fn2$Int$Int$Int"}
                                                     :jvm-type "example/multi-closures$Fn2$Int$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'chooser
                                                  :jvm-type "example/multi-closures$Fn2$Int$Int$Int"}
                                         :interface-name "example/multi-closures$Fn2$Int$Int$Int"
                                         :method-name "apply"
                                         :parameter-types [:int :int]
                                         :return-type :int
                                         :args [{:op :jvm-local
                                                 :name 'x
                                                 :jvm-type :int}
                                                {:op :jvm-local
                                                 :name 'y
                                                 :jvm-type :int}]
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers same-module function values into closure wrappers"
    (let [module {:name 'example/function-values
                  :imports []
                  :exports ['helper 'program]
                  :decls [{:op :fn
                           :name 'helper
                           :params [{:name 'a :type 'Int}
                                    {:name 'b :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :local :name 'a}}
                          {:op :fn
                           :name 'program
                           :params [{:name 'x :type 'Int}
                                    {:name 'y :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :let
                                  :bindings [{:name 'chooser
                                              :expr {:op :local :name 'helper}}]
                                  :body {:op :call
                                         :callee {:op :local :name 'chooser}
                                         :args [{:op :local :name 'x}
                                                {:op :local :name 'y}]}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/function-values
                :internal-name "example/function-values"
                :exports ['helper 'program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/function-values$Fn2$Int$Int$Int"
                                      :method-name "apply"
                                      :params [:int :int]
                                      :return-type :int}]
                :closures [{:class-name "example/function-values$Lambda$1"
                            :interface-name "example/function-values$Fn2$Int$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'a :jvm-type :int}
                                     {:name 'b :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-invoke-static
                                   :owner "example/function-values"
                                   :name 'helper
                                   :parameter-types [:int :int]
                                   :return-type :int
                                   :args [{:op :jvm-local
                                           :name 'a
                                           :jvm-type :int}
                                          {:op :jvm-local
                                           :name 'b
                                           :jvm-type :int}]
                                   :jvm-type :int}}]
                :methods [{:name 'helper
                           :owner "example/function-values"
                           :params [{:name 'a :jvm-type :int}
                                    {:name 'b :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-local
                                  :name 'a
                                  :jvm-type :int}}
                          {:name 'program
                           :owner "example/function-values"
                           :params [{:name 'x :jvm-type :int}
                                    {:name 'y :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'chooser
                                              :expr {:op :jvm-closure-new
                                                     :class-name "example/function-values$Lambda$1"
                                                     :args []
                                                     :jvm-type "example/function-values$Fn2$Int$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'chooser
                                                  :jvm-type "example/function-values$Fn2$Int$Int$Int"}
                                         :interface-name "example/function-values$Fn2$Int$Int$Int"
                                         :method-name "apply"
                                         :parameter-types [:int :int]
                                         :return-type :int
                                         :args [{:op :jvm-local
                                                 :name 'x
                                                 :jvm-type :int}
                                                {:op :jvm-local
                                                 :name 'y
                                                 :jvm-type :int}]
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers capturing lambda values into closure classes with captured args"
    (let [module {:name 'example/capturing-closures
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'x :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :let
                                  :bindings [{:name 'base
                                              :expr 5}
                                             {:name 'adder
                                              :expr {:op :if
                                                     :test true
                                                     :then {:op :lambda
                                                            :params [{:name 'y :type 'Int}]
                                                            :return-type 'Int
                                                            :effects []
                                                            :body {:op :if
                                                                   :test true
                                                                   :then {:op :local :name 'base}
                                                                   :else {:op :local :name 'y}}}
                                                     :else {:op :lambda
                                                            :params [{:name 'y :type 'Int}]
                                                            :return-type 'Int
                                                            :effects []
                                                            :body {:op :local :name 'y}}}}]
                                  :body {:op :call
                                         :callee {:op :local :name 'adder}
                                         :args [{:op :local :name 'x}]}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/capturing-closures
                :internal-name "example/capturing-closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/capturing-closures$Fn1$Int$Int"
                                      :method-name "apply"
                                      :params [:int]
                                      :return-type :int}]
                :closures [{:class-name "example/capturing-closures$Lambda$1"
                            :interface-name "example/capturing-closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures [{:name 'base
                                        :jvm-type :int}]
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-if
                                   :test {:op :jvm-boolean
                                          :value true
                                          :jvm-type :boolean}
                                   :then {:op :jvm-local
                                          :name 'base
                                          :jvm-type :int}
                                   :else {:op :jvm-local
                                          :name 'y
                                          :jvm-type :int}
                                   :jvm-type :int}}
                           {:class-name "example/capturing-closures$Lambda$2"
                            :interface-name "example/capturing-closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'y
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/capturing-closures"
                           :params [{:name 'x :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'base
                                              :expr {:op :jvm-int
                                                     :value 5
                                                     :jvm-type :int}}
                                             {:name 'adder
                                              :expr {:op :jvm-if
                                                     :test {:op :jvm-boolean
                                                            :value true
                                                            :jvm-type :boolean}
                                                     :then {:op :jvm-closure-new
                                                            :class-name "example/capturing-closures$Lambda$1"
                                                            :args [{:op :jvm-local
                                                                    :name 'base
                                                                    :jvm-type :int}]
                                                            :jvm-type "example/capturing-closures$Fn1$Int$Int"}
                                                     :else {:op :jvm-closure-new
                                                            :class-name "example/capturing-closures$Lambda$2"
                                                            :args []
                                                            :jvm-type "example/capturing-closures$Fn1$Int$Int"}
                                                     :jvm-type "example/capturing-closures$Fn1$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'adder
                                                  :jvm-type "example/capturing-closures$Fn1$Int$Int"}
                                         :interface-name "example/capturing-closures$Fn1$Int$Int"
                                         :method-name "apply"
                                         :parameter-types [:int]
                                         :return-type :int
                                         :args [{:op :jvm-local
                                                 :name 'x
                                                 :jvm-type :int}]
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers mutable captured locals into shared cell nodes"
    (let [module {:name 'example/mutable-capturing-closures
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params [{:name 'x :type 'Int}]
                           :return-type 'Int
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :var
                                           :name 'base
                                           :type 'Int
                                           :init 5}
                                          {:op :let
                                           :bindings [{:name 'adder
                                                       :expr {:op :if
                                                              :test true
                                                              :then {:op :lambda
                                                                     :params [{:name 'y :type 'Int}]
                                                                     :return-type 'Int
                                                                     :effects ['State.Write]
                                                                     :body {:op :local :name 'base}}
                                                              :else {:op :lambda
                                                                     :params [{:name 'y :type 'Int}]
                                                                     :return-type 'Int
                                                                     :effects ['State.Write]
                                                                     :body {:op :local :name 'y}}}}]
                                           :body {:op :seq
                                                  :exprs [{:op :set
                                                           :name 'base
                                                           :expr 9}
                                                          {:op :call
                                                           :callee {:op :local :name 'adder}
                                                           :args [{:op :local :name 'x}]}]}}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/mutable-capturing-closures
                :internal-name "example/mutable-capturing-closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/mutable-capturing-closures$Fn1$Int$Int"
                                      :method-name "apply"
                                      :params [:int]
                                      :return-type :int}]
                :closures [{:class-name "example/mutable-capturing-closures$Lambda$1"
                            :interface-name "example/mutable-capturing-closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures [{:name 'base
                                        :jvm-type "[I"}]
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-cell-get
                                   :name 'base
                                   :cell-jvm-type "[I"
                                   :jvm-type :int}}
                           {:class-name "example/mutable-capturing-closures$Lambda$2"
                            :interface-name "example/mutable-capturing-closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures []
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'y
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/mutable-capturing-closures"
                           :params [{:name 'x :jvm-type :int}]
                           :return-type :int
                           :effects ['State.Write]
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-var
                                           :name 'base
                                           :init {:op :jvm-int
                                                  :value 5
                                                  :jvm-type :int}
                                           :value-jvm-type :int
                                           :cell-jvm-type "[I"
                                           :jvm-type :void}
                                          {:op :jvm-let
                                           :bindings [{:name 'adder
                                                       :expr {:op :jvm-if
                                                              :test {:op :jvm-boolean
                                                                     :value true
                                                                     :jvm-type :boolean}
                                                              :then {:op :jvm-closure-new
                                                                     :class-name "example/mutable-capturing-closures$Lambda$1"
                                                                     :args [{:op :jvm-local
                                                                             :name 'base
                                                                             :jvm-type "[I"}]
                                                                     :jvm-type "example/mutable-capturing-closures$Fn1$Int$Int"}
                                                              :else {:op :jvm-closure-new
                                                                     :class-name "example/mutable-capturing-closures$Lambda$2"
                                                                     :args []
                                                                     :jvm-type "example/mutable-capturing-closures$Fn1$Int$Int"}
                                                              :jvm-type "example/mutable-capturing-closures$Fn1$Int$Int"}}]
                                           :body {:op :jvm-seq
                                                  :exprs [{:op :jvm-set
                                                           :name 'base
                                                           :expr {:op :jvm-int
                                                                  :value 9
                                                                  :jvm-type :int}
                                                           :value-jvm-type :int
                                                           :cell-jvm-type "[I"
                                                           :jvm-type :void}
                                                          {:op :jvm-closure-call
                                                           :callee {:op :jvm-local
                                                                    :name 'adder
                                                                    :jvm-type "example/mutable-capturing-closures$Fn1$Int$Int"}
                                                           :interface-name "example/mutable-capturing-closures$Fn1$Int$Int"
                                                           :method-name "apply"
                                                           :parameter-types [:int]
                                                           :return-type :int
                                                           :args [{:op :jvm-local
                                                                   :name 'x
                                                                   :jvm-type :int}]
                                                           :jvm-type :int}]
                                                  :jvm-type :int}
                                           :jvm-type :int}]
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers let bindings into JVM plan nodes"
    (let [module {:name 'example/lets
                  :imports []
                  :exports ['compute]
                  :decls [{:op :fn
                           :name 'compute
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :let
                                  :bindings [{:name 'x
                                              :expr {:op :local :name 'value}}
                                             {:name 'y
                                              :expr 9}]
                                  :body {:op :local :name 'x}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/lets
                :internal-name "example/lets"
                :exports ['compute]
                :records []
                :enums []
                :unions []
                :methods [{:name 'compute
                           :owner "example/lets"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'x
                                              :expr {:op :jvm-local
                                                     :name 'value
                                                     :jvm-type :int}}
                                             {:name 'y
                                              :expr {:op :jvm-int
                                                     :value 9
                                                     :jvm-type :int}}]
                                  :body {:op :jvm-local
                                         :name 'x
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers mutable locals into JVM plan nodes"
    (let [module {:name 'example/state
                  :imports []
                  :exports ['compute]
                  :decls [{:op :fn
                           :name 'compute
                           :params [{:name 'value :type 'Int}]
                           :return-type 'Int
                           :effects ['State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
                                  :exprs [{:op :var
                                           :name 'acc
                                           :type 'Int
                                           :init {:op :local :name 'value}}
                                          {:op :set
                                           :name 'acc
                                           :expr 9}
                                          {:op :local :name 'acc}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/state
                :internal-name "example/state"
                :exports ['compute]
                :records []
                :enums []
                :unions []
                :methods [{:name 'compute
                           :owner "example/state"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects ['State.Write]
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-var
                                           :name 'acc
                                           :init {:op :jvm-local
                                                  :name 'value
                                                  :jvm-type :int}
                                           :value-jvm-type :int
                                           :cell-jvm-type "[I"
                                           :jvm-type :void}
                                          {:op :jvm-set
                                           :name 'acc
                                           :expr {:op :jvm-int
                                                  :value 9
                                                  :jvm-type :int}
                                           :value-jvm-type :int
                                           :cell-jvm-type "[I"
                                           :jvm-type :void}
                                          {:op :jvm-cell-get
                                           :name 'acc
                                           :cell-jvm-type "[I"
                                           :jvm-type :int}]
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers loops and recur into JVM plan nodes"
    (let [module
          {:name 'example/looping
           :imports []
           :exports ['Step 'program]
           :decls [{:op :union
                    :name 'Step
                    :type-params []
                    :invariants []
                    :variants [{:name 'Continue
                                :fields [{:name 'value :type 'Int}]}
                               {:name 'Done
                                :fields [{:name 'value :type 'Int}]}]}
                   {:op :fn
                    :name 'program
                    :params []
                    :return-type 'Int
                    :effects []
                    :requires [true]
                    :ensures [true]
                    :body {:op :loop
                           :bindings [{:name 'state
                                       :expr {:op :variant
                                              :type 'Step
                                              :name 'Continue
                                              :args [3]}}]
                           :body {:op :match
                                  :target {:op :local :name 'state}
                                  :cases [{:pattern {:op :union-pattern
                                                     :name 'Continue
                                                     :args [{:op :binder-pattern
                                                             :name 'value}]}
                                           :body {:op :recur
                                                  :args [{:op :variant
                                                          :type 'Step
                                                          :name 'Done
                                                          :args [{:op :local
                                                                  :name 'value}]}]}}
                                          {:pattern {:op :union-pattern
                                                     :name 'Done
                                                     :args [{:op :binder-pattern
                                                             :name 'value}]}
                                           :body {:op :local
                                                  :name 'value}}]}}}]}
          ]
      (should= {:op :jvm-module
                :module-name 'example/looping
                :internal-name "example/looping"
                :exports ['Step 'program]
                :records []
                :enums []
                :unions [{:name 'Step
                          :base-class "example/looping$Step"
                          :variants [{:name 'Continue
                                      :class-name "example/looping$Step$Continue"
                                      :fields [{:name 'value :jvm-type :int}]}
                                     {:name 'Done
                                      :class-name "example/looping$Step$Done"
                                      :fields [{:name 'value :jvm-type :int}]}]}]
                :methods [{:name 'program
                           :owner "example/looping"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-loop
                                  :bindings [{:name 'state
                                              :init {:op :jvm-variant
                                                     :class-name "example/looping$Step$Continue"
                                                     :args [{:op :jvm-int
                                                             :value 3
                                                             :jvm-type :int}]
                                                     :jvm-type "example/looping$Step"}
                                              :jvm-type "example/looping$Step"}]
                                  :body {:op :jvm-match
                                         :target {:op :jvm-local
                                                  :name 'state
                                                  :jvm-type "example/looping$Step"}
                                         :cases [{:test {:op :jvm-instance-of
                                                         :target {:op :jvm-local
                                                                  :name 'state
                                                                  :jvm-type "example/looping$Step"}
                                                         :class-name "example/looping$Step$Continue"}
                                                  :bindings [{:name 'value
                                                              :expr {:op :jvm-variant-field
                                                                     :target {:op :jvm-local
                                                                              :name 'state
                                                                              :jvm-type "example/looping$Step"}
                                                                     :class-name "example/looping$Step$Continue"
                                                                     :field 'value
                                                                     :jvm-type :int}}]
                                                  :body {:op :jvm-recur
                                                         :args [{:op :jvm-variant
                                                                 :class-name "example/looping$Step$Done"
                                                                 :args [{:op :jvm-local
                                                                         :name 'value
                                                                         :jvm-type :int}]
                                                                 :jvm-type "example/looping$Step"}]
                                                         :jvm-type :void}}
                                                 {:test {:op :jvm-instance-of
                                                         :target {:op :jvm-local
                                                                  :name 'state
                                                                  :jvm-type "example/looping$Step"}
                                                         :class-name "example/looping$Step$Done"}
                                                  :bindings [{:name 'value
                                                              :expr {:op :jvm-variant-field
                                                                     :target {:op :jvm-local
                                                                              :name 'state
                                                                              :jvm-type "example/looping$Step"}
                                                                     :class-name "example/looping$Step$Done"
                                                                     :field 'value
                                                                     :jvm-type :int}}]
                                                  :body {:op :jvm-local
                                                         :name 'value
                                                         :jvm-type :int}}]
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers bottom-typed if expressions inside loops as void JVM nodes"
    (let [module {:name 'example/recur_if
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
                                  :bindings [{:name 'n
                                              :expr 1}]
                                  :body {:op :if
                                         :test true
                                         :then {:op :recur
                                                :args [{:op :local
                                                        :name 'n}]}
                                         :else {:op :recur
                                                :args [{:op :local
                                                        :name 'n}]}}}}]}]
      (should= :void
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body
                   :body
                   :jvm-type))))

  (it "lowers try catch finally and raise into JVM plan nodes"
    (let [module {:name 'example/raising
                  :imports []
                  :exports ['program]
                  :decls [{:op :fn
                           :name 'program
                           :params []
                           :return-type 'Int
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :try
                                  :body {:op :raise
                                         :expr {:op :java-new
                                                :class-name 'java.lang.RuntimeException
                                                :signature {:params ['String]}
                                                :args ["boom"]}}
                                  :catches [{:type '(Java java.lang.RuntimeException)
                                             :name 'ex
                                             :body 7}]
                                  :finally {:op :java-static-call
                                            :class-name 'java.lang.System
                                            :member-id 'gc
                                            :signature {:params []
                                                        :return-type 'Unit}
                                            :args []}}}]}]
      (should= {:op :jvm-module
                :module-name 'example/raising
                :internal-name "example/raising"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :methods [{:name 'program
                           :owner "example/raising"
                           :params []
                           :return-type :int
                           :effects ['Foreign.Throw]
                           :body {:op :jvm-try
                                  :body {:op :jvm-raise
                                         :expr {:op :jvm-java-new
                                                :class-name "java/lang/RuntimeException"
                                                :parameter-types ["java/lang/String"]
                                                :args [{:op :jvm-string
                                                        :value "boom"
                                                        :jvm-type "java/lang/String"}]
                                                :jvm-type "java/lang/RuntimeException"}
                                         :jvm-type :void}
                                  :catches [{:type "java/lang/RuntimeException"
                                             :name 'ex
                                             :body {:op :jvm-int
                                                    :value 7
                                                    :jvm-type :int}}]
                                  :finally {:op :jvm-java-static-call
                                            :class-name "java/lang/System"
                                            :member-id 'gc
                                            :parameter-types []
                                            :return-type :void
                                            :jvm-type :void
                                            :args []}
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers boolean matches into explicit JVM match cases"
    (let [module {:name 'example/bool-match
                  :imports []
                  :exports ['classify]
                  :decls [{:op :fn
                           :name 'classify
                           :params [{:name 'flag :type 'Bool}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'flag}
                                  :cases [{:pattern {:op :literal-pattern
                                                     :literal true}
                                           :body 1}
                                          {:pattern {:op :literal-pattern
                                                     :literal false}
                                           :body 0}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/bool-match
                :internal-name "example/bool-match"
                :exports ['classify]
                :records []
                :enums []
                :unions []
                :methods [{:name 'classify
                           :owner "example/bool-match"
                           :params [{:name 'flag :jvm-type :boolean}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-match
                                  :target {:op :jvm-local
                                           :name 'flag
                                           :jvm-type :boolean}
                                  :cases [{:test {:op :jvm-literal-test
                                                  :target {:op :jvm-local
                                                           :name 'flag
                                                           :jvm-type :boolean}
                                                  :literal {:op :jvm-boolean
                                                            :value true
                                                            :jvm-type :boolean}}
                                           :bindings []
                                           :body {:op :jvm-int
                                                  :value 1
                                                  :jvm-type :int}}
                                          {:test {:op :jvm-literal-test
                                                  :target {:op :jvm-local
                                                           :name 'flag
                                                           :jvm-type :boolean}
                                                  :literal {:op :jvm-boolean
                                                            :value false
                                                            :jvm-type :boolean}}
                                           :bindings []
                                           :body {:op :jvm-int
                                                  :value 0
                                                  :jvm-type :int}}]
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers union matches with binder payload extraction"
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
                                          {:pattern {:op :wildcard-pattern}
                                           :body 0}]}}]}]
      (should= {:op :jvm-module
                :module-name 'example/union-match
                :internal-name "example/union-match"
                :exports ['render]
                :records []
                :enums []
                :unions [{:name 'Response
                          :base-class "example/union-match$Response"
                          :variants [{:name 'Ok
                                      :class-name "example/union-match$Response$Ok"
                                      :fields [{:name 'value :jvm-type :int}]}
                                     {:name 'Error
                                      :class-name "example/union-match$Response$Error"
                                      :fields [{:name 'message :jvm-type "java/lang/String"}]}]}]
                :methods [{:name 'render
                           :owner "example/union-match"
                           :params [{:name 'response :jvm-type "example/union-match$Response"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-match
                                  :target {:op :jvm-local
                                           :name 'response
                                           :jvm-type "example/union-match$Response"}
                                  :cases [{:test {:op :jvm-instance-of
                                                  :target {:op :jvm-local
                                                           :name 'response
                                                           :jvm-type "example/union-match$Response"}
                                                  :class-name "example/union-match$Response$Ok"}
                                           :bindings [{:name 'value
                                                       :expr {:op :jvm-variant-field
                                                              :target {:op :jvm-local
                                                                       :name 'response
                                                                       :jvm-type "example/union-match$Response"}
                                                              :class-name "example/union-match$Response$Ok"
                                                              :field 'value
                                                              :jvm-type :int}}]
                                           :body {:op :jvm-local
                                                  :name 'value
                                                  :jvm-type :int}}
                                          {:test {:op :jvm-always-true}
                                           :bindings []
                                           :body {:op :jvm-int
                                                  :value 0
                                                  :jvm-type :int}}]
                                  :jvm-type :int}}]}
               (sut/lower-module module))))

  (it "lowers canonical JSON interchange primitives into JVM plan nodes"
    (let [interchange-interface {:name 'airj/core
                                 :imports []
                                 :exports ['Interchange]
                                 :decls [{:op :union
                                          :name 'Interchange
                                          :type-params []
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
                                                                :type '(Map String Interchange)}]}]}]}
          module {:name 'example/json_lower
                  :imports [{:op :airj-import
                             :module 'airj/core
                             :symbols ['Interchange]}]
                  :interfaces {'airj/core interchange-interface}
                  :exports ['roundtrip]
                  :decls [{:op :fn
                           :name 'roundtrip
                           :params [{:name 'text :type 'String}]
                           :return-type 'String
                           :effects ['Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :json-write
                                  :arg {:op :json-parse
                                        :arg {:op :local :name 'text}}}}]}]
      (should= {:op :jvm-json-write
                :arg {:op :jvm-json-parse
                      :arg {:op :jvm-local
                            :name 'text
                            :jvm-type "java/lang/String"}
                      :root-class-name "airj/core$Interchange"
                      :jvm-type "airj/core$Interchange"}
                :jvm-type "java/lang/String"}
               (-> (sut/lower-module module)
                   :methods
                   first
                   :body))))
