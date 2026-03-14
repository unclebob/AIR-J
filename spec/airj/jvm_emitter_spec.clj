(ns airj.jvm-emitter-spec
  (:require [airj.jvm-emitter :as sut]
            [clojure.java.io :as io]
            [speclj.core :refer :all])
  (:import (clojure.asm MethodVisitor Opcodes)))

(defn- define-class
  [class-name bytecode]
  (let [loader (clojure.lang.DynamicClassLoader.)]
    (.defineClass loader class-name bytecode nil)))

(defn- binary-name
  [internal-name]
  (.replace ^String internal-name \/ \.))

(defn- define-classes
  [bytecode-map]
  (let [loader (clojure.lang.DynamicClassLoader.)]
    (into {}
          (map (fn [[internal-name bytecode]]
                 [internal-name
                  (.defineClass loader (binary-name internal-name) bytecode nil)]))
          (sort-by key bytecode-map))))

(defn- compile-java-helper!
  [class-name source]
  (let [compiler (javax.tools.ToolProvider/getSystemJavaCompiler)
        output-dir (str (java.nio.file.Files/createTempDirectory
                         "airj-java-helper"
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        source-file (java.io.File.
                     output-dir
                     (str (.replace ^String class-name \. \/) ".java"))
        loader (java.net.URLClassLoader.
                (into-array java.net.URL [(.toURL (.toURI (java.io.File. output-dir)))]))]
    (io/make-parents source-file)
    (spit source-file source)
    (should= 0
             (.run compiler nil nil nil
                   (into-array String ["-d" output-dir (.getPath source-file)])))
    (.loadClass loader class-name)))

(describe "JVM emission"
  (it "emits a module class whose static method returns an int literal"
    (let [plan {:op :jvm-module
                :module-name 'example/emit
                :internal-name "example/emit"
                :exports ['forty-two]
                :records []
                :enums []
                :unions []
                :methods [{:name 'forty-two
                           :owner "example/emit"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-int
                                  :value 42
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.emit" bytes)
          method (.getMethod klass "forty_two" (into-array Class []))]
      (should= 42 (.invoke method nil (object-array [])))))

  (it "emits a module class whose static method returns an int parameter"
    (let [plan {:op :jvm-module
                :module-name 'example/identity
                :internal-name "example/identity"
                :exports ['identity-int]
                :records []
                :enums []
                :unions []
                :methods [{:name 'identity-int
                           :owner "example/identity"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-local
                                  :name 'value
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.identity" bytes)
          method (.getMethod klass "identity_int" (into-array Class [Integer/TYPE]))]
      (should= 7 (.invoke method nil (object-array [(int 7)])))))

  (it "emits module methods that return Float and Double literals"
    (let [plan {:op :jvm-module
                :module-name 'example/literals
                :internal-name "example/literals"
                :exports ['sample-float 'sample-double]
                :records []
                :enums []
                :unions []
                :methods [{:name 'sample-float
                           :owner "example/literals"
                           :params []
                           :return-type :float
                           :effects []
                           :body {:op :jvm-float
                                  :value (float 1.25)
                                  :jvm-type :float}}
                          {:name 'sample-double
                           :owner "example/literals"
                           :params []
                           :return-type :double
                           :effects []
                           :body {:op :jvm-double
                                  :value 1.25
                                  :jvm-type :double}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.literals" bytes)
          float-method (.getMethod klass "sample_float" (into-array Class []))
          double-method (.getMethod klass "sample_double" (into-array Class []))]
      (should (< (Math/abs (- (float (.invoke float-method nil (object-array []))) (float 1.25))) 1.0e-5))
      (should (< (Math/abs (- (double (.invoke double-method nil (object-array []))) 1.25)) 1.0e-9))))

  (it "rejects emission for an unknown local slot"
    (let [plan {:op :jvm-module
                :module-name 'example/bad_local
                :internal-name "example/bad_local"
                :exports ['broken]
                :records []
                :enums []
                :unions []
                :methods [{:name 'broken
                           :owner "example/bad_local"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-local
                                  :name 'missing
                                  :jvm-type :int}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unknown emitted local."
                    (sut/emit-module-bytes plan))))

  (it "emits static AIR-J calls between module methods"
    (let [plan {:op :jvm-module
                :module-name 'example/calls
                :internal-name "example/calls"
                :exports ['helper 'call-helper]
                :records []
                :enums []
                :unions []
                :methods [{:name 'helper
                           :owner "example/calls"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-local
                                  :name 'value
                                  :jvm-type :int}}
                          {:name 'call-helper
                           :owner "example/calls"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-invoke-static
                                  :owner "example/calls"
                                  :name 'helper
                                  :parameter-types [:int]
                                  :return-type :int
                                  :args [{:op :jvm-local
                                          :name 'value
                                          :jvm-type :int}]
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.calls" bytes)
          method (.getMethod klass "call_helper" (into-array Class [Integer/TYPE]))]
      (should= 9 (.invoke method nil (object-array [(int 9)])))))

  (it "emits Java static calls"
    (let [plan {:op :jvm-module
                :module-name 'example/java
                :internal-name "example/java"
                :exports ['abs]
                :records []
                :enums []
                :unions []
                :methods [{:name 'abs
                           :owner "example/java"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :int
                           :effects ['Foreign.Throw]
                           :body {:op :jvm-java-static-call
                                  :class-name "java/lang/Math"
                                  :member-id 'abs
                                  :parameter-types [:int]
                                  :return-type :int
                                  :args [{:op :jvm-local
                                          :name 'value
                                          :jvm-type :int}]}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.java" bytes)
          method (.getMethod klass "abs" (into-array Class [Integer/TYPE]))]
      (should= 4 (.invoke method nil (object-array [(int -4)])))))

  (it "emits floating-point primitives and conversions"
    (let [plan {:op :jvm-module
                :module-name 'example/floating
                :internal-name "example/floating"
                :exports ['orbit_step]
                :records []
                :enums []
                :unions []
                :methods [{:name 'orbit-step
                           :owner "example/floating"
                           :params [{:name 'phase :jvm-type :double}
                                    {:name 'scale :jvm-type :float}]
                           :return-type :float
                           :effects ['Foreign.Throw]
                           :body {:op :jvm-double->float
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
                                  :jvm-type :float}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.floating" bytes)
          method (.getMethod klass "orbit_step" (into-array Class [Double/TYPE Float/TYPE]))
          result (.invoke method nil (object-array [(double 0.0) (float 4.0)]))]
      (should (< (Math/abs (- (double result) 3.0)) 1.0e-5))))

  (it "emits floating-point comparisons"
    (let [plan {:op :jvm-module
                :module-name 'example/floating_compare
                :internal-name "example/floating_compare"
                :exports ['less]
                :records []
                :enums []
                :unions []
                :methods [{:name 'less
                           :owner "example/floating_compare"
                           :params [{:name 'left :jvm-type :double}
                                    {:name 'right :jvm-type :double}]
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-double-lt
                                  :args [{:op :jvm-local
                                          :name 'left
                                          :jvm-type :double}
                                         {:op :jvm-local
                                          :name 'right
                                          :jvm-type :double}]
                                  :jvm-type :boolean}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.floating_compare" bytes)
          method (.getMethod klass "less" (into-array Class [Double/TYPE Double/TYPE]))]
      (should= true (.invoke method nil (object-array [(double 1.5) (double 2.5)])))
      (should= false (.invoke method nil (object-array [(double 3.5) (double 2.5)])))))

  (it "discards double intermediates with POP2"
    (let [plan {:op :jvm-module
                :module-name 'example/discard_double
                :internal-name "example/discard_double"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :methods [{:name 'program
                           :owner "example/discard_double"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-double-add
                                           :args [{:op :jvm-double
                                                   :value 1.25
                                                   :jvm-type :double}
                                                  {:op :jvm-double
                                                   :value 2.75
                                                   :jvm-type :double}]
                                           :jvm-type :double}
                                          {:op :jvm-int
                                           :value 7
                                           :jvm-type :int}]
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.discard_double" bytes)
          method (.getMethod klass "program" (into-array Class []))]
      (should= 7 (.invoke method nil (object-array [])))))

  (it "emits Java instance calls and field access"
    (let [plan {:op :jvm-module
                :module-name 'example/java_instance
                :internal-name "example/java_instance"
                :exports ['interop]
                :records []
                :enums []
                :unions []
                :methods [{:name 'interop
                           :owner "example/java_instance"
                           :params []
                           :return-type :int
                           :effects ['State.Write 'Foreign.Throw]
                           :body {:op :jvm-seq
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
                                           :jvm-type :int}
                                          {:op :jvm-int
                                           :value 0
                                           :jvm-type :int}]
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.java_instance" bytes)
          method (.getMethod klass "interop" (into-array Class []))]
      (should= 0 (.invoke method nil (object-array [])))))

  (it "emits Java static field assignment"
    (let [helper-class (compile-java-helper! "example.StaticBox"
                                             "package example; public class StaticBox { public static java.io.PrintStream value; }")
          value-field (.getField helper-class "value")
          loader (clojure.lang.DynamicClassLoader. (.getClassLoader helper-class))
          plan {:op :jvm-module
                :module-name 'example/java_static_field_write
                :internal-name "example/java_static_field_write"
                :exports ['swap]
                :records []
                :enums []
                :unions []
                :methods [{:name 'swap
                           :owner "example/java_static_field_write"
                           :params [{:name 'stream
                                     :jvm-type "java/io/PrintStream"}]
                           :return-type "java/io/PrintStream"
                           :effects ['State.Write]
                           :body {:op :jvm-seq
                                   :exprs [{:op :jvm-java-static-set-field
                                           :class-name "example/StaticBox"
                                           :field-name 'value
                                           :field-type "java/io/PrintStream"
                                           :expr {:op :jvm-local
                                                  :name 'stream
                                                  :jvm-type "java/io/PrintStream"}
                                           :jvm-type :void}
                                          {:op :jvm-java-static-get-field
                                           :class-name "example/StaticBox"
                                           :field-name 'value
                                           :field-type "java/io/PrintStream"
                                           :jvm-type "java/io/PrintStream"}]
                                  :jvm-type "java/io/PrintStream"}}]}
          bytes (sut/emit-module-bytes plan)
          klass (.defineClass loader "example.java_static_field_write" bytes nil)
          method (.getMethod klass "swap" (into-array Class [java.io.PrintStream]))
          replacement (java.io.PrintStream. (java.io.ByteArrayOutputStream.))]
      (should= "example.java_static_field_write" (.getName klass))
      (should= "swap" (.getName method))
      (should= replacement (.invoke method nil (object-array [replacement])))
      (should= replacement (.get value-field nil))))

  (it "emits helper casts and unboxes for erased object fields"
    (let [emit-object-cast-or-unbox @#'airj.jvm-emitter/emit-object-cast-or-unbox
          emit-cast-or-unbox @#'airj.jvm-emitter/emit-cast-or-unbox
          recorder (fn [calls]
                     (proxy [MethodVisitor] [393216]
                       (visitTypeInsn [opcode type]
                         (swap! calls conj [:type opcode type]))
                       (visitMethodInsn [opcode owner name descriptor is-interface]
                         (swap! calls conj [:method opcode owner name descriptor is-interface]))))
          float-calls (atom [])
          double-calls (atom [])
          cast-calls (atom [])]
      (emit-object-cast-or-unbox (recorder float-calls) :float)
      (should= [[:type Opcodes/CHECKCAST "java/lang/Float"]
                [:method Opcodes/INVOKEVIRTUAL "java/lang/Float" "floatValue" "()F" false]]
               @float-calls)
      (emit-object-cast-or-unbox (recorder double-calls) :double)
      (should= [[:type Opcodes/CHECKCAST "java/lang/Double"]
                [:method Opcodes/INVOKEVIRTUAL "java/lang/Double" "doubleValue" "()D" false]]
               @double-calls)
      (emit-cast-or-unbox (recorder cast-calls) "java/lang/Number" "java/lang/String")
      (should= [[:type Opcodes/CHECKCAST "java/lang/String"]]
               @cast-calls)))

  (it "emits a host-backed module class with instance bridge methods"
    (let [plan {:op :jvm-module
                :module-name 'example/hosted
                :internal-name "example/hosted"
                :host {:class-name "java/util/ArrayList"}
                :exports ['snapshot]
                :records []
                :enums []
                :unions []
                :methods [{:name 'snapshot
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
                :instance-methods [{:name 'snapshot
                                    :owner "example/hosted"
                                    :params []
                                    :return-type :int
                                    :target {:name 'snapshot
                                             :owner "example/hosted"
                                             :parameter-types ["java/util/ArrayList"]
                                             :return-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.hosted" bytes)
          ctor (.getConstructor klass (into-array Class []))
          instance (.newInstance ctor (object-array []))
          add-method (.getMethod klass "add" (into-array Class [Object]))
          snapshot-method (.getMethod klass "snapshot" (into-array Class []))]
      (.invoke add-method instance (object-array ["a"]))
      (.invoke add-method instance (object-array ["b"]))
      (should= 2 (.invoke snapshot-method instance (object-array [])))
      (should= java.util.ArrayList (.getSuperclass klass))))

  (it "emits host-backed instance bridge methods with parameters"
    (let [plan {:op :jvm-module
                :module-name 'example/hosted_args
                :internal-name "example/hosted_args"
                :host {:class-name "java/util/ArrayList"}
                :exports ['snapshot_plus]
                :records []
                :enums []
                :unions []
                :methods [{:name 'snapshot_plus
                           :owner "example/hosted_args"
                           :params [{:name 'self
                                     :jvm-type "java/util/ArrayList"}
                                    {:name 'delta
                                     :jvm-type :int}]
                           :return-type :int
                           :effects ['Foreign.Throw]
                           :body {:op :jvm-int-add
                                  :args [{:op :jvm-java-call
                                          :target {:op :jvm-local
                                                   :name 'self
                                                   :jvm-type "java/util/ArrayList"}
                                          :member-id 'size
                                          :parameter-types []
                                          :return-type :int
                                          :args []
                                          :jvm-type :int}
                                         {:op :jvm-local
                                          :name 'delta
                                          :jvm-type :int}]
                                  :jvm-type :int}}]
                :instance-methods [{:name 'snapshot_plus
                                    :owner "example/hosted_args"
                                    :params [{:name 'delta
                                              :jvm-type :int}]
                                    :return-type :int
                                    :target {:name 'snapshot_plus
                                             :owner "example/hosted_args"
                                             :parameter-types ["java/util/ArrayList" :int]
                                             :return-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.hosted_args" bytes)
          ctor (.getConstructor klass (into-array Class []))
          instance (.newInstance ctor (object-array []))
          add-method (.getMethod klass "add" (into-array Class [Object]))
          snapshot-plus-method (.getMethod klass "snapshot_plus" (into-array Class [Integer/TYPE]))]
      (.invoke add-method instance (object-array ["earth"]))
      (.invoke add-method instance (object-array ["mars"]))
      (should= 7 (.invoke snapshot-plus-method instance (object-array [(int 5)])))))

  (it "emits simple boolean conditionals"
    (let [plan {:op :jvm-module
                :module-name 'example/branch
                :internal-name "example/branch"
                :exports ['choose]
                :records []
                :enums []
                :unions []
                :methods [{:name 'choose
                           :owner "example/branch"
                           :params [{:name 'flag :jvm-type :boolean}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-if
                                  :test {:op :jvm-local
                                         :name 'flag
                                         :jvm-type :boolean}
                                  :then {:op :jvm-int
                                         :value 1
                                         :jvm-type :int}
                                  :else {:op :jvm-int
                                         :value 0
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.branch" bytes)
          method (.getMethod klass "choose" (into-array Class [Boolean/TYPE]))]
      (should= 1 (.invoke method nil (object-array [true])))
      (should= 0 (.invoke method nil (object-array [false])))))

  (it "emits primitive arithmetic and logical operators"
    (let [plan {:op :jvm-module
                :module-name 'example/operators
                :internal-name "example/operators"
                :exports ['compute]
                :records []
                :enums []
                :unions []
                :methods [{:name 'compute
                           :owner "example/operators"
                           :params [{:name 'x :jvm-type :int}
                                    {:name 'y :jvm-type :int}
                                    {:name 'flag :jvm-type :boolean}]
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-bool-or
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
                                  :jvm-type :boolean}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.operators" bytes)
          method (.getMethod klass "compute" (into-array Class [Integer/TYPE Integer/TYPE Boolean/TYPE]))]
      (should= true (.invoke method nil (object-array [(int 3) (int 4) false])))
      (should= false (.invoke method nil (object-array [(int 8) (int 5) true])))))

  (it "emits additional comparisons and boolean equality"
    (let [plan {:op :jvm-module
                :module-name 'example/more_operators
                :internal-name "example/more_operators"
                :exports ['compare]
                :records []
                :enums []
                :unions []
                :methods [{:name 'compare
                           :owner "example/more_operators"
                           :params [{:name 'x :jvm-type :int}
                                    {:name 'y :jvm-type :int}]
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-bool-eq
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
                                  :jvm-type :boolean}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.more_operators" bytes)
          method (.getMethod klass "compare" (into-array Class [Integer/TYPE Integer/TYPE]))]
      (should= true (.invoke method nil (object-array [(int 4) (int 4)])))
      (should= false (.invoke method nil (object-array [(int 2) (int 9)])))))

  (it "emits conversion equality and stdout output"
    (let [plan {:op :jvm-module
                :module-name 'example/io_ops
                :internal-name "example/io_ops"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :methods [{:name 'program
                           :owner "example/io_ops"
                           :params [{:name 'x :jvm-type :int}
                                    {:name 'label :jvm-type "java/lang/String"}]
                           :return-type :boolean
                           :effects ['Stdout.Write]
                           :body {:op :jvm-seq
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
                                  :jvm-type :boolean}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.io_ops" bytes)
          method (.getMethod klass "program" (into-array Class [Integer/TYPE String]))
          out-bytes (java.io.ByteArrayOutputStream.)
          original-out (java.lang.System/out)]
      (try
        (java.lang.System/setOut (java.io.PrintStream. out-bytes true "UTF-8"))
        (should= true (.invoke method nil (object-array [(int 7) "ok"])))
        (should= "7\n" (.toString out-bytes "UTF-8"))
        (finally
          (java.lang.System/setOut original-out)))))

  (it "emits stdin stdout string operations and fallible conversion"
    (let [plan {:op :jvm-module
                :module-name 'example/text_io
                :internal-name "example/text_io"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :methods [{:name 'program
                           :owner "example/text_io"
                           :params []
                           :return-type :int
                           :effects ['Foreign.Throw 'Stdin.Read 'Stdout.Write]
                           :body {:op :jvm-seq
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
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.text_io" bytes)
          method (.getMethod klass "program" (into-array Class []))
          out-bytes (java.io.ByteArrayOutputStream.)
          original-out (java.lang.System/out)
          original-in (java.lang.System/in)]
      (try
        (java.lang.System/setOut (java.io.PrintStream. out-bytes true "UTF-8"))
        (java.lang.System/setIn (java.io.ByteArrayInputStream. (.getBytes "abc\n" "UTF-8")))
        (should= 9 (.invoke method nil (object-array [])))
        (should= ">abc" (.toString out-bytes "UTF-8"))
        (finally
          (java.lang.System/setOut original-out)
          (java.lang.System/setIn original-in)))))

  (it "emits string sequence primitives"
    (let [plan {:op :jvm-module
                :module-name 'example/text_seq
                :internal-name "example/text_seq"
                :exports ['metric]
                :records []
                :enums []
                :unions []
                :methods [{:name 'metric
                           :owner "example/text_seq"
                           :params [{:name 'line :jvm-type "java/lang/String"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-if
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
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.text_seq" bytes)
          method (.getMethod klass "metric" (into-array Class [String]))]
      (should= 2 (.invoke method nil (object-array [" a,bb "])))
      (should= 0 (.invoke method nil (object-array ["   "])))))

  (it "emits sequence length primitives"
    (let [plan {:op :jvm-module
                :module-name 'example/seq_length
                :internal-name "example/seq_length"
                :exports ['count_parts]
                :records []
                :enums []
                :unions []
                :methods [{:name 'count_parts
                           :owner "example/seq_length"
                           :params [{:name 'line :jvm-type "java/lang/String"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq-length
                                  :arg {:op :jvm-string-split-on
                                        :args [{:op :jvm-local
                                                :name 'line
                                                :jvm-type "java/lang/String"}
                                               {:op :jvm-string
                                                :value ","
                                                :jvm-type "java/lang/String"}]
                                        :jvm-type "java/util/List"}
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.seq_length" bytes)
          method (.getMethod klass "count_parts" (into-array Class [String]))]
      (should= 3 (.invoke method nil (object-array ["a,b,c"])))
      (should= 1 (.invoke method nil (object-array ["solo"])))))

  (it "emits substring char-at and first/empty sequence primitives"
    (let [plan {:op :jvm-module
                :module-name 'example/text_scan
                :internal-name "example/text_scan"
                :exports ['token]
                :records []
                :enums []
                :unions []
                :methods [{:name 'token
                           :owner "example/text_scan"
                           :params [{:name 'line :jvm-type "java/lang/String"}]
                           :return-type "java/lang/String"
                           :effects []
                           :body {:op :jvm-if
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
                                  :jvm-type "java/lang/String"}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.text_scan" bytes)
          method (.getMethod klass "token" (into-array Class [String]))]
      (should= "b" (.invoke method nil (object-array ["abz,qq"])))
      (should= "" (.invoke method nil (object-array [","])))))

  (it "emits typed sequence element casts for erased list values"
    (let [plan {:op :jvm-module
                :module-name 'example/typed_seq
                :internal-name "example/typed_seq"
                :exports ['first_int 'truthy 'first_double 'first_float]
                :records []
                :enums []
                :unions []
                :methods [{:name 'first_int
                           :owner "example/typed_seq"
                           :params [{:name 'items
                                     :jvm-type "java/util/List"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq-get
                                  :args [{:op :jvm-local
                                          :name 'items
                                          :jvm-type "java/util/List"}
                                         {:op :jvm-int
                                          :value 0
                                          :jvm-type :int}]
                                  :jvm-type :int}}
                          {:name 'truthy
                           :owner "example/typed_seq"
                           :params [{:name 'items
                                     :jvm-type "java/util/List"}]
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-seq-first
                                  :arg {:op :jvm-local
                                        :name 'items
                                        :jvm-type "java/util/List"}
                                  :jvm-type :boolean}}
                          {:name 'first_double
                           :owner "example/typed_seq"
                           :params [{:name 'items
                                     :jvm-type "java/util/List"}]
                           :return-type :double
                           :effects []
                           :body {:op :jvm-seq-first
                                  :arg {:op :jvm-local
                                        :name 'items
                                        :jvm-type "java/util/List"}
                                  :jvm-type :double}}
                          {:name 'first_float
                           :owner "example/typed_seq"
                           :params [{:name 'items
                                     :jvm-type "java/util/List"}]
                           :return-type :float
                           :effects []
                           :body {:op :jvm-seq-first
                                  :arg {:op :jvm-local
                                        :name 'items
                                        :jvm-type "java/util/List"}
                                  :jvm-type :float}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.typed_seq" bytes)
          first-int (.getMethod klass "first_int" (into-array Class [java.util.List]))
          truthy (.getMethod klass "truthy" (into-array Class [java.util.List]))
          first-double (.getMethod klass "first_double" (into-array Class [java.util.List]))
          first-float (.getMethod klass "first_float" (into-array Class [java.util.List]))]
      (should= 7 (.invoke first-int nil (object-array [(java.util.Arrays/asList (into-array Object [(int 7)]))])))
      (should= true (.invoke truthy nil (object-array [(java.util.Arrays/asList (into-array Object [true]))])))
      (should= 2.5 (.invoke first-double nil (object-array [(java.util.Arrays/asList (into-array Object [(double 2.5)]))])))
      (should= (float 3.5) (.invoke first-float nil (object-array [(java.util.Arrays/asList (into-array Object [(float 3.5)]))])))))

  (it "emits legacy array-backed sequence operations for StringSeq entry arguments"
    (let [plan {:op :jvm-module
                :module-name 'example/array_seq
                :internal-name "example/array_seq"
                :exports ['token_size]
                :records []
                :enums []
                :unions []
                :methods [{:name 'token_size
                           :owner "example/array_seq"
                           :params [{:name 'args
                                     :jvm-type "[Ljava/lang/String;"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-if
                                  :test {:op :jvm-seq-empty
                                         :arg {:op :jvm-local
                                               :name 'args
                                               :jvm-type "[Ljava/lang/String;"}
                                         :jvm-type :boolean}
                                  :then {:op :jvm-int
                                         :value 0
                                         :jvm-type :int}
                                  :else {:op :jvm-string-length
                                         :arg {:op :jvm-seq-first
                                               :arg {:op :jvm-local
                                                     :name 'args
                                                     :jvm-type "[Ljava/lang/String;"}
                                               :jvm-type "java/lang/String"}
                                         :jvm-type :int}
                                  :jvm-type :int}}
                          {:name 'pick
                           :owner "example/array_seq"
                           :params [{:name 'args
                                     :jvm-type "[Ljava/lang/String;"}]
                           :return-type "java/lang/String"
                           :effects []
                           :body {:op :jvm-seq-get
                                  :args [{:op :jvm-local
                                          :name 'args
                                          :jvm-type "[Ljava/lang/String;"}
                                         {:op :jvm-int
                                          :value 1
                                          :jvm-type :int}]
                                  :jvm-type "java/lang/String"}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.array_seq" bytes)
          token-size (.getMethod klass "token_size" (into-array Class [(class (into-array String []))]))
          pick (.getMethod klass "pick" (into-array Class [(class (into-array String []))]))]
      (should= 0 (.invoke token-size nil (object-array [(into-array String [])])))
      (should= 3 (.invoke token-size nil (object-array [(into-array String ["abc" "de"])])))
      (should= "de" (.invoke pick nil (object-array [(into-array String ["abc" "de"])])))))

  (it "emits legacy array-backed sequence length rest and concat operations"
    (let [plan {:op :jvm-module
                :module-name 'example/array_seq_more
                :internal-name "example/array_seq_more"
                :exports ['arg_count 'tail_first 'combined_count]
                :records []
                :enums []
                :unions []
                :methods [{:name 'arg_count
                           :owner "example/array_seq_more"
                           :params [{:name 'args
                                     :jvm-type "[Ljava/lang/String;"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq-length
                                  :arg {:op :jvm-local
                                        :name 'args
                                        :jvm-type "[Ljava/lang/String;"}
                                  :jvm-type :int}}
                          {:name 'tail_first
                           :owner "example/array_seq_more"
                           :params [{:name 'args
                                     :jvm-type "[Ljava/lang/String;"}]
                           :return-type "java/lang/String"
                           :effects []
                           :body {:op :jvm-seq-first
                                  :arg {:op :jvm-seq-rest
                                        :arg {:op :jvm-local
                                              :name 'args
                                              :jvm-type "[Ljava/lang/String;"}
                                        :jvm-type "java/util/List"}
                                  :jvm-type "java/lang/String"}}
                          {:name 'combined_count
                           :owner "example/array_seq_more"
                           :params [{:name 'args
                                     :jvm-type "[Ljava/lang/String;"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq-length
                                  :arg {:op :jvm-seq-concat
                                        :args [{:op :jvm-local
                                                :name 'args
                                                :jvm-type "[Ljava/lang/String;"}
                                               {:op :jvm-string-split-on
                                                :args [{:op :jvm-string
                                                        :value "tail,more"
                                                        :jvm-type "java/lang/String"}
                                                       {:op :jvm-string
                                                        :value ","
                                                        :jvm-type "java/lang/String"}]
                                                :jvm-type "java/util/List"}]
                                        :jvm-type "java/util/List"}
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.array_seq_more" bytes)
          arg-count (.getMethod klass "arg_count" (into-array Class [(class (into-array String []))]))
          tail-first (.getMethod klass "tail_first" (into-array Class [(class (into-array String []))]))
          combined-count (.getMethod klass "combined_count" (into-array Class [(class (into-array String []))]))]
      (should= 2 (.invoke arg-count nil (object-array [(into-array String ["abc" "de"])])))
      (should= "de" (.invoke tail-first nil (object-array [(into-array String ["abc" "de"])])))
      (should= 4 (.invoke combined-count nil (object-array [(into-array String ["abc" "de"])])))))

  (it "emits static Java field access"
    (let [plan {:op :jvm-module
                :module-name 'example/java_static_field
                :internal-name "example/java_static_field"
                :exports ['interop]
                :records []
                :enums []
                :unions []
                :methods [{:name 'interop
                           :owner "example/java_static_field"
                           :params []
                           :return-type "java/io/PrintStream"
                           :effects []
                           :body {:op :jvm-java-static-get-field
                                  :class-name "java/lang/System"
                                  :field-name 'out
                                  :field-type "java/io/PrintStream"
                                  :jvm-type "java/io/PrintStream"}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.java_static_field" bytes)
          method (.getMethod klass "interop" (into-array Class []))]
      (should (instance? java.io.PrintStream
                         (.invoke method nil (object-array []))))))

  (it "emits record classes and module methods that construct and read them"
    (let [plan {:op :jvm-module
                :module-name 'example/records
                :internal-name "example/records"
                :exports ['status]
                :records [{:name 'Response
                           :class-name "example/records$Response"
                           :fields [{:name 'status :jvm-type :int}
                                    {:name 'body :jvm-type "java/lang/String"}]}]
                :enums []
                :unions []
                :methods [{:name 'status
                           :owner "example/records"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-construct
                                           :class-name "example/records$Response"
                                           :args [{:op :jvm-int
                                                   :value 7
                                                   :jvm-type :int}
                                                  {:op :jvm-string
                                                   :value "ok"
                                                   :jvm-type "java/lang/String"}]
                                           :jvm-type "example/records$Response"}
                                          {:op :jvm-record-get
                                           :target {:op :jvm-construct
                                                    :class-name "example/records$Response"
                                                    :args [{:op :jvm-int
                                                            :value 9
                                                            :jvm-type :int}
                                                           {:op :jvm-string
                                                            :value "body"
                                                            :jvm-type "java/lang/String"}]
                                                    :jvm-type "example/records$Response"}
                                           :field 'status
                                           :jvm-type :int}]}}]}
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/records")
          record-class (get classes "example/records$Response")
          method (.getMethod module-class "status" (into-array Class []))
          ctor (.getConstructor record-class (into-array Class [Integer/TYPE String]))
          instance (.newInstance ctor (object-array [(int 3) "x"]))
          field (.getField record-class "status")]
      (should= #{"example/records" "example/records$Response"} (set (keys classes)))
      (should= 9 (.invoke method nil (object-array [])))
      (should= 3 (.get field instance))))

  (it "emits let bindings in module methods"
    (let [plan {:op :jvm-module
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
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.lets" bytes)
          method (.getMethod klass "compute" (into-array Class [Integer/TYPE]))]
      (should= 5 (.invoke method nil (object-array [(int 5)])))))

  (it "emits mutable locals in module methods"
    (let [plan {:op :jvm-module
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
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.state" bytes)
          method (.getMethod klass "compute" (into-array Class [Integer/TYPE]))]
      (should= 9 (.invoke method nil (object-array [(int 5)])))))

  (it "emits record field reads that unbox float values stored as Object"
    (let [plan {:op :jvm-module
                :module-name 'example/object_box
                :internal-name "example/object_box"
                :exports ['weight]
                :records [{:name 'Box
                           :class-name "example/object_box$Box"
                           :fields [{:name 'value :jvm-type "java/lang/Object"}]}]
                :enums []
                :unions []
                :methods [{:name 'weight
                           :owner "example/object_box"
                           :params []
                           :return-type :float
                           :effects []
                           :body {:op :jvm-record-get
                                  :target {:op :jvm-construct
                                           :class-name "example/object_box$Box"
                                           :parameter-types ["java/lang/Object"]
                                           :args [{:op :jvm-float
                                                   :value (float 2.5)
                                                   :jvm-type :float}]
                                           :jvm-type "example/object_box$Box"}
                                  :field 'value
                                  :field-jvm-type "java/lang/Object"
                                  :jvm-type :float}}]}
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/object_box")
          method (.getMethod module-class "weight" (into-array Class []))]
      (should= (float 2.5) (.invoke method nil (object-array [])))))

  (it "emits mutable JVM locals without cell backing"
    (let [plan {:op :jvm-module
                :module-name 'example/plain_state
                :internal-name "example/plain_state"
                :exports ['run]
                :records []
                :enums []
                :unions []
                :methods [{:name 'run
                           :owner "example/plain_state"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-var
                                           :name 'total
                                           :init {:op :jvm-int
                                                  :value 1
                                                  :jvm-type :int}
                                           :jvm-type :void}
                                          {:op :jvm-set
                                           :name 'total
                                           :expr {:op :jvm-int
                                                  :value 4
                                                  :jvm-type :int}
                                           :jvm-type :void}
                                          {:op :jvm-local
                                           :name 'total
                                           :jvm-type :int}]
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.plain_state" bytes)
          method (.getMethod klass "run" (into-array Class []))]
      (should= 4 (.invoke method nil (object-array [])))))

  (it "emits boxed primitive map operations and key enumeration"
    (let [plan {:op :jvm-module
                :module-name 'example/maps
                :internal-name "example/maps"
                :exports ['int_key? 'bool_key? 'float_key? 'double_key? 'key_count]
                :records []
                :enums []
                :unions []
                :methods [{:name 'int_key?
                           :owner "example/maps"
                           :params []
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-map-contains
                                  :args [{:op :jvm-map-set
                                          :args [{:op :jvm-map-empty
                                                  :jvm-type "java/util/Map"}
                                                 {:op :jvm-string
                                                  :value "i"
                                                  :jvm-type "java/lang/String"}
                                                 {:op :jvm-int
                                                  :value 3
                                                  :jvm-type :int}]
                                          :jvm-type "java/util/Map"}
                                         {:op :jvm-string
                                          :value "i"
                                          :jvm-type "java/lang/String"}]
                                  :jvm-type :boolean}}
                          {:name 'bool_key?
                           :owner "example/maps"
                           :params []
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-map-contains
                                  :args [{:op :jvm-map-set
                                          :args [{:op :jvm-map-empty
                                                  :jvm-type "java/util/Map"}
                                                 {:op :jvm-string
                                                  :value "b"
                                                  :jvm-type "java/lang/String"}
                                                 {:op :jvm-boolean
                                                  :value true
                                                  :jvm-type :boolean}]
                                          :jvm-type "java/util/Map"}
                                         {:op :jvm-string
                                          :value "b"
                                          :jvm-type "java/lang/String"}]
                                  :jvm-type :boolean}}
                          {:name 'float_key?
                           :owner "example/maps"
                           :params []
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-map-contains
                                  :args [{:op :jvm-map-set
                                          :args [{:op :jvm-map-empty
                                                  :jvm-type "java/util/Map"}
                                                 {:op :jvm-string
                                                  :value "f"
                                                  :jvm-type "java/lang/String"}
                                                 {:op :jvm-float
                                                  :value (float 1.5)
                                                  :jvm-type :float}]
                                          :jvm-type "java/util/Map"}
                                         {:op :jvm-string
                                          :value "f"
                                          :jvm-type "java/lang/String"}]
                                  :jvm-type :boolean}}
                          {:name 'double_key?
                           :owner "example/maps"
                           :params []
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-map-contains
                                  :args [{:op :jvm-map-set
                                          :args [{:op :jvm-map-empty
                                                  :jvm-type "java/util/Map"}
                                                 {:op :jvm-string
                                                  :value "d"
                                                  :jvm-type "java/lang/String"}
                                                 {:op :jvm-double
                                                  :value 2.5
                                                  :jvm-type :double}]
                                          :jvm-type "java/util/Map"}
                                         {:op :jvm-string
                                          :value "d"
                                          :jvm-type "java/lang/String"}]
                                  :jvm-type :boolean}}
                          {:name 'key_count
                           :owner "example/maps"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-seq-length
                                  :arg {:op :jvm-map-keys
                                        :arg {:op :jvm-map-set
                                              :args [{:op :jvm-map-empty
                                                      :jvm-type "java/util/Map"}
                                                     {:op :jvm-string
                                                      :value "k"
                                                      :jvm-type "java/lang/String"}
                                                     {:op :jvm-string
                                                      :value "v"
                                                      :jvm-type "java/lang/String"}]
                                              :jvm-type "java/util/Map"}
                                        :jvm-type "java/util/List"}
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.maps" bytes)
          int-key? (.getMethod klass "int_key?" (into-array Class []))
          bool-key? (.getMethod klass "bool_key?" (into-array Class []))
          float-key? (.getMethod klass "float_key?" (into-array Class []))
          double-key? (.getMethod klass "double_key?" (into-array Class []))
          key-count (.getMethod klass "key_count" (into-array Class []))]
      (should= true (.invoke int-key? nil (object-array [])))
      (should= true (.invoke bool-key? nil (object-array [])))
      (should= true (.invoke float-key? nil (object-array [])))
      (should= true (.invoke double-key? nil (object-array [])))
      (should= 1 (.invoke key-count nil (object-array [])))))

  (it "emits literal tests"
    (let [plan {:op :jvm-module
                :module-name 'example/literal_test
                :internal-name "example/literal_test"
                :exports ['matches]
                :records []
                :enums []
                :unions []
                :methods [{:name 'matches
                           :owner "example/literal_test"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type :boolean
                           :effects []
                           :body {:op :jvm-literal-test
                                  :target {:op :jvm-local
                                           :name 'value
                                           :jvm-type :int}
                                  :literal {:op :jvm-int
                                            :value 7
                                            :jvm-type :int}
                                  :jvm-type :boolean}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.literal_test" bytes)
          method (.getMethod klass "matches" (into-array Class [Integer/TYPE]))]
      (should= true (.invoke method nil (object-array [(int 7)])))
      (should= false (.invoke method nil (object-array [(int 3)])))))

  (it "rejects recur outside a loop"
    (let [plan {:op :jvm-module
                :module-name 'example/bad_recur
                :internal-name "example/bad_recur"
                :exports ['broken]
                :records []
                :enums []
                :unions []
                :methods [{:name 'broken
                           :owner "example/bad_recur"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-recur
                                  :args [{:op :jvm-int
                                          :value 1
                                          :jvm-type :int}]
                                  :jvm-type :int}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Recur emitted outside loop."
                    (sut/emit-module-bytes plan))))

  (it "rejects unsupported JVM emission expressions"
    (let [plan {:op :jvm-module
                :module-name 'example/bad_expr
                :internal-name "example/bad_expr"
                :exports ['broken]
                :records []
                :enums []
                :unions []
                :methods [{:name 'broken
                           :owner "example/bad_expr"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-unknown
                                  :jvm-type :int}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unsupported JVM emission expression."
                    (sut/emit-module-bytes plan))))

  (it "emits loops and recur in module methods"
    (let [plan {:op :jvm-module
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
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/looping")
          method (.getMethod module-class "program" (into-array Class []))]
      (should= 3 (.invoke method nil (object-array [])))))

  (it "emits non-capturing closure interfaces and calls"
    (let [plan {:op :jvm-module
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
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'y
                                   :jvm-type :int}}
                           {:class-name "example/closures$Lambda$2"
                            :interface-name "example/closures$Fn1$Int$Int"
                            :method-name "apply"
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
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/closures")
          method (.getMethod module-class "program" (into-array Class [Boolean/TYPE Integer/TYPE]))]
      (should= 8 (.invoke method nil (object-array [true (int 8)])))
      (should= 0 (.invoke method nil (object-array [false (int 8)])))))

  (it "emits multi-arg closure interfaces and calls"
    (let [plan {:op :jvm-module
                :module-name 'example/multi_closures
                :internal-name "example/multi_closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/multi_closures$Fn2$Int$Int$Int"
                                      :method-name "apply"
                                      :params [:int :int]
                                      :return-type :int}]
                :closures [{:class-name "example/multi_closures$Lambda$1"
                            :interface-name "example/multi_closures$Fn2$Int$Int$Int"
                            :method-name "apply"
                            :params [{:name 'a :jvm-type :int}
                                     {:name 'b :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'a
                                   :jvm-type :int}}
                           {:class-name "example/multi_closures$Lambda$2"
                            :interface-name "example/multi_closures$Fn2$Int$Int$Int"
                            :method-name "apply"
                            :params [{:name 'a :jvm-type :int}
                                     {:name 'b :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-local
                                   :name 'b
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/multi_closures"
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
                                                            :class-name "example/multi_closures$Lambda$1"
                                                            :args []
                                                            :jvm-type "example/multi_closures$Fn2$Int$Int$Int"}
                                                     :else {:op :jvm-closure-new
                                                            :class-name "example/multi_closures$Lambda$2"
                                                            :args []
                                                            :jvm-type "example/multi_closures$Fn2$Int$Int$Int"}
                                                     :jvm-type "example/multi_closures$Fn2$Int$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'chooser
                                                  :jvm-type "example/multi_closures$Fn2$Int$Int$Int"}
                                         :interface-name "example/multi_closures$Fn2$Int$Int$Int"
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
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/multi_closures")
          method (.getMethod module-class "program" (into-array Class [Integer/TYPE Integer/TYPE]))]
      (should= 7 (.invoke method nil (object-array [(int 7) (int 9)])))))

  (it "emits same-module function values as closure wrappers"
    (let [plan {:op :jvm-module
                :module-name 'example/function_values
                :internal-name "example/function_values"
                :exports ['helper 'program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/function_values$Fn2$Int$Int$Int"
                                      :method-name "apply"
                                      :params [:int :int]
                                      :return-type :int}]
                :closures [{:class-name "example/function_values$Lambda$1"
                            :interface-name "example/function_values$Fn2$Int$Int$Int"
                            :method-name "apply"
                            :params [{:name 'a :jvm-type :int}
                                     {:name 'b :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-invoke-static
                                   :owner "example/function_values"
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
                           :owner "example/function_values"
                           :params [{:name 'a :jvm-type :int}
                                    {:name 'b :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-local
                                  :name 'a
                                  :jvm-type :int}}
                          {:name 'program
                           :owner "example/function_values"
                           :params [{:name 'x :jvm-type :int}
                                    {:name 'y :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'chooser
                                              :expr {:op :jvm-closure-new
                                                     :class-name "example/function_values$Lambda$1"
                                                     :args []
                                                     :jvm-type "example/function_values$Fn2$Int$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'chooser
                                                  :jvm-type "example/function_values$Fn2$Int$Int$Int"}
                                         :interface-name "example/function_values$Fn2$Int$Int$Int"
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
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/function_values")
          method (.getMethod module-class "program" (into-array Class [Integer/TYPE Integer/TYPE]))]
      (should= 7 (.invoke method nil (object-array [(int 7) (int 9)])))))

  (it "emits capturing closure interfaces and calls"
    (let [plan {:op :jvm-module
                :module-name 'example/capturing_closures
                :internal-name "example/capturing_closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/capturing_closures$Fn1$Int$Int"
                                      :method-name "apply"
                                      :params [:int]
                                      :return-type :int}]
                :closures [{:class-name "example/capturing_closures$Lambda$1"
                            :interface-name "example/capturing_closures$Fn1$Int$Int"
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
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/capturing_closures"
                           :params [{:name 'x :jvm-type :int}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-let
                                  :bindings [{:name 'base
                                              :expr {:op :jvm-int
                                                     :value 5
                                                     :jvm-type :int}}
                                             {:name 'adder
                                              :expr {:op :jvm-closure-new
                                                     :class-name "example/capturing_closures$Lambda$1"
                                                     :args [{:op :jvm-local
                                                             :name 'base
                                                             :jvm-type :int}]
                                                     :jvm-type "example/capturing_closures$Fn1$Int$Int"}}]
                                  :body {:op :jvm-closure-call
                                         :callee {:op :jvm-local
                                                  :name 'adder
                                                  :jvm-type "example/capturing_closures$Fn1$Int$Int"}
                                         :interface-name "example/capturing_closures$Fn1$Int$Int"
                                         :method-name "apply"
                                         :parameter-types [:int]
                                         :return-type :int
                                         :args [{:op :jvm-local
                                                 :name 'x
                                                 :jvm-type :int}]
                                         :jvm-type :int}
                                  :jvm-type :int}}]}
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/capturing_closures")
          method (.getMethod module-class "program" (into-array Class [Integer/TYPE]))]
      (should= 5 (.invoke method nil (object-array [(int 8)])))))

  (it "emits mutable capturing closures with shared cell state"
    (let [plan {:op :jvm-module
                :module-name 'example/mutable_capturing_closures
                :internal-name "example/mutable_capturing_closures"
                :exports ['program]
                :records []
                :enums []
                :unions []
                :closure-interfaces [{:class-name "example/mutable_capturing_closures$Fn1$Int$Int"
                                      :method-name "apply"
                                      :params [:int]
                                      :return-type :int}]
                :closures [{:class-name "example/mutable_capturing_closures$Lambda$1"
                            :interface-name "example/mutable_capturing_closures$Fn1$Int$Int"
                            :method-name "apply"
                            :captures [{:name 'base
                                        :jvm-type "[I"}]
                            :params [{:name 'y :jvm-type :int}]
                            :return-type :int
                            :body {:op :jvm-cell-get
                                   :name 'base
                                   :cell-jvm-type "[I"
                                   :jvm-type :int}}]
                :methods [{:name 'program
                           :owner "example/mutable_capturing_closures"
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
                                                       :expr {:op :jvm-closure-new
                                                              :class-name "example/mutable_capturing_closures$Lambda$1"
                                                              :args [{:op :jvm-local
                                                                      :name 'base
                                                                      :jvm-type "[I"}]
                                                              :jvm-type "example/mutable_capturing_closures$Fn1$Int$Int"}}]
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
                                                                    :jvm-type "example/mutable_capturing_closures$Fn1$Int$Int"}
                                                           :interface-name "example/mutable_capturing_closures$Fn1$Int$Int"
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
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/mutable_capturing_closures")
          method (.getMethod module-class "program" (into-array Class [Integer/TYPE]))]
      (should= 9 (.invoke method nil (object-array [(int 8)])))))

  (it "emits try catch finally and raise in module methods"
    (let [property-name "airj.jvm-emitter.finally"
          _ (System/clearProperty property-name)
          plan {:op :jvm-module
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
                                  :finally {:op :jvm-seq
                                            :exprs [{:op :jvm-java-static-call
                                                     :class-name "java/lang/System"
                                                     :member-id 'setProperty
                                                     :parameter-types ["java/lang/String"
                                                                       "java/lang/String"]
                                                     :return-type "java/lang/String"
                                                     :args [{:op :jvm-string
                                                             :value property-name
                                                             :jvm-type "java/lang/String"}
                                                            {:op :jvm-string
                                                             :value "done"
                                                             :jvm-type "java/lang/String"}]}
                                                    {:op :jvm-int
                                                     :value 0
                                                     :jvm-type :int}]
                                            :jvm-type :int}
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.raising" bytes)
          method (.getMethod klass "program" (into-array Class []))]
      (should= 7 (.invoke method nil (object-array [])))
      (should= "done" (System/getProperty property-name))
      (System/clearProperty property-name)))

  (it "emits union classes and module methods that construct and match variants"
    (let [plan {:op :jvm-module
                :module-name 'example/unions
                :internal-name "example/unions"
                :exports ['make 'render]
                :records []
                :enums []
                :unions [{:name 'Result
                          :base-class "example/unions$Result"
                          :variants [{:name 'Done
                                      :class-name "example/unions$Result$Done"
                                      :fields [{:name 'value :jvm-type :int}]}
                                     {:name 'Failed
                                      :class-name "example/unions$Result$Failed"
                                      :fields [{:name 'message :jvm-type "java/lang/String"}]}]}]
                :methods [{:name 'make
                           :owner "example/unions"
                           :params [{:name 'value :jvm-type :int}]
                           :return-type "example/unions$Result"
                           :effects []
                           :body {:op :jvm-variant
                                  :class-name "example/unions$Result$Done"
                                  :args [{:op :jvm-local
                                          :name 'value
                                          :jvm-type :int}]
                                  :jvm-type "example/unions$Result"}}
                          {:name 'render
                           :owner "example/unions"
                           :params [{:name 'result :jvm-type "example/unions$Result"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-match
                                  :target {:op :jvm-local
                                           :name 'result
                                           :jvm-type "example/unions$Result"}
                                  :cases [{:test {:op :jvm-instance-of
                                                  :target {:op :jvm-local
                                                           :name 'result
                                                           :jvm-type "example/unions$Result"}
                                                  :class-name "example/unions$Result$Done"}
                                           :bindings [{:name 'value
                                                       :expr {:op :jvm-variant-field
                                                              :target {:op :jvm-local
                                                                       :name 'result
                                                                       :jvm-type "example/unions$Result"}
                                                              :class-name "example/unions$Result$Done"
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
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/unions")
          base-class (get classes "example/unions$Result")
          done-class (get classes "example/unions$Result$Done")
          make-method (.getMethod module-class "make" (into-array Class [Integer/TYPE]))
          render-method (.getMethod module-class "render" (into-array Class [base-class]))
          value-field (.getField done-class "value")
          result (.invoke make-method nil (object-array [(int 12)]))]
      (should= #{"example/unions" "example/unions$Result" "example/unions$Result$Done" "example/unions$Result$Failed"}
                (set (keys classes)))
      (should= 12 (.get value-field result))
      (should= 12 (.invoke render-method nil (object-array [result])))))

  (it "emits a standard JVM main wrapper when AIR-J exports main"
    (let [plan {:op :jvm-module
                :module-name 'example/entry
                :internal-name "example/entry"
                :exports ['main]
                :records []
                :enums []
                :unions []
                :methods [{:name 'main
                           :owner "example/entry"
                           :params []
                           :return-type :int
                           :effects []
                           :body {:op :jvm-int
                                  :value 42
                                  :jvm-type :int}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.entry" bytes)
          airj-main (.getMethod klass "airj_main" (into-array Class []))
          jvm-main (.getMethod klass "main" (into-array Class [(class (into-array String []))]))]
      (should= 42 (.invoke airj-main nil (object-array [])))
      (should jvm-main)))

  (it "passes JVM args into AIR-J main when it declares a String array parameter"
    (let [plan {:op :jvm-module
                :module-name 'example/entry-args
                :internal-name "example/entry_args"
                :exports ['main]
                :records []
                :enums []
                :unions []
                :methods [{:name 'main
                           :owner "example/entry_args"
                           :params [{:name 'args
                                     :jvm-type "[Ljava/lang/String;"}]
                           :return-type :int
                           :effects []
                           :body {:op :jvm-java-static-call
                                  :class-name "java/util/Arrays"
                                  :member-id 'hashCode
                                  :parameter-types ["[Ljava/lang/Object;"]
                                  :return-type :int
                                  :args [{:op :jvm-local
                                          :name 'args
                                          :jvm-type "[Ljava/lang/String;"}]}}]}
          bytes (sut/emit-module-bytes plan)
          klass (define-class "example.entry_args" bytes)
          airj-main (.getMethod klass "airj_main" (into-array Class [(class (into-array String []))]))
          jvm-main (.getMethod klass "main" (into-array Class [(class (into-array String []))]))
          args (into-array String ["a" "b"])
          expected (java.util.Arrays/hashCode (object-array ["a" "b"]))]
      (should= (java.util.Arrays/hashCode (into-array Object ["a" "b"]))
                (.invoke airj-main nil (object-array [args])))
      (should= expected (.invoke airj-main nil (object-array [args])))
      (should jvm-main)))

  (it "emits host runtime environment and process helpers"
    (let [plan {:op :jvm-module
                :module-name 'example/host_runtime
                :internal-name "example/host_runtime"
                :exports ['run]
                :records [{:name 'ProcessResult
                           :class-name "example/host_runtime$ProcessResult"
                           :fields [{:name 'exit-code :jvm-type :int}
                                    {:name 'stdout :jvm-type "[B"}
                                    {:name 'stderr :jvm-type "[B"}]}]
                :enums []
                :unions [{:name 'Option
                          :base-class "example/host_runtime$Option"
                          :variants [{:name 'None
                                      :class-name "example/host_runtime$Option$None"
                                      :fields []}
                                     {:name 'Some
                                      :class-name "example/host_runtime$Option$Some"
                                      :fields [{:name 'value
                                                :jvm-type "java/lang/Object"}]}]}]
                :methods [{:name 'run
                           :owner "example/host_runtime"
                           :params [{:name 'name :jvm-type "java/lang/String"}
                                    {:name 'command :jvm-type "java/util/List"}
                                    {:name 'stdin :jvm-type "[B"}]
                           :return-type "example/host_runtime$ProcessResult"
                           :effects ['Env.Read 'Process.Run 'Foreign.Throw]
                           :body {:op :jvm-seq
                                  :exprs [{:op :jvm-env-get
                                           :arg {:op :jvm-local
                                                 :name 'name
                                                 :jvm-type "java/lang/String"}
                                           :root-class-name "example/host_runtime$Option"
                                           :jvm-type "example/host_runtime$Option"}
                                          {:op :jvm-process-run
                                           :args [{:op :jvm-local
                                                   :name 'command
                                                   :jvm-type "java/util/List"}
                                                  {:op :jvm-local
                                                   :name 'stdin
                                                   :jvm-type "[B"}]
                                           :root-class-name "example/host_runtime$ProcessResult"
                                           :jvm-type "example/host_runtime$ProcessResult"}]
                                  :jvm-type "example/host_runtime$ProcessResult"}}]}
          classes (define-classes (sut/emit-class-bytes plan))
          module-class (get classes "example/host_runtime")
          method (.getMethod module-class
                             "run"
                             (into-array Class [String java.util.List (Class/forName "[B")]))
          command (java.util.ArrayList.)
          _ (.add command "/bin/cat")
          result (.invoke method nil (object-array ["PATH" command (.getBytes "{\"tool\":\"ok\"}" "UTF-8")]))
          fields (vec (.getFields (class result)))
          exit-code (.get (first (filter #(= Integer/TYPE (.getType %)) fields)) result)
          byte-values (map #(.get % result)
                           (filter #(= (Class/forName "[B") (.getType %)) fields))]
      (should= 0 exit-code)
      (should= #{"{\"tool\":\"ok\"}" ""}
               (set (map #(String. ^bytes % "UTF-8") byte-values))))))
