(ns airj.jvm-emitter-spec
  (:require [airj.jvm-emitter :as sut]
            [speclj.core :refer :all]))

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
      (should jvm-main))))
