(ns airj.compiler-spec
  (:require [airj.compiler :as sut]
            [clojure.java.io :as io]
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

(defn- load-class-from-dir
  [output-dir class-name]
  (let [url (.toURL (.toURI (io/file output-dir)))
        loader (java.net.URLClassLoader.
                (into-array java.net.URL [url]))]
    (.loadClass loader class-name)))

(defn- java-command
  []
  (str (System/getProperty "java.home")
       java.io.File/separator
       "bin"
       java.io.File/separator
       "java"))

(defn- run-main-process
  [output-dir class-name & args]
  (let [process (.start (ProcessBuilder.
                         (into-array String
                                     (concat [(java-command) "-cp" output-dir class-name]
                                             args))))]
    (.waitFor process)))

(describe "compiler"
  (it "compiles AIR-J source into executable module class bytes"
    (let [source "(module example/build
                    (imports)
                    (export forty-two)
                    (fn forty-two
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      42))"
          bundle (sut/compile-source source)
          klass (define-class "example.build" (get bundle "example/build"))
          method (.getMethod klass "forty_two" (into-array Class []))]
      (should= #{"example/build"} (set (keys bundle)))
      (should= 42 (.invoke method nil (object-array [])))))

  (it "compiles AIR-J source with Java static calls into executable bytes"
    (let [source "(module example/java-abs
                    (imports
                      (java java.lang.Math))
                    (export abs)
                    (fn abs
                      (params (value Int))
                      (returns Int)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
          (java/static-call
                        java.lang.Math
                        abs
                        (signature (Int) Int)
                        (local value))))"
          bundle (sut/compile-source source)
          klass (define-class "example.java-abs" (get bundle "example/java-abs"))
          method (.getMethod klass "abs" (into-array Class [Integer/TYPE]))]
      (should= 4 (.invoke method nil (object-array [(int -4)])))))

  (it "compiles AIR-J source with Java instance calls and fields into executable bytes"
    (let [source "(module example/java_instance
                    (imports
                      (java java.lang.StringBuilder)
                      (java java.awt.Point))
                    (export interop)
                    (fn interop
                      (params)
                      (returns Int)
                      (effects (State.Write Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (java/call
                          (java/new java.lang.StringBuilder \"a\")
                          append
                          (signature (String) (Java java.lang.StringBuilder))
                          \"b\")
                        (java/set-field
                          (java/new java.awt.Point 1 2)
                          x
                          Int
                          9)
                        (java/get-field
                          (java/new java.awt.Point 1 2)
                          x
                          Int))))"
          bundle (sut/compile-source source)
          klass (define-class "example.java_instance" (get bundle "example/java_instance"))
          method (.getMethod klass "interop" (into-array Class []))]
      (should= 1 (.invoke method nil (object-array [])))))

  (it "compiles a host-backed module into a Java subclass with instance callbacks"
    (let [source "(module example/hosted
                    (host java.util.ArrayList)
                    (imports
                      (java java.util.ArrayList))
                    (export snapshot)
                    (fn snapshot
                      (params (self (Java java.util.ArrayList)))
                      (returns Int)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (java/call
                        (local self)
                        size
                        (signature () Int))))"
          bundle (sut/compile-source source)
          klass (define-class "example.hosted" (get bundle "example/hosted"))
          ctor (.getConstructor klass (into-array Class []))
          instance (.newInstance ctor (object-array []))
          add-method (.getMethod klass "add" (into-array Class [Object]))
          snapshot-method (.getMethod klass "snapshot" (into-array Class []))]
      (.invoke add-method instance (object-array ["earth"]))
      (.invoke add-method instance (object-array ["mars"]))
      (should= 2 (.invoke snapshot-method instance (object-array [])))
      (should= java.util.ArrayList (.getSuperclass klass))))

  (it "compiles AIR-J source with simple conditionals into executable bytes"
    (let [source "(module example/choose
                    (imports)
                    (export choose)
                    (fn choose
                      (params (flag Bool))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (if
                        (local flag)
                        1
                        0)))"
          bundle (sut/compile-source source)
          klass (define-class "example.choose" (get bundle "example/choose"))
          method (.getMethod klass "choose" (into-array Class [Boolean/TYPE]))]
      (should= 1 (.invoke method nil (object-array [true])))
      (should= 0 (.invoke method nil (object-array [false])))))

  (it "compiles AIR-J source with primitive operators into executable bytes"
    (let [source "(module example/operators
                    (imports)
                    (export compute)
                    (fn compute
                      (params (x Int) (y Int) (flag Bool))
                      (returns Bool)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (bool-or
                        (bool-not (local flag))
                        (bool-and
                          (int-lt (int-add (local x) (local y))
                                  (int-mul 10 (int-sub (local y) 1)))
                          (int-eq (int-mod (local x) 2)
                                  (int-div (local y) 2))))))"
          bundle (sut/compile-source source)
          klass (define-class "example.operators" (get bundle "example/operators"))
          method (.getMethod klass "compute" (into-array Class [Integer/TYPE Integer/TYPE Boolean/TYPE]))]
      (should= true (.invoke method nil (object-array [(int 3) (int 4) false])))
      (should= false (.invoke method nil (object-array [(int 8) (int 5) true])))))

  (it "compiles AIR-J source with additional comparisons into executable bytes"
    (let [source "(module example/more-operators
                    (imports)
                    (export compare)
                    (fn compare
                      (params (x Int) (y Int))
                      (returns Bool)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (bool-eq
                        (int-ge (local x) (local y))
                        (bool-and
                          (int-gt (local x) 0)
                          (int-le (local x) (local y))))))"
          bundle (sut/compile-source source)
          klass (define-class "example.more-operators" (get bundle "example/more-operators"))
          method (.getMethod klass "compare" (into-array Class [Integer/TYPE Integer/TYPE]))]
      (should= true (.invoke method nil (object-array [(int 4) (int 4)])))
      (should= false (.invoke method nil (object-array [(int 2) (int 9)])))))

  (it "compiles AIR-J source with floating-point operators and Java double interop"
    (let [source "(module example/floating
                    (imports
                      (java java.lang.Math))
                    (export orbit-step)
                    (fn orbit-step
                      (params (phase Double) (scale Float))
                      (returns Float)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (double->float
                        (double-add
                          (java/static-call java.lang.Math cos (signature (Double) Double) (local phase))
                          (float->double
                            (float-div (local scale) (int->float 2)))))))"
          bundle (sut/compile-source source)
          klass (define-class "example.floating" (get bundle "example/floating"))
          method (.getMethod klass "orbit_step" (into-array Class [Double/TYPE Float/TYPE]))
          result (.invoke method nil (object-array [(double 0.0) (float 4.0)]))]
      (should (< (Math/abs (- (double result) 3.0)) 1.0e-5))))

  (it "compiles AIR-J source with conversion equality and stdout output"
    (let [source "(module example/io-ops
                    (imports)
                    (export program)
                    (fn program
                      (params (x Int) (label String))
                      (returns Bool)
                      (effects (Stdout.Write))
                      (requires true)
                      (ensures true)
                      (seq
                        (io/println (int->string (local x)))
                        (string-eq (local label) \"ok\")
                        (int-ne (local x) 0))))"
          bundle (sut/compile-source source)
          klass (define-class "example.io-ops" (get bundle "example/io-ops"))
          method (.getMethod klass "program" (into-array Class [Integer/TYPE String]))
          out-bytes (java.io.ByteArrayOutputStream.)
          out-stream (java.io.PrintStream. out-bytes true "UTF-8")
          original-out (java.lang.System/out)]
      (try
        (java.lang.System/setOut out-stream)
        (should= true (.invoke method nil (object-array [(int 7) "ok"])))
        (should= "7\n" (.toString out-bytes "UTF-8"))
        (finally
          (java.lang.System/setOut original-out)))))

  (it "compiles AIR-J source with stdin stdout string operations and fallible conversion"
    (let [source "(module example/text-io
                    (imports)
                    (export program)
                    (fn program
                      (params)
                      (returns Int)
                      (effects (Foreign.Throw Stdin.Read Stdout.Write))
                      (requires true)
                      (ensures true)
                      (seq
                        (io/print (string-concat \">\" (io/read-line)))
                        (int-add
                          (string-length \"ab\")
                          (string->int \"7\")))))"
          bundle (sut/compile-source source)
          klass (define-class "example.text-io" (get bundle "example/text-io"))
          method (.getMethod klass "program" (into-array Class []))
          out-bytes (java.io.ByteArrayOutputStream.)
          out-stream (java.io.PrintStream. out-bytes true "UTF-8")
          original-out (java.lang.System/out)
          original-in (java.lang.System/in)]
      (try
        (java.lang.System/setOut out-stream)
        (java.lang.System/setIn (java.io.ByteArrayInputStream. (.getBytes "abc\n" "UTF-8")))
        (should= 9 (.invoke method nil (object-array [])))
        (should= ">abc" (.toString out-bytes "UTF-8"))
        (finally
          (java.lang.System/setOut original-out)
          (java.lang.System/setIn original-in)))))

  (it "compiles AIR-J source with string sequence primitives"
    (let [source "(module example/text-seq
                    (imports)
                    (export metric)
                    (fn metric
                      (params (line String))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (if
                        (string-empty? (string-trim (local line)))
                        0
                        (string-length
                          (seq-get
                            (string-split-on (string-trim (local line)) \",\")
                            1)))))"
          bundle (sut/compile-source source)
          klass (define-class "example.text-seq" (get bundle "example/text-seq"))
          method (.getMethod klass "metric" (into-array Class [String]))]
      (should= 2 (.invoke method nil (object-array [" a,bb "])))
      (should= 0 (.invoke method nil (object-array ["   "])))))

  (it "compiles AIR-J source with substring char-at and first/empty sequence primitives"
    (let [source "(module example/text-scan
                    (imports)
                    (export token)
                    (fn token
                      (params (line String))
                      (returns String)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (if
                        (seq-empty? (string-split-on (local line) \",\"))
                        \"\"
                        (string-char-at
                          (string-substring
                            (seq-first (string-split-on (local line) \",\"))
                            1
                            3)
                          0))))"
          bundle (sut/compile-source source)
          klass (define-class "example.text-scan" (get bundle "example/text-scan"))
          method (.getMethod klass "token" (into-array Class [String]))]
      (should= "b" (.invoke method nil (object-array ["abz,qq"])))
      (should= "" (.invoke method nil (object-array [","])))))

  (it "writes compiled class files to disk"
    (let [source "(module example/write
                    (imports)
                    (export forty-two)
                    (fn forty-two
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      42))"
          output-dir (.toString (java.nio.file.Files/createTempDirectory "airj-classes"
                                                                         (make-array java.nio.file.attribute.FileAttribute 0)))
          written (sut/build-source! source output-dir)
          class-file (io/file output-dir "example/write.class")]
      (should= (.getPath class-file) (get written "example/write"))
      (should (.exists class-file))
      (should (< 0 (.length class-file)))))

  (it "loads emitted class files from disk and executes them"
    (let [source "(module example/on-disk
                    (imports)
                    (export forty-two)
                    (fn forty-two
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      42))"
          output-dir (.toString (java.nio.file.Files/createTempDirectory "airj-on-disk"
                                                                         (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (sut/build-source! source output-dir)
          klass (load-class-from-dir output-dir "example.on-disk")
          method (.getMethod klass "forty_two" (into-array Class []))]
      (should= 42 (.invoke method nil (object-array [])))))

  (it "extracts exported checked module interfaces from source"
    (let [source "(module example/interface
                    (imports
                      (airj alpha/math add))
                    (export Result main)
                    (union Result
                      (variant Ok
                        (field value Int))
                      (variant Err))
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      0)
                    (fn helper
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      1))"]
      (should= {:name 'example/interface
                :imports [{:op :airj-import
                           :module 'alpha/math
                           :symbols ['add]}]
                :exports ['Result 'main]
                :decls [{:op :union
                         :name 'Result
                         :type-params []
                         :invariants []
                         :variants [{:name 'Ok
                                     :fields [{:name 'value
                                               :type 'Int}]}
                                    {:name 'Err
                                     :fields []}]}
                        {:op :fn
                         :name 'main
                         :params []
                         :return-type 'Int
                         :effects []}]}
               (sut/interface-source source))))

  (it "compiles source using explicit imported AIR-J interfaces"
    (let [source "(module example/import-use
                    (imports
                      (airj alpha/math tick))
                    (export program)
                    (fn program
                      (params)
                      (returns Int)
                      (effects (Clock.Read))
                      (requires true)
                      (ensures true)
                      (call (local tick) 1)))"
          interfaces {'alpha/math {:name 'alpha/math
                                   :imports []
                                   :exports ['tick]
                                   :decls [{:op :fn
                                            :name 'tick
                                            :params [{:name 'x :type 'Int}]
                                            :return-type 'Int
                                            :effects ['Clock.Read]}]}}
          bundle (sut/compile-source source {:interfaces interfaces})]
      (should (contains? bundle "example/import-use"))))

  (it "compiles source using imported AIR-J interface sources"
    (let [source "(module example/import-use
                    (imports
                      (airj alpha/math tick))
                    (export program)
                    (fn program
                      (params)
                      (returns Int)
                      (effects (Clock.Read))
                      (requires true)
                      (ensures true)
                      (call (local tick) 1)))"
          interface-sources {'alpha/math "(module alpha/math
                                           (imports)
                                           (export tick)
                                           (fn tick
                                             (params (x Int))
                                             (returns Int)
                                             (effects (Clock.Read))
                                             (requires true)
                                             (ensures true)
                                             (local x)))"}
          bundle (sut/compile-source source {:interface-sources interface-sources})]
      (should (contains? bundle "example/import-use"))))

  (it "compiles a root module using sibling project sources"
    (let [project-sources {'alpha/math "(module alpha/math
                                          (imports)
                                          (export tick)
                                          (fn tick
                                            (params (x Int))
                                            (returns Int)
                                            (effects (Clock.Read))
                                            (requires true)
                                            (ensures true)
                                            (local x)))"
                           'example/use "(module example/use
                                           (imports
                                             (airj alpha/math tick))
                                           (export program)
                                           (fn program
                                             (params)
                                             (returns Int)
                                             (effects (Clock.Read))
                                             (requires true)
                                             (ensures true)
                                             (call (local tick) 1)))"}
          bundle (sut/compile-project-source project-sources 'example/use)]
      (should (contains? bundle "example/use"))))

  (it "runs a root module using sibling project sources"
    (let [project-sources {'alpha/math "(module alpha/math
                                          (imports)
                                          (export tick)
                                          (fn tick
                                            (params (x Int))
                                            (returns Int)
                                            (effects (Clock.Read))
                                            (requires true)
                                            (ensures true)
                                            (local x)))"
                           'example/use "(module example/use
                                           (imports
                                             (airj alpha/math tick))
                                           (export main)
                                           (fn main
                                             (params)
                                             (returns Int)
                                             (effects (Clock.Read))
                                             (requires true)
                                             (ensures true)
                                             (call (local tick) 1)))"}]
      (should= 1
               (sut/run-project-source! project-sources 'example/use []))))

  (it "runs a root module from project sources without compiling unreachable broken modules"
    (let [project-sources {'alpha/math "(module alpha/math
                                          (imports)
                                          (export tick)
                                          (fn tick
                                            (params (x Int))
                                            (returns Int)
                                            (effects ())
                                            (requires true)
                                            (ensures true)
                                            (local x)))"
                           'example/use "(module example/use
                                           (imports
                                             (airj alpha/math tick))
                                           (export main)
                                           (fn main
                                             (params)
                                             (returns Int)
                                             (effects ())
                                             (requires true)
                                             (ensures true)
                                             (call (local tick) 1)))"
                           'broken/unused "(module broken/unused
                                             (imports)
                                             (export main)
                                             (fn main
                                               (params)
                                               (returns Int)
                                               (effects ())
                                               (requires true)
                                               (ensures true)
                                               (local nope)))"}]
      (should= 1
               (sut/run-project-source! project-sources 'example/use []))))

  (it "runs a root module using AIR-J project files on disk"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-project-dir"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (io/file project-dir "math.airj")
                  "(module alpha/math
                     (imports)
                     (export tick)
                     (fn tick
                       (params (x Int))
                       (returns Int)
                       (effects (Clock.Read))
                       (requires true)
                       (ensures true)
                       (local x)))")
          _ (spit (io/file project-dir "use.airj")
                  "(module example/use
                     (imports
                       (airj alpha/math tick))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects (Clock.Read))
                       (requires true)
                       (ensures true)
                       (call (local tick) 1)))")]
      (should= 1
               (sut/run-project-dir! project-dir 'example/use []))))

  (it "runs a root module from project files without compiling unreachable broken modules"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-project-dir-pruned"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (io/file project-dir "math.airj")
                  "(module alpha/math
                     (imports)
                     (export tick)
                     (fn tick
                       (params (x Int))
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (local x)))")
          _ (spit (io/file project-dir "use.airj")
                  "(module example/use
                     (imports
                       (airj alpha/math tick))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (call (local tick) 1)))")
          _ (spit (io/file project-dir "broken.airj")
                  "(module broken/unused
                     (imports)
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (local nope)))")]
      (should= 1
               (sut/run-project-dir! project-dir 'example/use []))))

  (it "writes a JVM main wrapper for exported AIR-J main"
    (let [source "(module example/entry
                    (imports)
                    (export main)
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      42))"
          output-dir (.toString (java.nio.file.Files/createTempDirectory "airj-main"
                                                                         (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (sut/build-source! source output-dir)
          klass (load-class-from-dir output-dir "example.entry")
          jvm-main (.getMethod klass "main" (into-array Class [(class (into-array String []))]))]
      (should jvm-main)))

  (it "runs exported AIR-J main functions"
    (let [property-name "airj.compiler-spec.main"
          _ (System/clearProperty property-name)
          source "(module example/run
                    (imports
                      (java java.lang.System))
                    (export main)
                    (fn main
                      (params)
                      (returns Int)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (java/static-call
                          java.lang.System
                          setProperty
                          (signature (String String) (Java java.lang.String))
                          \"airj.compiler-spec.main\"
                          \"ran\")
                        0)))"]
      (should= 0 (sut/run-source! source []))
      (should= "ran" (System/getProperty property-name))
      (System/clearProperty property-name)))

  (it "passes run arguments into exported AIR-J main"
    (let [property-name "airj.compiler-spec.args"
          _ (System/clearProperty property-name)
          args ["a" "b"]
          expected (str (java.util.Arrays/hashCode (object-array args)))
          source "(module example/run-args
                    (imports
                      (java java.lang.System)
                      (java java.lang.String)
                      (java java.util.Arrays))
                    (export main)
                    (fn main
                      (params (args (Java \"[Ljava.lang.String;\")))
                      (returns Int)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (java/static-call
                          java.lang.System
                          setProperty
                          (signature (String String) (Java java.lang.String))
                          \"airj.compiler-spec.args\"
                          (java/static-call
                            java.lang.String
                            valueOf
                            (signature (Int) String)
                            (java/static-call
                              java.util.Arrays
                              hashCode
                              (signature ((Java \"[Ljava.lang.Object;\")) Int)
                              (local args))))
                        0)))"]
      (should= 0 (sut/run-source! source args))
      (should= expected (System/getProperty property-name))
      (System/clearProperty property-name)))

  (it "uses AIR-J main Int returns as JVM process exit codes on disk"
    (let [source "(module example/run_args
                    (imports
                      (java java.util.Arrays))
                    (export main)
                    (fn main
                      (params (args (Java \"[Ljava.lang.String;\")))
                      (returns Int)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (java/static-call
                        java.util.Arrays
                        hashCode
                        (signature ((Java \"[Ljava.lang.Object;\")) Int)
                        (local args))))"
          output-dir (.toString (java.nio.file.Files/createTempDirectory "airj-main-from-disk"
                                                                         (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (sut/build-source! source output-dir)
          exit-code (run-main-process output-dir "example.run_args" "a" "b")]
      (should= (bit-and 0xFF (java.util.Arrays/hashCode (object-array ["a" "b"])))
                exit-code)))

  (it "compiles loops and recur into executable bytes"
    (let [source "(module example/looping
                    (imports)
                    (export Step program)
                    (union Step
                      (variant Continue (field value Int))
                      (variant Done (field value Int)))
                    (fn program
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (loop ((state (variant Step Continue 3)))
                        (match (local state)
                          (case (Continue value)
                            (recur (variant Step Done (local value))))
                          (case (Done value)
                            (local value))))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/looping")
          method (.getMethod klass "program" (into-array Class []))]
      (should= 3 (.invoke method nil (object-array [])))))

  (it "compiles parameterized local unions into executable bytes"
    (let [source "(module example/local_result
                    (imports)
                    (export Result main)
                    (union Result
                      (type-params Ok Err)
                      (variant Ok
                        (field value Ok))
                      (variant Err
                        (field error Err)))
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (match
                        (variant (Result Int String) Ok 7)
                        (case (Ok value)
                          (local value))
                        (case (Err error)
                          0))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/local_result")
          method (.getMethod klass "airj_main" (into-array Class []))]
      (should (contains? bundle "example/local_result$Result"))
      (should (contains? bundle "example/local_result$Result$Ok"))
      (should= 7 (.invoke method nil (object-array [])))))

  (it "compiles imported canonical Option Result and Diagnostic types"
    (let [source "(module example/imported_core
                    (imports
                      (airj airj/core Option Result Diagnostic))
                    (export main)
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (seq
                        (variant (Result Int Diagnostic)
                                 Err
                                 (construct Diagnostic
                                            \"type-check\"
                                            \"unused\"
                                            \"detail\"))
                        (match
                          (variant (Option Int) Some 9)
                          (case (Some value)
                            (local value))
                          (case None
                            0)))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/imported_core")
          method (.getMethod klass "airj_main" (into-array Class []))]
      (should (contains? bundle "airj/core"))
      (should (contains? bundle "airj/core$Option"))
      (should (contains? bundle "airj/core$Result"))
      (should (contains? bundle "airj/core$Diagnostic"))
      (should= 9 (.invoke method nil (object-array [])))))

  (it "runs canonical AIR-J file I/O through imported file functions"
    (let [file (.toString (java.nio.file.Files/createTempFile "airj-file-io"
                                                              ".txt"
                                                              (make-array java.nio.file.attribute.FileAttribute 0)))
          source "(module example/file_io
                    (imports
                      (airj airj/file read-string write-string))
                    (export main)
                    (fn main
                      (params (args StringSeq))
                      (returns Int)
                      (effects (File.Read File.Write Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (call (local write-string)
                              (seq-get (local args) 0)
                              \"hello\")
                        (if
                          (string-eq
                            (call (local read-string)
                                  (seq-get (local args) 0))
                            \"hello\")
                          0
                          1))))"
          result (sut/run-source! source [file])]
      (should= 0 result)
      (should= "hello" (slurp file))))

  (it "runs canonical AIR-J parse and file result wrappers"
    (let [file (.toString (java.nio.file.Files/createTempFile "airj-file-result"
                                                              ".txt"
                                                              (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (clojure.java.io/delete-file file)
          source "(module example/file_result
                    (imports
                      (airj airj/core Diagnostic Result parse-int)
                      (airj airj/file exists? read-string-result write-string-result))
                    (export main)
                    (fn main
                      (params (args StringSeq))
                      (returns Int)
                      (effects (File.Read File.Write Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (if
                        (call (local exists?) (seq-get (local args) 0))
                        1
                        (seq
                          (call (local write-string-result)
                                (seq-get (local args) 0)
                                \"41\")
                          (call (local read-string-result)
                                (seq-get (local args) 0))
                          (call (local parse-int) \"41\")
                          42))))"
          result (sut/run-source! source [file])]
      (should= 42 result)
      (should= "41" (slurp file))))

  (it "runs canonical sequence and map primitives through the compiler"
    (let [source "(module example/structured_runtime
                    (imports
                      (airj airj/core Option))
                    (export main)
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (match
                        (map-get
                          (map-set
                            (map-empty Int)
                            \"tail-mark\"
                            (string-length
                              (seq-first
                                (seq-rest
                                  (string-split-on \"alpha\nbeta\ngamma\" \"\n\"))))
                            )
                          \"tail-mark\")
                        (case (Some value)
                          (int-add (local value) 1))
                        (case None
                          0))))"
          result (sut/run-source! source [])]
      (should= 5 result)))

  (it "compiles direct local lambda calls into executable bytes"
    (let [source "(module example/lambdas
                    (imports)
                    (export program)
                    (fn program
                      (params (flag Bool) (x Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (let ((f (lambda
                                  (params (y Int))
                                  (returns Int)
                                  (effects ())
                                  (if
                                    (local flag)
                                    (local y)
                                    0))))
                        (call (local f) (local x)))))"
          bundle (sut/compile-source source)
          klass (define-class "example.lambdas" (get bundle "example/lambdas"))
          method (.getMethod klass "program" (into-array Class [Boolean/TYPE Integer/TYPE]))]
      (should= 9 (.invoke method nil (object-array [true (int 9)])))
      (should= 0 (.invoke method nil (object-array [false (int 9)])))))

  (it "compiles non-capturing lambda values into executable bytes"
    (let [source "(module example/closures
                    (imports)
                    (export program)
                    (fn program
                      (params (flag Bool) (x Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (let ((chooser
                              (if
                                (local flag)
                                (lambda
                                  (params (y Int))
                                  (returns Int)
                                  (effects ())
                                  (local y))
                                (lambda
                                  (params (y Int))
                                  (returns Int)
                                  (effects ())
                                  0))))
                        (call (local chooser) (local x)))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/closures")
          method (.getMethod klass "program" (into-array Class [Boolean/TYPE Integer/TYPE]))]
      (should= 8 (.invoke method nil (object-array [true (int 8)])))
      (should= 0 (.invoke method nil (object-array [false (int 8)])))))

  (it "compiles multi-arg lambda values into executable bytes"
    (let [source "(module example/multi_closures
                    (imports)
                    (export program)
                    (fn program
                      (params (x Int) (y Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (let ((chooser
                              (if
                                true
                                (lambda
                                  (params (a Int) (b Int))
                                  (returns Int)
                                  (effects ())
                                  (local a))
                                (lambda
                                  (params (a Int) (b Int))
                                  (returns Int)
                                  (effects ())
                                  (local b)))))
                        (call (local chooser) (local x) (local y)))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/multi_closures")
          method (.getMethod klass "program" (into-array Class [Integer/TYPE Integer/TYPE]))]
      (should= 7 (.invoke method nil (object-array [(int 7) (int 9)])))))

  (it "compiles same-module function values into executable bytes"
    (let [source "(module example/function_values
                    (imports)
                    (export helper program)
                    (fn helper
                      (params (a Int) (b Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (local a))
                    (fn program
                      (params (x Int) (y Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (let ((chooser (local helper)))
                        (call (local chooser) (local x) (local y)))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/function_values")
          method (.getMethod klass "program" (into-array Class [Integer/TYPE Integer/TYPE]))]
      (should= 7 (.invoke method nil (object-array [(int 7) (int 9)])))))

  (it "compiles capturing lambda values into executable bytes"
    (let [source "(module example/capturing_closures
                    (imports)
                    (export program)
                    (fn program
                      (params (x Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (let ((base 5)
                            (adder
                              (if
                                true
                                (lambda
                                  (params (y Int))
                                  (returns Int)
                                  (effects ())
                                  (if true (local base) (local y)))
                                (lambda
                                  (params (y Int))
                                  (returns Int)
                                  (effects ())
                                  (local y)))))
                        (call (local adder) (local x)))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/capturing_closures")
          method (.getMethod klass "program" (into-array Class [Integer/TYPE]))]
      (should= 5 (.invoke method nil (object-array [(int 8)])))))

  (it "compiles mutable capturing lambda values into executable bytes"
    (let [source "(module example/mutable_capturing_closures
                    (imports)
                    (export program)
                    (fn program
                      (params (x Int))
                      (returns Int)
                      (effects (State.Write))
                      (requires true)
                      (ensures true)
                      (seq
                        (var base Int 5)
                        (let ((adder
                                (if
                                  true
                                  (lambda
                                    (params (y Int))
                                    (returns Int)
                                    (effects (State.Write))
                                    (local base))
                                  (lambda
                                    (params (y Int))
                                    (returns Int)
                                    (effects (State.Write))
                                    (local y)))))
                          (seq
                            (set base 9)
                            (call (local adder) (local x)))))))"
          bundle (sut/compile-source source)
          classes (define-classes bundle)
          klass (get classes "example/mutable_capturing_closures")
          method (.getMethod klass "program" (into-array Class [Integer/TYPE]))]
      (should= 9 (.invoke method nil (object-array [(int 8)])))))

  (it "compiles try catch finally and raise into executable bytes"
    (let [property-name "airj.compiler-spec.finally"
          _ (System/clearProperty property-name)
          source "(module example/raising
                    (imports
                      (java java.lang.RuntimeException)
                      (java java.lang.System))
                    (export program)
                    (fn program
                      (params)
                      (returns Int)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (try
                        (raise
                          (java/new java.lang.RuntimeException \"boom\"))
                        (catch (Java java.lang.RuntimeException) ex 7)
                        (finally
                          (seq
                            (java/static-call
                              java.lang.System
                              setProperty
                              (signature (String String) String)
                              \"airj.compiler-spec.finally\"
                              \"done\")
                            0)))))"
          bundle (sut/compile-source source)
          klass (define-class "example.raising" (get bundle "example/raising"))
          method (.getMethod klass "program" (into-array Class []))]
      (should= 7 (.invoke method nil (object-array [])))
      (should= "done" (System/getProperty property-name))
      (System/clearProperty property-name)))

  (it "enforces failing preconditions at runtime"
    (let [source "(module example/preconditions
                    (imports)
                    (export main)
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires false)
                      (ensures true)
                      42))"]
      (try
        (sut/run-source! source [])
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should (instance? IllegalStateException (.getCause e)))
          (should= "Precondition failed: main"
                   (.getMessage (.getCause e)))))))

  (it "enforces failing postconditions at runtime"
    (let [source "(module example/postconditions
                    (imports)
                    (export main)
                    (fn main
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures false)
                      42))"]
      (try
        (sut/run-source! source [])
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should (instance? IllegalStateException (.getCause e)))
          (should= "Postcondition failed: main"
                   (.getMessage (.getCause e)))))))

  (it "enforces failing data invariants at runtime when constructing values"
    (let [source "(module example/data_invariants
                    (imports)
                    (export main)
                    (data Counter
                      (invariants
                        (local valid))
                      (field valid Bool))
                    (fn main
                      (params)
                      (returns Counter)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (construct Counter false)))"]
      (try
        (sut/run-source! source [])
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should (instance? IllegalStateException (.getCause e)))
          (should= "Invariant failed: Counter"
                   (.getMessage (.getCause e)))))))

  (it "enforces failing union invariants at runtime when constructing variants"
    (let [source "(module example/union_invariants
                    (imports)
                    (export main)
                    (union Response
                      (invariants
                        (match
                          (local self)
                          (case (Ok value)
                            (local value))
                          (case (Error)
                            true)))
                      (variant Ok
                        (field value Bool))
                      (variant Error))
                    (fn main
                      (params)
                      (returns Response)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (variant Response Ok false)))"]
      (try
        (sut/run-source! source [])
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should (instance? IllegalStateException (.getCause e)))
          (should= "Invariant failed: Response"
                   (.getMessage (.getCause e)))))))

  (it "rejects constructing imported AIR-J data types from interface-only imports"
    (let [source "(module example/imported_counter_use
                    (imports
                      (airj alpha/math Counter))
                    (export main)
                    (fn main
                      (params)
                      (returns Counter)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (construct Counter 1)))"
          interfaces {'alpha/math {:name 'alpha/math
                                   :imports []
                                   :exports ['Counter]
                                   :decls [{:op :data
                                            :name 'Counter
                                            :type-params []
                                            :invariants [true]
                                            :fields [{:name 'value
                                                      :type 'Int}]}]}}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unknown constructed type."
                    (sut/compile-source source {:interfaces interfaces}))))

  (it "enforces imported type invariants in the defining project module"
    (let [project-sources {'alpha/math "(module alpha/math
                                          (imports)
                                          (export Counter make-bad)
                                          (data Counter
                                            (invariants
                                              (local valid))
                                            (field valid Bool))
                                          (fn make-bad
                                            (params)
                                            (returns Counter)
                                            (effects ())
                                            (requires true)
                                            (ensures true)
                                            (construct Counter false)))"
                           'example/use "(module example/use
                                           (imports
                                             (airj alpha/math Counter make-bad))
                                           (export main)
                                           (fn main
                                             (params)
                                             (returns Int)
                                             (effects ())
                                             (requires true)
                                             (ensures true)
                                             (seq
                                               (call (local make-bad))
                                               1)))"}]
      (try
        (sut/run-project-source! project-sources 'example/use [])
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should (instance? IllegalStateException (.getCause e)))
          (should= "Invariant failed: Counter"
                   (.getMessage (.getCause e)))))))

  (it "rejects running modules without an exported AIR-J main"
    (let [source "(module example/no-run
                    (imports)
                    (export forty-two)
                    (fn forty-two
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      42))"]
      (should-throw clojure.lang.ExceptionInfo
                    "Missing AIR-J main entrypoint."
                    (sut/run-source! source []))))

  (it "runs AIR-J programs that print explicitly via Java static fields"
    (let [source "(module example/print
                    (imports
                      (java java.lang.System)
                      (java java.io.PrintStream))
                    (export main)
                    (fn main
                      (params)
                      (returns Unit)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (java/call
                        (java/get-static-field
                          java.lang.System
                          out
                          (Java java.io.PrintStream))
                        println
                        (signature (String) Unit)
                        \"hello\")))"
          out-bytes (java.io.ByteArrayOutputStream.)
          out-stream (java.io.PrintStream. out-bytes)
          original-out java.lang.System/out]
      (try
        (java.lang.System/setOut out-stream)
        (sut/run-source! source [])
        (.flush out-stream)
        (should= "hello\n"
                 (.toString out-bytes))
        (finally
          (java.lang.System/setOut original-out)
          (.close out-stream)))))

  (it "runs canonical JSON interchange through file boundaries"
    (let [input (.toString (java.nio.file.Files/createTempFile "airj-json-input"
                                                               ".json"
                                                               (make-array java.nio.file.attribute.FileAttribute 0)))
          output (.toString (java.nio.file.Files/createTempFile "airj-json-output"
                                                                ".json"
                                                                (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit input "{\"planet\":\"earth\"}")
          _ (spit output "")
          source "(module example/json_file
                    (imports
                      (airj airj/core Diagnostic Interchange Result)
                      (airj airj/file read-string-result write-string-result)
                      (airj airj/json parse-result write))
                    (export main)
                    (fn main
                      (params (args StringSeq))
                      (returns Int)
                      (effects (File.Read File.Write Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (match
                        (call (local read-string-result) (seq-get (local args) 0))
                        (case (Ok text)
                          (match
                            (call (local parse-result) (local text))
                            (case (Ok payload)
                              (seq
                                (call (local write-string-result)
                                      (seq-get (local args) 1)
                                      (call (local write) (local payload)))
                                1))
                            (case (Err error)
                              0)))
                        (case (Err error)
                          0))))"
          result (sut/run-source! source [input output])]
      (should= 1 result)
      (should= "{\"planet\":\"earth\"}"
               (slurp output)))))

  (it "runs a canonical tool workflow through env process bytes json and file boundaries"
    (let [input (.toString (java.nio.file.Files/createTempFile "airj-tool-input"
                                                               ".json"
                                                               (make-array java.nio.file.attribute.FileAttribute 0)))
          output (.toString (java.nio.file.Files/createTempFile "airj-tool-output"
                                                                ".json"
                                                                (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit input "{\"tool\":\"ok\"}")
          _ (spit output "")
          source "(module example/tool_workflow
                    (imports
                      (airj airj/core Diagnostic Interchange Option Result)
                      (airj airj/bytes utf8-encode utf8-decode)
                      (airj airj/env get)
                      (airj airj/file read-string-result write-string-result)
                      (airj airj/json parse-result write)
                      (airj airj/process ProcessResult run-result))
                    (export main)
                    (fn main
                      (params (args StringSeq))
                      (returns Int)
                      (effects (Env.Read File.Read File.Write Process.Run Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (match
                        (call (local get) \"PATH\")
                        (case (Some path)
                          (match
                            (call (local read-string-result) (seq-get (local args) 0))
                            (case (Ok text)
                              (match
                                (call (local run-result)
                                      (string-split-on \"/bin/cat\" \"\\u0000\")
                                      (call (local utf8-encode) (local text)))
                                (case (Ok process)
                                  (match
                                    (call (local parse-result)
                                          (call (local utf8-decode)
                                                (record-get (local process) stdout)))
                                    (case (Ok payload)
                                      (seq
                                        (call (local write-string-result)
                                              (seq-get (local args) 1)
                                              (call (local write) (local payload)))
                                        1))
                                    (case (Err error)
                                      0)))
                                (case (Err error)
                                  0)))
                            (case (Err error)
                              0)))
                        (case (None)
                          0))))"
          result (sut/run-source! source [input output])]
      (should= 1 result)
      (should= "{\"tool\":\"ok\"}"
               (slurp output))))
