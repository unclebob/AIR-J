(ns airj.cli-spec
  (:require [airj.cli :as sut]
            [airj.effect-checker :as effect-checker]
            [airj.java-resolver :as java-resolver]
            [airj.type-checker :as type-checker]
            [airj.parser :as parser]
            [airj.normalizer :as normalizer]
            [airj.resolver :as resolver]
            [speclj.core :refer :all]))

(describe "run"
  (it "renders parsed modules as edn for the parse command"
    (let [source "(module example/minimal
                    (imports)
                    (export)
                    (fn identity
                      (params (x Int))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (local x)))"
          result (sut/run ["parse" "--stdin"] source)]
      (should-contain ":name example/minimal" result)
      (should-contain ":op :fn" result)
      (should-contain ":name identity" result)))

  (it "parses modules from a file path"
    (let [file (java.io.File/createTempFile "airj-cli" ".airj")
          _ (spit file "(module example/from-file
                          (imports)
                          (export))")
          result (sut/run ["parse" (.getPath file)] "ignored")]
      (should-contain ":name example/from-file" result)
      (.delete file)))

  (it "rejects unsupported commands"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported command."
                  (sut/run ["compile"] "")))

  (it "renders normalized modules as edn for the normalize command"
    (let [source "(module example/normalize
                    (imports
                      (java java.time.Instant)
                      (airj zeta/mod z)
                      (airj alpha/mod a))
                    (export zeta alpha)
                    (fn f
                      (params (x Int))
                      (returns Int)
                      (effects (State.Write Clock.Read))
                      (requires true)
                      (ensures true)
                      (local x)))"
          expected (-> source
                       parser/parse-module
                       normalizer/normalize-module
                       pr-str)
          result (sut/run ["normalize" "--stdin"] source)]
      (should= expected result)))

  (it "renders checked modules as edn for the check command"
    (let [source "(module example/check
                    (imports)
                    (export status)
                    (data Response
                      (field status Int)
                      (field body String))
                    (fn status
                      (params (response Response))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (record-get (local response) status)))"
          expected (-> source
                       parser/parse-module
                       normalizer/normalize-module
                       resolver/resolve-module
                       effect-checker/check-module
                       type-checker/check-module
                       java-resolver/check-module
                       pr-str)
          result (sut/run ["check" "--stdin"] source)]
      (should= expected result)))

  (it "renders checked exported interfaces as edn for the interface command"
    (let [source "(module example/interface
                    (imports
                      (airj alpha/math add))
                    (export main)
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
                      1))"
          result (sut/run ["interface" "--stdin"] source)]
      (should-contain ":name example/interface" result)
      (should-contain ":op :airj-import" result)
      (should-contain ":name main" result)
      (should-not-contain ":name helper" result)))

  (it "checks java interop modules through the CLI"
    (let [source "(module example/interop
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
          result (sut/run ["check" "--stdin"] source)]
      (should-contain ":name example/interop" result)
      (should-contain ":op :java-call" result)
      (should-contain ":op :java-get-field" result)))

  (it "renders lowered JVM plans for the lower command"
    (let [source "(module example/interop
                    (imports
                      (java java.lang.Math))
                    (export interop)
                    (fn interop
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
          result (sut/run ["lower" "--stdin"] source)]
      (should-contain ":op :jvm-module" result)
      (should-contain ":op :jvm-java-static-call" result)
      (should-contain ":parameter-types [:int]" result)))

  (it "accepts imported AIR-J interfaces through an explicit CLI option"
    (let [source "(module example/imported-lower
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
          interfaces-file (java.io.File/createTempFile "airj-interfaces" ".edn")
          _ (spit interfaces-file
                  (pr-str
                   {'alpha/math {:name 'alpha/math
                                 :imports []
                                 :exports ['tick]
                                 :decls [{:op :fn
                                          :name 'tick
                                          :params [{:name 'x :type 'Int}]
                                          :return-type 'Int
                                          :effects ['Clock.Read]}]}}))
          result (sut/run ["lower" "--interfaces-edn" (.getPath interfaces-file) "--stdin"] source)]
      (should-contain ":owner \"alpha/math\"" result)
      (.delete interfaces-file)))

  (it "accepts imported AIR-J interface sources through an explicit CLI option"
    (let [source "(module example/imported-lower
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
          sources-file (java.io.File/createTempFile "airj-interface-sources" ".edn")
          _ (spit sources-file
                  (pr-str
                   {'alpha/math "(module alpha/math
                                   (imports)
                                   (export tick)
                                   (fn tick
                                     (params (x Int))
                                     (returns Int)
                                     (effects (Clock.Read))
                                     (requires true)
                                     (ensures true)
                                     (local x)))"}))
          result (sut/run ["lower" "--interface-sources-edn" (.getPath sources-file) "--stdin"] source)]
      (should-contain ":owner \"alpha/math\"" result)
      (.delete sources-file)))

  (it "lowers a named root module from project sources"
    (let [sources-file (java.io.File/createTempFile "airj-project-sources" ".edn")
          _ (spit sources-file
                  (pr-str
                   {'alpha/math "(module alpha/math
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
                                      (call (local tick) 1)))"}))
          result (sut/run ["lower" "--project-sources-edn" (.getPath sources-file) "example/use"] "ignored")]
      (should-contain ":owner \"alpha/math\"" result)
      (.delete sources-file)))

  (it "runs a named root module from project sources"
    (let [sources-file (java.io.File/createTempFile "airj-project-sources-run" ".edn")
          _ (spit sources-file
                  (pr-str
                   {'alpha/math "(module alpha/math
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
                                      (call (local tick) 1)))"}))]
      (should= "1"
               (sut/run ["run" "--project-sources-edn" (.getPath sources-file) "example/use"] "ignored"))
      (.delete sources-file)))

  (it "runs a named root module from project sources without compiling unreachable broken modules"
    (let [sources-file (java.io.File/createTempFile "airj-project-sources-pruned" ".edn")
          _ (spit sources-file
                  (pr-str
                   {'alpha/math "(module alpha/math
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
                                        (local nope)))"}))]
      (should= "1"
               (sut/run ["run" "--project-sources-edn" (.getPath sources-file) "example/use"] "ignored"))
      (.delete sources-file)))

  (it "surfaces imported type invariant failures when running from project sources"
    (let [sources-file (java.io.File/createTempFile "airj-project-sources-invariant" ".edn")
          _ (spit sources-file
                  (pr-str
                    {'alpha/math "(module alpha/math
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
                                        0)))"}))]
      (try
        (sut/run ["run" "--project-sources-edn" (.getPath sources-file) "example/use"] "ignored")
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should= "Invariant failed: Counter"
                   (.getMessage (.getTargetException e)))))
      (.delete sources-file)))

  (it "adds project command context to missing-module failures from project sources"
    (let [sources-file (java.io.File/createTempFile "airj-project-sources-missing" ".edn")
          _ (spit sources-file
                  (pr-str
                   {'example/use "(module example/use
                                    (imports
                                      (airj alpha/math tick))
                                    (export main)
                                    (fn main
                                      (params)
                                      (returns Int)
                                      (effects ())
                                      (requires true)
                                      (ensures true)
                                      (call (local tick) 1)))"}))]
      (try
        (sut/run ["run" "--project-sources-edn" (.getPath sources-file) "example/use"] "ignored")
        (should false)
        (catch clojure.lang.ExceptionInfo e
          (should= "Project command failed." (.getMessage e))
          (should= {:command "run"
                    :root-module 'example/use
                    :module 'alpha/math}
                   (select-keys (ex-data e) [:command :root-module :module]))))
      (.delete sources-file)))

  (it "adds cycle details to project-source failures"
    (let [sources-file (java.io.File/createTempFile "airj-project-sources-cycle" ".edn")
          _ (spit sources-file
                  (pr-str
                   {'alpha/math "(module alpha/math
                                   (imports
                                     (airj beta/math ping))
                                   (export tick)
                                   (fn tick
                                     (params (x Int))
                                     (returns Int)
                                     (effects ())
                                     (requires true)
                                     (ensures true)
                                     (call (local ping) (local x))))"
                    'beta/math "(module beta/math
                                  (imports
                                    (airj alpha/math tick))
                                  (export ping)
                                  (fn ping
                                    (params (x Int))
                                    (returns Int)
                                    (effects ())
                                    (requires true)
                                    (ensures true)
                                    (call (local tick) (local x))))"}))]
      (try
        (sut/run ["check" "--project-sources-edn" (.getPath sources-file) "alpha/math"] "ignored")
        (should false)
        (catch clojure.lang.ExceptionInfo e
          (should= "Project command failed." (.getMessage e))
          (should= ['alpha/math 'beta/math 'alpha/math]
                   (:cycle (ex-data e)))))
      (.delete sources-file)))

  (it "runs a named root module from a project directory"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-cli-project-dir"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (java.io.File. project-dir "math.airj")
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
          _ (spit (java.io.File. project-dir "use.airj")
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
      (should= "1"
               (sut/run ["run" "--project-dir" project-dir "example/use"] "ignored"))))

  (it "surfaces imported type invariant failures when running from a project directory"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-cli-project-invariant"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (java.io.File. project-dir "math.airj")
                  "(module alpha/math
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
                       (construct Counter false)))")
          _ (spit (java.io.File. project-dir "use.airj")
                  "(module example/use
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
                         0)))")]
      (try
        (sut/run ["run" "--project-dir" project-dir "example/use"] "ignored")
        (should false)
        (catch java.lang.reflect.InvocationTargetException e
          (should= "Invariant failed: Counter"
                   (.getMessage (.getTargetException e)))))))

  (it "adds project command context to missing-module failures from a project directory"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-cli-project-missing"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (java.io.File. project-dir "use.airj")
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
                       (call (local tick) 1)))")]
      (try
        (sut/run ["run" "--project-dir" project-dir "example/use"] "ignored")
        (should false)
        (catch clojure.lang.ExceptionInfo e
          (should= "Project command failed." (.getMessage e))
          (should= {:command "run"
                    :root-module 'example/use
                    :module 'alpha/math}
                   (select-keys (ex-data e) [:command :root-module :module]))))))

  (it "adds cycle details to project-directory failures"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-cli-project-cycle"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (java.io.File. project-dir "alpha.airj")
                  "(module alpha/math
                     (imports
                       (airj beta/math ping))
                     (export tick)
                     (fn tick
                       (params (x Int))
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (call (local ping) (local x))))")
          _ (spit (java.io.File. project-dir "beta.airj")
                  "(module beta/math
                     (imports
                       (airj alpha/math tick))
                     (export ping)
                     (fn ping
                       (params (x Int))
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (call (local tick) (local x))))")]
      (try
        (sut/run ["check" "--project-dir" project-dir "alpha/math"] "ignored")
        (should false)
        (catch clojure.lang.ExceptionInfo e
          (should= "Project command failed." (.getMessage e))
          (should= ['alpha/math 'beta/math 'alpha/math]
                   (:cycle (ex-data e)))))))

  (it "formats project missing-module failures for main"
    (let [error (ex-info "Project command failed."
                         {:command "run"
                          :root-module 'example/use
                          :module 'alpha/math})]
      (should= "Project command failed: run example/use\nMissing module: alpha/math"
               (#'sut/format-cli-error error))))

  (it "formats project cycle failures for main"
    (let [error (ex-info "Project command failed."
                         {:command "check"
                          :root-module 'alpha/math
                          :cycle ['alpha/math 'beta/math 'alpha/math]})]
      (should= "Project command failed: check alpha/math\nImport cycle: [alpha/math beta/math alpha/math]"
               (#'sut/format-cli-error error))))

  (it "prints formatted project errors to stderr in main"
    (let [project-dir (.toString (java.nio.file.Files/createTempDirectory "airj-cli-main-project-missing"
                                                                          (make-array java.nio.file.attribute.FileAttribute 0)))
          _ (spit (java.io.File. project-dir "use.airj")
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
          err (java.io.StringWriter.)
          out (java.io.StringWriter.)]
      (binding [*err* err
                *out* out
                *in* (java.io.StringReader. "")]
        (should= 1 (#'sut/-main "run" "--project-dir" project-dir "example/use")))
      (should= "" (str out))
      (should-contain "Project command failed: run example/use" (str err))
      (should-contain "Missing module: alpha/math" (str err))))

  (it "renders built module byte sizes for the build command"
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
          output-dir (.toString (java.nio.file.Files/createTempDirectory "airj-cli-build"
                                                                         (make-array java.nio.file.attribute.FileAttribute 0)))
          result (sut/run ["build" "--stdin" output-dir] source)]
      (should-contain "\"example/build\"" result)
      (should-contain ".class" result)
      (should-contain output-dir result)))

  (it "runs exported AIR-J main through the CLI"
    (let [property-name "airj.cli-spec.main"
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
                          \"airj.cli-spec.main\"
                          \"ran\")
                        0)))"]
      (should= "0" (sut/run ["run" "--stdin"] source))
      (should= "ran" (System/getProperty property-name))
      (System/clearProperty property-name)))

  (it "passes CLI run arguments into exported AIR-J main"
    (let [property-name "airj.cli-spec.args"
          _ (System/clearProperty property-name)
          expected (str (java.util.Arrays/hashCode (object-array ["a" "b"])))
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
                          \"airj.cli-spec.args\"
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
      (should= "0" (sut/run ["run" "--stdin" "a" "b"] source))
      (should= expected (System/getProperty property-name))
      (System/clearProperty property-name)))

  (it "renders empty output for Unit-returning AIR-J main"
    (let [source "(module example/run-unit
                    (imports
                      (java java.lang.System))
                    (export main)
                    (fn main
                      (params)
                      (returns Unit)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        true
                        (if true 0 0)
                        (java/static-call
                          java.lang.System
                          gc
                          (signature () Unit)))))"]
      (should= "" (sut/run ["run" "--stdin"] source)))))
