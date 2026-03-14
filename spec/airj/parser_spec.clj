(ns airj.parser-spec
  (:require [airj.parser :as sut]
            [speclj.core :refer :all]))

(describe "parse-module"
  (it "parses a minimal AIR-J module"
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
          ast (sut/parse-module source)]
      (should= {:name 'example/minimal
                :imports []
                :exports []
                :decls [{:op :fn
                         :name 'identity
                         :params [{:name 'x :type 'Int}]
                         :return-type 'Int
                         :effects []
                         :requires [true]
                         :ensures [true]
                         :body {:op :local :name 'x}}]}
               ast)))

  (it "parses a host-backed AIR-J module"
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
          ast (sut/parse-module source)]
      (should= {:name 'example/hosted
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
                                :args []}}]}
               ast)))

  (it "rejects unsupported declarations"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported declaration."
                  (sut/parse-module "(module example/minimal
                                      (imports)
                                      (export)
                                      (bogus Thing))")))

  (it "rejects malformed module headers"
    (should-throw clojure.lang.ExceptionInfo
                  "Unexpected form tag."
                  (sut/parse-module "(not-a-module example/minimal
                                      (imports)
                                      (export))")))

  (it "parses data, enum, and union declarations"
    (let [source "(module example/types
                    (imports)
                    (export Stack Method PopResult)
                    (data Stack
                      (invariants true)
                      (field items (List Int)))
                    (enum Method Get Post)
                    (union PopResult
                      (invariants true)
                      (variant Empty)
                      (variant Popped
                        (field value Int)
                        (field rest Stack))))"
          ast (sut/parse-module source)]
      (should= {:name 'example/types
                :imports []
                :exports ['Stack 'Method 'PopResult]
                :decls [{:op :data
                         :name 'Stack
                         :type-params []
                         :invariants [true]
                         :fields [{:name 'items
                                   :type '(List Int)}]}
                        {:op :enum
                         :name 'Method
                         :variants ['Get 'Post]}
                        {:op :union
                         :name 'PopResult
                         :type-params []
                         :invariants [true]
                         :variants [{:name 'Empty
                                     :fields []}
                                    {:name 'Popped
                                     :fields [{:name 'value :type 'Int}
                                              {:name 'rest :type 'Stack}]}]}]}
               ast)))

  (it "parses core expression forms"
    (let [source "(module example/expr
                    (imports)
                    (export route)
                    (fn route
                      (params (req Request))
                      (returns Response)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (if
                        (call (local is-empty) (record-get (local req) path))
                        (construct Response 404 \"not-found\")
                        (match (record-get (local req) method)
                          (case Get
                            (variant RouteResult Matched
                              (construct Response 200 \"ok\")
                              (record-get (local req) path)))
                          (case _
                            (variant RouteResult Missing
                              (construct Response 405 \"bad-method\")))))))"
          ast (sut/parse-module source)]
      (should= {:op :if
                :test {:op :call
                       :callee {:op :local :name 'is-empty}
                       :args [{:op :record-get
                               :target {:op :local :name 'req}
                               :field 'path}]}
                :then {:op :construct
                       :type 'Response
                       :args [404 "not-found"]}
                :else {:op :match
                       :target {:op :record-get
                                :target {:op :local :name 'req}
                                :field 'method}
                       :cases [{:pattern {:op :binder-pattern
                                          :name 'Get}
                                :body {:op :variant
                                       :type 'RouteResult
                                       :name 'Matched
                                       :args [{:op :construct
                                               :type 'Response
                                               :args [200 "ok"]}
                                              {:op :record-get
                                               :target {:op :local :name 'req}
                                               :field 'path}]}}
                               {:pattern {:op :wildcard-pattern}
                                :body {:op :variant
                                       :type 'RouteResult
                                       :name 'Missing
                                       :args [{:op :construct
                                               :type 'Response
                                               :args [405 "bad-method"]}]}}]}}
               (-> ast :decls first :body))))

  (it "parses let seq lambda try var and set expressions"
    (let [source "(module example/effects
                    (imports)
                    (export program)
                    (fn program
                      (params (x Int))
                      (returns Int)
                      (effects (State.Write))
                      (requires true)
                      (ensures true)
                      (let ((f (lambda
                                  (params (y Int))
                                  (returns Int)
                                  (effects ())
                                  (local y))))
                        (seq
                          (var counter Int 0)
                          (set counter (call (local f) (local x)))
                          (try
                            (local counter)
                            (catch Error ex 1)
                            (finally 2))))))"
          ast (sut/parse-module source)]
      (should= {:op :let
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
                                       :args [{:op :local :name 'x}]}}
                               {:op :try
                                :body {:op :local :name 'counter}
                                :catches [{:type 'Error
                                           :name 'ex
                                           :body 1}]
                                :finally 2}]}}
               (-> ast :decls first :body))))

  (it "parses loop recur raise and richer match patterns"
    (let [source "(module example/looping
                    (imports)
                    (export step)
                    (fn step
                      (params (state State))
                      (returns Result)
                      (effects (State.Write))
                      (requires true)
                      (ensures true)
                      (loop ((current (local state)))
                        (match (local current)
                          (case (Running next)
                            (recur (local next)))
                          (case (record Done (value final))
                            (local final))
                          (case _
                            (raise \"bad-state\"))))))"
          ast (sut/parse-module source)]
      (should= {:op :loop
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
                               {:pattern {:op :record-pattern
                                          :type 'Done
                                          :fields [{:name 'value
                                                    :pattern {:op :binder-pattern
                                                              :name 'final}}]}
                                :body {:op :local :name 'final}}
                               {:pattern {:op :wildcard-pattern}
                               :body {:op :raise
                                       :expr "bad-state"}}]}}
               (-> ast :decls first :body))))

  (it "parses literal patterns in match expressions"
    (let [source "(module example/literals
                    (imports)
                    (export classify)
                    (fn classify
                      (params (flag Bool))
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (match (local flag)
                        (case true 1)
                        (case false 0))))"
          ast (sut/parse-module source)]
      (should= {:op :match
                :target {:op :local :name 'flag}
                :cases [{:pattern {:op :literal-pattern
                                   :literal true}
                         :body 1}
                        {:pattern {:op :literal-pattern
                                   :literal false}
                         :body 0}]}
               (-> ast :decls first :body))))

  (it "parses java interop expressions"
    (let [source "(module example/interop
                    (imports
                      (java java.lang.StringBuilder)
                      (java java.awt.Point))
                    (export use-interop)
                    (fn use-interop
                      (params)
                      (returns Int)
                      (effects (State.Write Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (java/static-call java.lang.Integer valueOf (signature (String) (Java java.lang.Integer)) \"42\")
                        (java/call
                          (java/new java.lang.StringBuilder \"a\")
                          append
                          (signature (String) (Java java.lang.StringBuilder))
                          \"b\")
                        (java/get-field
                          (java/new java.awt.Point 1 2)
                          x
                          Int)
                        (java/set-field
                          (java/new java.awt.Point 1 2)
                          x
                          Int
                          9))))"
          ast (sut/parse-module source)]
      (should= [{:op :java-import
                 :class-name 'java.lang.StringBuilder}
                {:op :java-import
                 :class-name 'java.awt.Point}]
               (:imports ast))
      (should= {:op :seq
                :exprs [{:op :java-static-call
                         :class-name 'java.lang.Integer
                         :member-id 'valueOf
                         :signature {:params ['String]
                                     :return-type '(Java java.lang.Integer)}
                         :args ["42"]}
                        {:op :java-call
                         :target {:op :java-new
                                  :class-name 'java.lang.StringBuilder
                                  :type-args []
                                  :args ["a"]}
                         :member-id 'append
                         :signature {:params ['String]
                                     :return-type '(Java java.lang.StringBuilder)}
                         :args ["b"]}
                        {:op :java-get-field
                         :target {:op :java-new
                                  :class-name 'java.awt.Point
                                  :type-args []
                                  :args [1 2]}
                         :field-name 'x
                         :field-type 'Int}
                        {:op :java-set-field
                         :target {:op :java-new
                                  :class-name 'java.awt.Point
                                  :type-args []
                                  :args [1 2]}
                         :field-name 'x
                         :field-type 'Int
                         :expr 9}]}
               (-> ast :decls first :body))))

  (it "parses java constructors with explicit type arguments"
    (let [source "(module example/java-type-args
                    (imports
                      (java java.util.ArrayList))
                    (export make-list)
                    (fn make-list
                      (params)
                      (returns (Java java.util.ArrayList))
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (java/new java.util.ArrayList (type-args String))))"
          ast (sut/parse-module source)]
      (should= {:op :java-new
                :class-name 'java.util.ArrayList
                :type-args ['String]
                :args []}
               (-> ast :decls first :body)))))

  (it "parses static Java field access"
    (let [source "(module example/java-static-field
                    (imports
                      (java java.lang.System))
                    (export stream)
                    (fn stream
                      (params)
                      (returns (Java java.io.PrintStream))
                      (effects ())
                      (requires true)
                      (ensures true)
                      (java/get-static-field
                        java.lang.System
                        out
                        (Java java.io.PrintStream))))"
          ast (sut/parse-module source)]
      (should= {:op :java-static-get-field
                :class-name 'java.lang.System
                :field-name 'out
                :field-type '(Java java.io.PrintStream)}
               (-> ast :decls first :body))))

  (it "parses primitive operator expressions"
    (let [source "(module example/ops
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
          ast (sut/parse-module source)]
      (should= {:op :bool-or
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
               (-> ast :decls first :body))))

  (it "parses additional comparison operators"
    (let [source "(module example/more-ops
                    (imports)
                    (export compare)
                    (fn compare
                      (params (x Int) (y Int) (flag Bool))
                      (returns Bool)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (bool-eq
                        (int-ge (local x) (local y))
                        (bool-or
                          (int-gt (local x) 0)
                          (int-le (local y) 10)))))"
          ast (sut/parse-module source)]
      (should= {:op :bool-eq
                :args [{:op :int-ge
                        :args [{:op :local :name 'x}
                               {:op :local :name 'y}]}
                       {:op :bool-or
                        :args [{:op :int-gt
                                :args [{:op :local :name 'x}
                                       0]}
                               {:op :int-le
                                :args [{:op :local :name 'y}
                                       10]}]}]}
               (-> ast :decls first :body))))

  (it "parses decimal literals floating operators and numeric conversions"
    (let [source "(module example/floats
                    (imports)
                    (export orbit)
                    (fn orbit
                      (params (radius Double) (phase Float))
                      (returns Float)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (double->float
                        (double-add
                          1.5
                          (float->double
                            (float-div
                              (local phase)
                              (int->float 2)))))))"
          ast (sut/parse-module source)]
      (should= {:op :double->float
                :arg {:op :double-add
                      :args [1.5
                             {:op :float->double
                              :arg {:op :float-div
                                    :args [{:op :local :name 'phase}
                                           {:op :int->float
                                            :arg 2}]}}]}}
               (-> ast :decls first :body))))

  (it "parses string equality int inequality conversion and stdout output"
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
          ast (sut/parse-module source)]
      (should= {:op :seq
                :exprs [{:op :io-println
                         :arg {:op :int->string
                               :arg {:op :local :name 'x}}}
                        {:op :string-eq
                         :args [{:op :local :name 'label}
                                "ok"]}
                        {:op :int-ne
                         :args [{:op :local :name 'x}
                                0]}]}
               (-> ast :decls first :body))))

  (it "parses stdin stdout string and conversion primitives"
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
          ast (sut/parse-module source)]
      (should= {:op :seq
                :exprs [{:op :io-print
                         :arg {:op :string-concat
                               :args [">"
                                      {:op :io-read-line}]}}
                        {:op :int-add
                         :args [{:op :string-length
                                 :arg "ab"}
                                {:op :string->int
                                 :arg "7"}]}]}
               (-> ast :decls first :body))))

  (it "parses string splitting trimming emptiness and sequence access primitives"
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
          ast (sut/parse-module source)]
      (should= {:op :if
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
                                    1]}}}
               (-> ast :decls first :body))))

  (it "parses substring char-at and first/empty sequence primitives"
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
          ast (sut/parse-module source)]
      (should= {:op :if
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
                             0]}}
               (-> ast :decls first :body))))

  (it "parses sequence rest concat and canonical map primitives"
    (let [source "(module example/data-core
                    (imports)
                    (export build)
                    (fn build
                      (params (items (Seq String)))
                      (returns (Map String Int))
                      (effects ())
                      (requires true)
                      (ensures true)
                      (map-set
                        (map-set
                          (map-empty Int)
                          \"head\"
                          (string-length (seq-first (local items))))
                        \"tail-size\"
                        (seq-length
                          (seq-concat
                            (seq-rest (local items))
                            (string-split-on \"z\" \",\"))))))"
          ast (sut/parse-module source)]
      (should= {:op :map-set
                :args [{:op :map-set
                        :args [{:op :map-empty
                                :value-type 'Int}
                               "head"
                               {:op :string-length
                                :arg {:op :seq-first
                                      :arg {:op :local :name 'items}}}]}
                       "tail-size"
                       {:op :seq-length
                        :arg {:op :seq-concat
                              :args [{:op :seq-rest
                                      :arg {:op :local :name 'items}}
                                     {:op :string-split-on
                                      :args ["z" ","]}]}}]}
               (-> ast :decls first :body))))

  (it "rejects when as a non-canonical persisted form"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported expression."
                  (sut/parse-module "(module example/noncanonical
                                      (imports)
                                      (export f)
                                      (fn f
                                        (params)
                                        (returns Int)
                                        (effects ())
                                        (requires true)
                                        (ensures true)
                                        (when true 1))")))

  (it "rejects unless as a non-canonical persisted form"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported expression."
                  (sut/parse-module "(module example/noncanonical
                                      (imports)
                                      (export f)
                                      (fn f
                                        (params)
                                        (returns Int)
                                        (effects ())
                                        (requires true)
                                        (ensures true)
                                        (unless true 1))")))

  (it "parses structured AIR-J imports"
    (let [source "(module example/imports
                    (imports
                      (airj alpha/math add sub)
                      (java java.time.Instant))
                    (export)
                    (fn noop
                      (params)
                      (returns Int)
                      (effects ())
                      (requires true)
                      (ensures true)
                      0))"
          ast (sut/parse-module source)]
      (should= [{:op :airj-import
                 :module 'alpha/math
                 :symbols ['add 'sub]}
                {:op :java-import
                 :class-name 'java.time.Instant}]
               (:imports ast))))

  (it "parses canonical JSON interchange primitives"
    (let [source "(module example/json_forms
                    (imports
                      (airj airj/core Interchange))
                    (export normalize)
                    (fn normalize
                      (params (text String))
                      (returns String)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (json-write
                        (json-parse (local text)))))"
          ast (sut/parse-module source)]
      (should= {:op :json-write
                :arg {:op :json-parse
                      :arg {:op :local
                            :name 'text}}}
               (-> ast :decls first :body))))

  (it "parses host environment and process primitives"
    (let [source "(module example/host_forms
                    (imports)
                    (export run)
                    (fn run
                      (params (name String) (command (Seq String)) (stdin Bytes))
                      (returns ProcessResult)
                      (effects (Env.Read Process.Run Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (env-get (local name))
                        (process-run (local command) (local stdin)))))"
          ast (sut/parse-module source)]
      (should= {:op :seq
                :exprs [{:op :env-get
                         :arg {:op :local
                               :name 'name}}
                        {:op :process-run
                         :args [{:op :local
                                 :name 'command}
                                {:op :local
                                 :name 'stdin}]}]}
               (-> ast :decls first :body))))
