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
