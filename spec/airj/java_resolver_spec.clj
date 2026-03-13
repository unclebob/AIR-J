(ns airj.java-resolver-spec
  (:require [airj.java-resolver :as sut]
            [speclj.core :refer :all]))

(describe "java interop resolution"
  (it "accepts resolvable java interop members"
    (let [module {:name 'example/interop
                  :imports [{:op :java-import
                             :class-name 'java.lang.Integer}
                            {:op :java-import
                             :class-name 'java.lang.StringBuilder}
                            {:op :java-import
                             :class-name 'java.awt.Point}]
                  :exports ['interop]
                  :decls [{:op :fn
                           :name 'interop
                           :params []
                           :return-type 'Int
                           :effects ['State.Write 'Foreign.Throw]
                           :requires [true]
                           :ensures [true]
                           :body {:op :seq
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
                                           :expr 9}
                                          1]}}]}]
      (should= module
               (sut/check-module module))))

  (it "rejects unknown java classes"
    (let [module {:name 'example/bad-class
                  :imports [{:op :java-import
                             :class-name 'nope.Missing}]
                  :exports []
                  :decls [{:op :fn
                           :name 'bad
                           :params []
                           :return-type '(Java nope.Missing)
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-new
                                  :class-name 'nope.Missing
                                  :type-args []
                                  :args []}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unknown Java class."
                    (sut/check-module module))))

  (it "rejects missing java imports for interop classes"
    (let [module {:name 'example/missing-import
                  :imports []
                  :exports []
                  :decls [{:op :fn
                           :name 'bad
                           :params []
                           :return-type '(Java java.lang.StringBuilder)
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-new
                                  :class-name 'java.lang.StringBuilder
                                  :type-args []
                                  :args []}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Missing Java import."
                    (sut/check-module module))))

  (it "rejects unresolved java methods"
    (let [module {:name 'example/bad-method
                  :imports [{:op :java-import
                             :class-name 'java.lang.StringBuilder}]
                  :exports []
                  :decls [{:op :fn
                           :name 'bad
                           :params []
                           :return-type '(Java java.lang.StringBuilder)
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-call
                                  :target {:op :java-new
                                           :class-name 'java.lang.StringBuilder
                                           :type-args []
                                           :args []}
                                  :member-id 'missing
                                  :signature {:params []
                                              :return-type '(Java java.lang.StringBuilder)}
                                  :args []}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unknown Java method."
                    (sut/check-module module))))

  (it "rejects unresolved java fields"
    (let [module {:name 'example/bad-field
                  :imports [{:op :java-import
                             :class-name 'java.awt.Point}]
                  :exports []
                  :decls [{:op :fn
                           :name 'bad
                           :params []
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :java-get-field
                                  :target {:op :java-new
                                           :class-name 'java.awt.Point
                                           :type-args []
                                           :args [1 2]}
                                  :field-name 'missing
                                  :field-type 'Int}}]}]
      (should-throw clojure.lang.ExceptionInfo
                    "Unknown Java field."
                    (sut/check-module module)))))

  (it "accepts java interop over loop-bound locals"
    (let [module {:name 'example/loop-builder
                  :imports [{:op :java-import
                             :class-name 'java.lang.StringBuilder}]
                  :exports ['main]
                  :decls [{:op :fn
                           :name 'main
                           :params []
                           :return-type 'String
                           :effects ['Foreign.Throw 'State.Write]
                           :requires [true]
                           :ensures [true]
                           :body {:op :loop
                                  :bindings [{:name 'builder
                                              :expr {:op :java-new
                                                     :class-name 'java.lang.StringBuilder
                                                     :type-args []
                                                     :args []}}]
                                  :body {:op :java-call
                                         :target {:op :local
                                                  :name 'builder}
                                         :member-id 'toString
                                         :signature {:params []
                                                     :return-type 'String}
                                         :args []}}}]}]
      (should= module
               (sut/check-module module))))
