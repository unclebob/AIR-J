(ns airj.jvm-closures-spec
  (:require [airj.jvm-closures :as sut]
            [speclj.core :refer :all]))

(describe "type-token"
  (it "normalizes JVM reference and array type names for closure interfaces"
    (let [fail! (fn [message data]
                  (throw (ex-info message data)))]
      (should= "java_lang_String"
               (sut/type-token "java/lang/String;" fail!))
      (should= "Arr_I"
               (sut/type-token "[I" fail!))))

  (it "rejects unsupported JVM types"
    (let [fail! (fn [message data]
                  (throw (ex-info message data)))]
      (should-throw clojure.lang.ExceptionInfo
                    "Unsupported JVM type token."
                    (sut/type-token nil fail!)))))

(describe "closure interface registration"
  (it "builds canonical interface names from lowered parameter and return types"
    (let [fail! (fn [message data]
                  (throw (ex-info message data)))]
      (should= "example.math$Fn2$Int$java_lang_String$Bool"
               (sut/closure-interface-name "example.math"
                                           [:int "java/lang/String;"]
                                           :boolean
                                           fail!))))

  (it "registers closure interfaces idempotently"
    (let [ctx {:module-name "example.math"
               :closure-interfaces (atom {})}
          fail! (fn [message data]
                  (throw (ex-info message data)))]
      (should= "example.math$Fn1$Int$Bool"
               (sut/register-closure-interface! ctx [:int] :boolean fail!))
      (should= "example.math$Fn1$Int$Bool"
               (sut/register-closure-interface! ctx [:int] :boolean fail!))
      (should= {"example.math$Fn1$Int$Bool"
                {:class-name "example.math$Fn1$Int$Bool"
                 :method-name "apply"
                 :params [:int]
                 :return-type :boolean}}
               @(:closure-interfaces ctx))))

  (it "lowers closure function types through the provided type lowerer"
    (let [ctx {:module-name "example.math"
               :closure-interfaces (atom {})}
          lowered (sut/lower-closure-type {:params ['Int 'String]
                                           :return-type 'Bool}
                                          ctx
                                          (fn [type-expr _]
                                            (condp = type-expr
                                              'Int :int
                                              'String "java/lang/String;"
                                              'Bool :boolean))
                                          (fn [message data]
                                            (throw (ex-info message data))))]
      (should= "example.math$Fn2$Int$java_lang_String$Bool"
               lowered)
      (should= {"example.math$Fn2$Int$java_lang_String$Bool"
                {:class-name "example.math$Fn2$Int$java_lang_String$Bool"
                 :method-name "apply"
                 :params [:int "java/lang/String;"]
                 :return-type :boolean}}
               @(:closure-interfaces ctx)))))

(describe "free-locals"
  (it "collects free locals in first-use order"
    (should= ['base 'y]
             (sut/free-locals
              {:op :if
               :test {:op :local :name 'base}
               :then {:op :call
                      :callee {:op :local :name 'f}
                      :args [{:op :local :name 'y}
                             {:op :local :name 'base}]}
               :else 0}
              #{'f})))

  (it "excludes names bound by let"
    (should= ['outside]
             (sut/free-locals
              {:op :let
               :bindings [{:name 'inside
                           :expr {:op :local :name 'outside}}]
               :body {:op :if
                      :test true
                      :then {:op :local :name 'inside}
                      :else 0}}
              #{})))

  (it "does not descend into nested lambdas"
    (should= []
             (sut/free-locals
              {:op :lambda
               :params [{:name 'x :type 'Int}]
               :return-type 'Int
               :effects []
               :body {:op :local :name 'outside}}
              #{})))

  (it "excludes names bound by loop bindings"
    (should= ['outside]
             (sut/free-locals
              {:op :loop
               :bindings [{:name 'inside
                           :expr {:op :local :name 'outside}}]
               :body {:op :recur
                      :args [{:op :local :name 'inside}]}}
              #{})))

  (it "collects free locals from try, catches, and finally in order"
    (should= ['body-ref 'catch-ref 'finally-ref]
             (sut/free-locals
              {:op :try
               :body {:op :local :name 'body-ref}
               :catches [{:name 'ex
                          :type '(Java java.lang.Exception)
                          :body {:op :local :name 'catch-ref}}]
               :finally {:op :local :name 'finally-ref}}
              #{})))

  (it "collects free locals from static field writes"
    (should= ['replacement]
             (sut/free-locals
              {:op :java-static-set-field
               :class-name 'java.lang.System
               :field-name 'out
               :field-type '(Java java.io.PrintStream)
               :expr {:op :local :name 'replacement}}
              #{})))

  (it "returns no free locals for unknown ops"
    (should= []
             (sut/free-locals
              {:op :unknown}
              #{})))

  (it "collects free locals across remaining expression forms"
    (should= ['left 'right]
             (sut/free-locals
              {:op :seq
               :exprs [{:op :local :name 'left}
                       {:op :local :name 'right}]}
              #{}))
    (should= ['updated]
             (sut/free-locals
              {:op :set
               :name 'slot
               :expr {:op :local :name 'updated}}
              #{'slot}))
    (should= ['receiver 'arg]
             (sut/free-locals
              {:op :java-call
               :target {:op :local :name 'receiver}
               :args [{:op :local :name 'arg}]}
              #{}))
    (should= ['arg]
             (sut/free-locals
              {:op :java-static-call
               :args [{:op :local :name 'arg}]}
              #{}))
    (should= ['receiver]
             (sut/free-locals
              {:op :java-get-field
               :target {:op :local :name 'receiver}}
              #{}))
    (should= ['receiver 'value]
             (sut/free-locals
              {:op :java-set-field
               :target {:op :local :name 'receiver}
               :expr {:op :local :name 'value}}
              #{}))
    (should= []
             (sut/free-locals
              {:op :java-static-get-field}
              #{}))
    (should= ['body-ref 'catch-ref]
             (sut/free-locals
              {:op :try
               :body {:op :local :name 'body-ref}
               :catches [{:name 'ex
                          :type '(Java java.lang.Exception)
                          :body {:op :local :name 'catch-ref}}]}
              #{})))

  (it "collects free locals for constructor, data, and control forms"
    (should= ['arg]
             (sut/free-locals
              {:op :java-new
               :args [{:op :local :name 'arg}]}
              #{}))
    (should= ['field]
             (sut/free-locals
              {:op :construct
               :args [{:op :local :name 'field}]}
              #{}))
    (should= ['payload]
             (sut/free-locals
              {:op :variant
               :args [{:op :local :name 'payload}]}
              #{}))
    (should= ['record]
             (sut/free-locals
              {:op :record-get
               :target {:op :local :name 'record}}
              #{}))
    (should= ['init]
             (sut/free-locals
              {:op :var
               :init {:op :local :name 'init}}
              #{}))
    (should= ['problem]
             (sut/free-locals
              {:op :raise
               :expr {:op :local :name 'problem}}
              #{}))
    (should= ['target 'body-ref]
             (sut/free-locals
              {:op :match
               :target {:op :local :name 'target}
               :cases [{:body {:op :local :name 'body-ref}}]}
              #{}))))
