(ns airj.jvm-lowerer-match-spec
  (:require [airj.jvm-lowerer-match :as sut]
            [speclj.core :refer :all]))

(describe "JVM lowerer match"
  (it "lowers union match cases into JVM match cases"
    (let [ctx {:module-name 'example/match
               :decls {'Result {:op :union
                                :name 'Result
                                :variants [{:name 'Done
                                            :fields [{:name 'value :type 'Int}]}
                                           {:name 'Failed
                                            :fields [{:name 'message :type 'String}]}]}}
               :locals {'result 'Result}}
          expr {:op :match
                :target {:op :local :name 'result}
                :cases [{:pattern {:op :union-pattern
                                   :name 'Done
                                   :args [{:op :binder-pattern :name 'value}]}
                         :body {:op :local :name 'value}}
                        {:pattern {:op :union-pattern
                                   :name 'Failed
                                   :args [{:op :binder-pattern :name 'message}]}
                         :body 0}]}
          helpers {:fail! (fn [message data] (throw (ex-info message data)))
                   :lower-expr (fn lower-expr [subexpr subctx]
                                 (if (= :local (:op subexpr))
                                   {:op :jvm-local
                                    :name (:name subexpr)
                                    :jvm-type (cond
                                                (= 'Int (get-in subctx [:locals (:name subexpr)])) :int
                                                (= 'String (get-in subctx [:locals (:name subexpr)])) "java/lang/String"
                                                (= 'Result (get-in subctx [:locals (:name subexpr)])) "example/match$Result")}
                                   {:op :jvm-int
                                    :value subexpr
                                    :jvm-type :int}))
                   :infer-type (fn infer-type [subexpr subctx]
                                 (case (:op subexpr)
                                   :local (get-in subctx [:locals (:name subexpr)])
                                   :match 'Int))
                   :lower-type (fn lower-type [type-expr _]
                                 (cond
                                   (= 'Int type-expr) :int
                                   (= 'String type-expr) "java/lang/String"
                                   (= 'Result type-expr) "example/match$Result"))
                   :union-variant (fn [subctx union-name variant-name]
                                    (->> (get-in subctx [:decls union-name :variants])
                                         (some #(when (= variant-name (:name %)) %))))
                   :union-variant-class-name (fn [module-name union-name variant-name]
                                               (str module-name "$" union-name "$" variant-name))
                   :bind-local (fn [subctx name type-expr]
                                 (assoc-in subctx [:locals name] type-expr))}]
      (should= {:op :jvm-match
                :target {:op :jvm-local
                         :name 'result
                         :jvm-type "example/match$Result"}
                :cases [{:test {:op :jvm-instance-of
                                :target {:op :jvm-local
                                         :name 'result
                                         :jvm-type "example/match$Result"}
                                :class-name "example/match$Result$Done"}
                         :bindings [{:name 'value
                                     :expr {:op :jvm-variant-field
                                            :target {:op :jvm-local
                                                     :name 'result
                                                     :jvm-type "example/match$Result"}
                                            :class-name "example/match$Result$Done"
                                            :field 'value
                                            :jvm-type :int}}]
                         :body {:op :jvm-local
                                :name 'value
                                :jvm-type :int}}
                        {:test {:op :jvm-instance-of
                                :target {:op :jvm-local
                                         :name 'result
                                         :jvm-type "example/match$Result"}
                                :class-name "example/match$Result$Failed"}
                         :bindings [{:name 'message
                                     :expr {:op :jvm-variant-field
                                            :target {:op :jvm-local
                                                     :name 'result
                                                     :jvm-type "example/match$Result"}
                                            :class-name "example/match$Result$Failed"
                                            :field 'message
                                            :jvm-type "java/lang/String"}}]
                         :body {:op :jvm-int
                                :value 0
                                :jvm-type :int}}]
                :jvm-type :int}
               (sut/lower-match expr ctx helpers))))

  (it "infers match result type by joining case body types"
    (let [ctx {:decls {'Result {:op :union
                                :name 'Result
                                :variants [{:name 'Done
                                            :fields [{:name 'value :type 'Int}]}
                                           {:name 'Failed
                                            :fields [{:name 'message :type 'String}]}]}}
               :locals {'result 'Result}}
          expr {:op :match
                :target {:op :local :name 'result}
                :cases [{:pattern {:op :union-pattern
                                   :name 'Done
                                   :args [{:op :binder-pattern :name 'value}]}
                         :body {:op :local :name 'value}}
                        {:pattern {:op :union-pattern
                                   :name 'Failed
                                   :args [{:op :binder-pattern :name 'message}]}
                         :body 0}]}
          helpers {:fail! (fn [message data] (throw (ex-info message data)))
                   :infer-type (fn infer-type [subexpr subctx]
                                 (cond
                                   (integer? subexpr) 'Int
                                   (= :local (:op subexpr)) (get-in subctx [:locals (:name subexpr)])))
                   :union-variant (fn [subctx union-name variant-name]
                                    (->> (get-in subctx [:decls union-name :variants])
                                         (some #(when (= variant-name (:name %)) %))))
                   :bind-local (fn [subctx name type-expr]
                                 (assoc-in subctx [:locals name] type-expr))
                   :join-branch-types (fn [current next-branch _]
                                        (if (= current next-branch)
                                          current
                                          (throw (ex-info "Lowered branch types must agree."
                                                          {:expected current
                                                           :actual next-branch}))))}]
      (should= 'Int
               (sut/infer-match-type expr ctx helpers)))))
