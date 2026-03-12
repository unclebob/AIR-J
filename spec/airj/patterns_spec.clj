(ns airj.patterns-spec
  (:require [airj.patterns :as sut]
            [speclj.core :refer :all]))

(describe "pattern helpers"
  (let [decls {'Color {:op :enum
                       :name 'Color
                       :variants ['Red 'Blue]}
               'Response {:op :union
                          :name 'Response
                          :type-params []
                          :invariants []
                          :variants [{:name 'Ok
                                      :fields [{:name 'value :type 'Int}]}
                                     {:name 'Error
                                      :fields [{:name 'message :type 'String}]}]}
               'Record {:op :data
                        :name 'Record
                        :type-params []
                        :invariants []
                        :fields [{:name 'status :type 'Int}]}}]
    (it "finds declarations and fields by type name"
      (should= 'Int
               (sut/field-type (sut/decl-for-type decls 'Record) 'status)))

    (it "detects enum variants and enum patterns"
      (should (sut/enum-variant? (sut/decl-for-type decls 'Color) 'Red))
      (should (sut/enum-pattern? {:op :binder-pattern :name 'Red} 'Color decls))
      (should-not (sut/enum-pattern? {:op :binder-pattern :name 'Other} 'Color decls)))

    (it "finds union variants by name"
      (should= {:name 'Ok
                :fields [{:name 'value :type 'Int}]}
               (sut/union-variant (sut/decl-for-type decls 'Response) 'Ok)))

    (it "walks union patterns recursively with injected binder behavior"
      (let [result (sut/bind-pattern {:locals {}}
                                     {:op :union-pattern
                                      :name 'Ok
                                      :args [{:op :binder-pattern
                                              :name 'value}]}
                                     'Response
                                     decls
                                     {:bind-binder (fn [ctx pattern target-type _decls]
                                                     (assoc-in ctx [:locals (:name pattern)] target-type))
                                      :bind-literal (fn [ctx _pattern _target-type] ctx)
                                      :fail! (fn [message data]
                                               (throw (ex-info message data)))})]
        (should= 'Int (get-in result [:locals 'value]))))

    (it "walks record patterns recursively with injected binder behavior"
      (let [result (sut/bind-pattern {:locals {}}
                                     {:op :record-pattern
                                      :type 'Record
                                      :fields [{:name 'status
                                                :pattern {:op :binder-pattern
                                                          :name 'code}}]}
                                     'Record
                                     decls
                                     {:bind-binder (fn [ctx pattern target-type _decls]
                                                     (assoc-in ctx [:locals (:name pattern)] target-type))
                                      :bind-literal (fn [ctx _pattern _target-type] ctx)
                                      :fail! (fn [message data]
                                               (throw (ex-info message data)))})]
        (should= 'Int (get-in result [:locals 'code]))))

    (it "fails for unknown union variants"
      (should-throw clojure.lang.ExceptionInfo
                    (sut/bind-pattern {:locals {}}
                                      {:op :union-pattern
                                       :name 'Missing
                                       :args []}
                                      'Response
                                      decls
                                      {:bind-binder (fn [ctx _pattern _target-type _decls] ctx)
                                       :bind-literal (fn [ctx _pattern _target-type] ctx)
                                       :fail! (fn [message data]
                                                (throw (ex-info message data)))})))

    (it "fails for unknown record fields"
      (should-throw clojure.lang.ExceptionInfo
                    (sut/bind-pattern {:locals {}}
                                      {:op :record-pattern
                                       :type 'Record
                                       :fields [{:name 'missing
                                                 :pattern {:op :binder-pattern
                                                           :name 'code}}]}
                                      'Record
                                      decls
                                      {:bind-binder (fn [ctx _pattern _target-type _decls] ctx)
                                       :bind-literal (fn [ctx _pattern _target-type] ctx)
                                       :fail! (fn [message data]
                                                (throw (ex-info message data)))})))))
