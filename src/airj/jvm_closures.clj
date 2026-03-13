(ns airj.jvm-closures
  (:require [airj.jvm-cells :as jvm-cells]
            [clojure.string :as str]))

(def ^:private primitive-type-tokens
  {:int "Int"
   :boolean "Bool"
   :void "Unit"})

(defn- normalize-type-token
  [jvm-type]
  (-> jvm-type
      (.replace "/" "_")
      (.replace "[" "Arr_")
      (.replace ";" "")))

(defn type-token
  [jvm-type fail!]
  (or (get primitive-type-tokens jvm-type)
      (when (string? jvm-type)
        (normalize-type-token jvm-type))
      (fail! "Unsupported JVM type token."
             {:jvm-type jvm-type})))

(defn closure-interface-name
  [module-name param-types return-type fail!]
  (str module-name
       "$Fn"
       (count param-types)
       "$"
       (str/join "$" (map #(type-token % fail!)
                          (conj (vec param-types) return-type)))))

(defn register-closure-interface!
  [ctx param-types return-type fail!]
  (let [class-name (closure-interface-name (:module-name ctx) param-types return-type fail!)]
    (swap! (:closure-interfaces ctx)
           update class-name
           #(or %
                {:class-name class-name
                 :method-name "apply"
                 :params (vec param-types)
                 :return-type return-type}))
    class-name))

(defn next-closure-class-name!
  [ctx]
  (let [id (swap! (:closure-counter ctx) inc)]
    (str (:module-name ctx)
         "$Lambda$"
         id)))

(defn lower-closure-type
  [type-expr ctx lower-type fail!]
  (register-closure-interface!
   ctx
   (mapv #(lower-type % ctx) (:params type-expr))
   (lower-type (:return-type type-expr) ctx)
   fail!))

(defn lower-inline-lambda-call
  [lambda-expr args ctx bind-local lower-expr lower-type]
  (let [lambda-ctx (reduce (fn [acc param]
                             (bind-local acc (:name param) (:type param)))
                           ctx
                           (:params lambda-expr))]
    {:op :jvm-let
     :bindings (mapv (fn [param arg]
                       {:name (:name param)
                        :expr (lower-expr arg ctx)})
                     (:params lambda-expr)
                     args)
     :body (lower-expr (:body lambda-expr) lambda-ctx)
     :jvm-type (lower-type (:return-type lambda-expr) ctx)}))

(defn lower-fn-value
  [decl ctx {:keys [fail! lower-type lower-param]}]
  (let [param-types (mapv #(lower-type (:type %) ctx) (:params decl))
        return-type (lower-type (:return-type decl) ctx)
        interface-name (register-closure-interface! ctx param-types return-type fail!)
        class-name (next-closure-class-name! ctx)
        params (mapv #(lower-param % ctx) (:params decl))]
    (swap! (:closures ctx) conj
           {:class-name class-name
            :interface-name interface-name
            :method-name "apply"
            :captures []
            :params params
            :return-type return-type
            :body {:op :jvm-invoke-static
                   :owner (str (:owner-module decl))
                   :name (:name decl)
                   :parameter-types param-types
                   :return-type return-type
                   :args (mapv (fn [param]
                                 {:op :jvm-local
                                  :name (:name param)
                                  :jvm-type (:jvm-type param)})
                               params)
                   :jvm-type return-type}})
    {:op :jvm-closure-new
     :class-name class-name
     :args []
     :jvm-type interface-name}))

(declare free-locals)

(defn- append-refs
  [acc refs]
  (reduce (fn [current ref]
            (if (some #{ref} current)
              current
              (conj current ref)))
          acc
          refs))

(defn- free-locals-in-order
  [exprs bound]
  (reduce (fn [acc expr]
            (append-refs acc (free-locals expr bound)))
          []
          exprs))

(defn- literal?
  [expr]
  (or (integer? expr) (string? expr) (true? expr) (false? expr)))

(defn- local-free-locals
  [expr bound]
  (if (contains? bound (:name expr))
    []
    [(:name expr)]))

(defn- bindings-free-locals
  [bindings bound]
  (reduce (fn [{:keys [refs bound]} binding]
            {:refs (append-refs refs (free-locals (:expr binding) bound))
             :bound (conj bound (:name binding))})
          {:refs []
           :bound bound}
          bindings))

(defn- let-free-locals
  [expr bound]
  (let [{:keys [refs bound]} (bindings-free-locals (:bindings expr) bound)]
    (append-refs refs (free-locals (:body expr) bound))))

(defn- loop-free-locals
  [expr bound]
  (let [{:keys [refs bound]} (bindings-free-locals (:bindings expr) bound)]
    (append-refs refs (free-locals (:body expr) bound))))

(defn- try-free-locals
  [expr bound]
  (append-refs
   (free-locals (:body expr) bound)
   (append-refs
    (free-locals-in-order (map :body (:catches expr)) bound)
    (if-let [finally-expr (:finally expr)]
      (free-locals finally-expr bound)
      []))))

(def ^:private free-locals-handlers
  {:local local-free-locals
   :lambda (fn [_ _] [])
   :call (fn [expr bound]
           (append-refs (free-locals (:callee expr) bound)
                        (free-locals-in-order (:args expr) bound)))
   :int-add (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-sub (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-mul (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-div (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-mod (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-eq (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-lt (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-le (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-gt (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-ge (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :bool-eq (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int-ne (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :string-eq (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :string-concat (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :string-split-on (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :int->string (fn [expr bound] (free-locals (:arg expr) bound))
   :string->int (fn [expr bound] (free-locals (:arg expr) bound))
   :string-length (fn [expr bound] (free-locals (:arg expr) bound))
   :string-trim (fn [expr bound] (free-locals (:arg expr) bound))
   :string-empty? (fn [expr bound] (free-locals (:arg expr) bound))
   :seq-length (fn [expr bound] (free-locals (:arg expr) bound))
   :seq-get (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :io-read-line (fn [_expr _bound] [])
   :io-print (fn [expr bound] (free-locals (:arg expr) bound))
   :bool-not (fn [expr bound] (free-locals (:arg expr) bound))
   :bool-and (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :bool-or (fn [expr bound] (free-locals-in-order (:args expr) bound))
   :io-println (fn [expr bound] (free-locals (:arg expr) bound))
   :if (fn [expr bound]
         (free-locals-in-order [(:test expr) (:then expr) (:else expr)] bound))
   :let let-free-locals
   :seq (fn [expr bound]
          (free-locals-in-order (:exprs expr) bound))
   :construct (fn [expr bound]
                (free-locals-in-order (:args expr) bound))
   :variant (fn [expr bound]
              (free-locals-in-order (:args expr) bound))
   :record-get (fn [expr bound]
                 (free-locals (:target expr) bound))
   :var (fn [expr bound]
          (free-locals (:init expr) bound))
   :set (fn [expr bound]
          (append-refs (if (contains? bound (:name expr))
                         []
                         [(:name expr)])
                       (free-locals (:expr expr) bound)))
   :loop loop-free-locals
   :recur (fn [expr bound]
            (free-locals-in-order (:args expr) bound))
   :raise (fn [expr bound]
            (free-locals (:expr expr) bound))
   :try try-free-locals
   :match (fn [expr bound]
            (append-refs
             (free-locals (:target expr) bound)
             (free-locals-in-order (map :body (:cases expr)) bound)))
   :java-new (fn [expr bound]
               (free-locals-in-order (:args expr) bound))
   :java-call (fn [expr bound]
                (append-refs (free-locals (:target expr) bound)
                             (free-locals-in-order (:args expr) bound)))
   :java-static-call (fn [expr bound]
                       (free-locals-in-order (:args expr) bound))
   :java-get-field (fn [expr bound]
                     (free-locals (:target expr) bound))
   :java-set-field (fn [expr bound]
                     (append-refs (free-locals (:target expr) bound)
                                  (free-locals (:expr expr) bound)))
   :java-static-get-field (fn [_expr _bound]
                            [])
   :java-static-set-field (fn [expr bound]
                            (free-locals (:expr expr) bound))})

(defn free-locals
  [expr bound]
  (if (literal? expr)
    []
    (if-let [handler (get free-locals-handlers (:op expr))]
      (handler expr bound)
      [])))

(defn lower-lambda
  [expr ctx {:keys [fail! lower-type lower-expr lower-param bind-local]}]
  (let [param-types (mapv #(lower-type (:type %) ctx) (:params expr))
        return-type (lower-type (:return-type expr) ctx)
        interface-name (register-closure-interface! ctx param-types return-type fail!)
        class-name (next-closure-class-name! ctx)
        param-names (into #{} (map :name) (:params expr))
        capture-names (filterv #(and (contains? (:locals ctx) %)
                                     (not (contains? param-names %)))
                               (free-locals (:body expr) param-names))
        mutable-locals (:mutable-locals ctx)
        captures (mapv (fn [name]
                         (let [type-expr (get-in ctx [:locals name])
                               mutable? (contains? mutable-locals name)
                               value-jvm-type (lower-type type-expr ctx)]
                           {:name name
                            :type-expr type-expr
                            :mutable? mutable?
                            :jvm-type (if mutable?
                                        (jvm-cells/cell-jvm-type value-jvm-type)
                                        value-jvm-type)}))
                       capture-names)
        lambda-ctx (reduce (fn [current capture]
                             (cond-> (bind-local current
                                                 (:name capture)
                                                 (:type-expr capture))
                               (:mutable? capture)
                               (assoc-in [:mutable-locals (:name capture)]
                                         (:type-expr capture))))
                           (assoc ctx
                                  :locals {}
                                  :lambdas {}
                                  :mutable-locals {})
                           captures)
        lambda-ctx (reduce (fn [current param]
                             (bind-local current (:name param) (:type param)))
                           lambda-ctx
                           (:params expr))]
    (swap! (:closures ctx) conj
           {:class-name class-name
            :interface-name interface-name
            :method-name "apply"
            :captures (mapv #(select-keys % [:name :jvm-type]) captures)
            :params (mapv #(lower-param % ctx) (:params expr))
            :return-type return-type
            :body (lower-expr (:body expr) lambda-ctx)})
    {:op :jvm-closure-new
     :class-name class-name
     :args (mapv (fn [capture]
                   (if (:mutable? capture)
                     {:op :jvm-local
                      :name (:name capture)
                      :jvm-type (:jvm-type capture)}
                     (lower-expr {:op :local :name (:name capture)} ctx)))
                 captures)
     :jvm-type interface-name}))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T13:48:23.640669-05:00", :module-hash "572426184", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "376211175"} {:id "def/primitive-type-tokens", :kind "def", :line 5, :end-line 8, :hash "2041793518"} {:id "defn-/normalize-type-token", :kind "defn-", :line 10, :end-line 15, :hash "-120487065"} {:id "defn/type-token", :kind "defn", :line 17, :end-line 23, :hash "-1071139432"} {:id "defn/closure-interface-name", :kind "defn", :line 25, :end-line 32, :hash "312285882"} {:id "defn/register-closure-interface!", :kind "defn", :line 34, :end-line 44, :hash "-511433516"} {:id "defn/next-closure-class-name!", :kind "defn", :line 46, :end-line 51, :hash "-1697851975"} {:id "defn/lower-closure-type", :kind "defn", :line 53, :end-line 59, :hash "-358020799"} {:id "defn/lower-inline-lambda-call", :kind "defn", :line 61, :end-line 74, :hash "-713637770"} {:id "defn/lower-fn-value", :kind "defn", :line 76, :end-line 104, :hash "289364577"} {:id "form/10/declare", :kind "declare", :line 106, :end-line 106, :hash "-1406156954"} {:id "defn-/append-refs", :kind "defn-", :line 108, :end-line 115, :hash "-1302944863"} {:id "defn-/free-locals-in-order", :kind "defn-", :line 117, :end-line 122, :hash "1071735307"} {:id "defn-/literal?", :kind "defn-", :line 124, :end-line 126, :hash "-360436424"} {:id "defn-/local-free-locals", :kind "defn-", :line 128, :end-line 132, :hash "1999702150"} {:id "defn-/bindings-free-locals", :kind "defn-", :line 134, :end-line 141, :hash "-1282158552"} {:id "defn-/let-free-locals", :kind "defn-", :line 143, :end-line 146, :hash "-1817839784"} {:id "defn-/loop-free-locals", :kind "defn-", :line 148, :end-line 151, :hash "-266376812"} {:id "defn-/try-free-locals", :kind "defn-", :line 153, :end-line 161, :hash "316088900"} {:id "def/free-locals-handlers", :kind "def", :line 163, :end-line 240, :hash "-447364681"} {:id "defn/free-locals", :kind "defn", :line 242, :end-line 248, :hash "1848382373"} {:id "defn/lower-lambda", :kind "defn", :line 250, :end-line 305, :hash "1534189032"}]}
;; clj-mutate-manifest-end
