(ns airj.jvm-lowerer
  (:require [airj.imported-interfaces :as imported-interfaces]
            [airj.jvm-lowerer-expr :as expr]
            [airj.jvm-lowerer-types :as types]))

(defn- lower-field
  [field ctx]
  {:name (:name field)
   :jvm-type (types/lower-type (:type field) ctx)})

(defn- lower-record
  [ctx decl]
  {:name (:name decl)
   :class-name (types/nested-class-name (:module-name ctx) (:name decl))
   :fields (mapv #(lower-field % ctx) (:fields decl))})

(defn- lower-enum
  [ctx decl]
  {:name (:name decl)
   :class-name (types/nested-class-name (:module-name ctx) (:name decl))
   :variants (vec (:variants decl))})

(defn- lower-union-variant
  [ctx union-name variant]
  {:name (:name variant)
   :class-name (types/union-variant-class-name (:module-name ctx) union-name (:name variant))
   :fields (mapv #(lower-field % ctx) (:fields variant))})

(defn- lower-union
  [ctx decl]
  {:name (:name decl)
   :base-class (types/nested-class-name (:module-name ctx) (:name decl))
   :variants (mapv #(lower-union-variant ctx (:name decl) %) (:variants decl))})

(defn- lower-decls
  [decls op lower-fn]
  (->> decls
       (filter #(= op (:op %)))
       (mapv lower-fn)))

(defn lower-module
  [module]
  (let [decls (types/decl-map module)
        ctx {:module-name (:name module)
             :decls decls
             :imported-decls (imported-interfaces/imported-decls module)
             :closure-counter (atom 0)
             :closures (atom [])
             :closure-interfaces (atom {})}
        lower-method* (fn [decl]
                        (let [method-ctx (assoc ctx
                                           :locals (into {}
                                                         (map (fn [param]
                                                                [(:name param) (:type param)]))
                                                         (:params decl)))]
                          {:name (:name decl)
                           :owner (types/internal-name (:module-name ctx))
                           :params (mapv #(expr/lower-param % method-ctx) (:params decl))
                           :return-type (types/lower-type (:return-type decl) method-ctx)
                           :effects (vec (:effects decl))
                           :body (expr/lower-expr (:body decl) method-ctx)}))]
    (cond-> {:op :jvm-module
             :module-name (:name module)
             :internal-name (types/internal-name (:name module))
             :exports (vec (:exports module))
             :records (lower-decls (:decls module) :data #(lower-record ctx %))
             :enums (lower-decls (:decls module) :enum #(lower-enum ctx %))
             :unions (lower-decls (:decls module) :union #(lower-union ctx %))
             :methods (lower-decls (:decls module) :fn lower-method*)}
      (seq @(:closure-interfaces ctx))
      (assoc :closure-interfaces (->> @(:closure-interfaces ctx) vals (sort-by :class-name) vec))
      (seq @(:closures ctx))
      (assoc :closures (vec @(:closures ctx))))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T13:36:39.130199-05:00", :module-hash "-1143569854", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "-1410994628"} {:id "defn-/lower-field", :kind "defn-", :line 6, :end-line 9, :hash "-959639482"} {:id "defn-/lower-record", :kind "defn-", :line 11, :end-line 15, :hash "892928500"} {:id "defn-/lower-enum", :kind "defn-", :line 17, :end-line 21, :hash "507139670"} {:id "defn-/lower-union-variant", :kind "defn-", :line 23, :end-line 27, :hash "-503436825"} {:id "defn-/lower-union", :kind "defn-", :line 29, :end-line 33, :hash "-1311118607"} {:id "defn-/lower-decls", :kind "defn-", :line 35, :end-line 39, :hash "625096245"} {:id "defn/lower-module", :kind "defn", :line 41, :end-line 73, :hash "227944578"}]}
;; clj-mutate-manifest-end
