(ns airj.module-interface)

(defn- exported-decls
  [module]
  (let [exports (set (:exports module))]
    (filter #(contains? exports (:name %)) (:decls module))))

(defmulti ^:private interface-decl :op)

(defmethod interface-decl :data
  [decl]
  {:op :data
   :name (:name decl)
   :type-params (vec (:type-params decl))
   :fields (mapv (fn [field]
                   {:name (:name field)
                    :type (:type field)})
                 (:fields decl))})

(defmethod interface-decl :enum
  [decl]
  {:op :enum
   :name (:name decl)
   :variants (vec (:variants decl))})

(defmethod interface-decl :union
  [decl]
  {:op :union
   :name (:name decl)
   :type-params (vec (:type-params decl))
   :variants (mapv (fn [variant]
                     {:name (:name variant)
                      :fields (mapv (fn [field]
                                      {:name (:name field)
                                       :type (:type field)})
                                    (:fields variant))})
                   (:variants decl))})

(defmethod interface-decl :fn
  [decl]
  {:op :fn
   :name (:name decl)
   :params (mapv (fn [param]
                   {:name (:name param)
                    :type (:type param)})
                 (:params decl))
   :return-type (:return-type decl)
   :effects (vec (:effects decl))})

(defn extract-interface
  [module]
  {:name (:name module)
   :imports (vec (:imports module))
   :exports (vec (:exports module))
   :decls (mapv interface-decl (exported-decls module))})

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T12:38:56.658578-05:00", :module-hash "-264858494", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-1213040533"} {:id "defn-/exported-decls", :kind "defn-", :line 3, :end-line 6, :hash "561377682"} {:id "defmulti/interface-decl", :kind "defmulti", :line 8, :end-line 8, :hash "1850949441"} {:id "defmethod/interface-decl/:data", :kind "defmethod", :line 10, :end-line 18, :hash "265478117"} {:id "defmethod/interface-decl/:enum", :kind "defmethod", :line 20, :end-line 24, :hash "1508552064"} {:id "defmethod/interface-decl/:union", :kind "defmethod", :line 26, :end-line 37, :hash "-858643393"} {:id "defmethod/interface-decl/:fn", :kind "defmethod", :line 39, :end-line 48, :hash "1824397938"} {:id "defn/extract-interface", :kind "defn", :line 50, :end-line 55, :hash "-644411584"}]}
;; clj-mutate-manifest-end
