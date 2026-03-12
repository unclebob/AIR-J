(ns airj.java-resolver
  (:require [airj.expr-walker :as expr-walker]
            [airj.java-members :as java-members]
            [airj.java-types :as java-types]
            [airj.type-checker :as type-checker]))

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- decl-map
  [module]
  (into {} (map (juxt :name identity) (:decls module))))

(defn- imported-java-classes
  [module]
  (->> (:imports module)
       (filter #(= :java-import (:op %)))
       (map :class-name)
       set))

(defn- require-java-import!
  [class-name imports]
  (when-not (contains? imports class-name)
    (fail! "Missing Java import."
           {:class-name class-name})))

(defn- load-class
  [class-name]
  (try
    (java-types/load-class class-name)
    (catch ClassNotFoundException _
      (fail! "Unknown Java class."
             {:class-name class-name}))))

(defn resolve-type
  [type-expr]
  (or (java-types/resolve-type type-expr)
      (fail! "Unsupported Java type mapping."
             {:type type-expr})))

(defn- java-class-type?
  [type-expr]
  (java-types/java-type-expr? type-expr))

(defn- target-class
  [expr ctx decls imports]
  (let [target-type (type-checker/infer-expr-type expr ctx decls)]
    (when-not (java-class-type? target-type)
      (fail! "Expected Java receiver type."
             {:type target-type}))
    (require-java-import! (second target-type) imports)
    (resolve-type target-type)))

(defn- validate-java-new
  [expr ctx decls imports]
  (require-java-import! (:class-name expr) imports)
  (load-class (:class-name expr))
  (let [arg-types (mapv #(resolve-type (type-checker/infer-expr-type % ctx decls))
                        (:args expr))]
    (when-not (java-members/resolve-constructor (:class-name expr) arg-types)
      (fail! "Unknown Java constructor."
             {:class-name (:class-name expr)
              :arg-types arg-types}))))

(defn- validate-java-call
  [expr ctx decls imports]
  (let [receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
        klass (target-class (:target expr) ctx decls imports)]
    (when-not (java-members/resolve-instance-method receiver-type
                                                    (:member-id expr)
                                                    (:signature expr))
      (fail! "Unknown Java method."
             {:class-name (.getName klass)
              :member-id (:member-id expr)
              :signature (:signature expr)}))))

(defn- validate-java-static-call
  [expr _ctx _decls imports]
  (require-java-import! (:class-name expr) imports)
  (when-not (java-members/resolve-static-method (:class-name expr)
                                                (:member-id expr)
                                                (:signature expr))
    (let [klass (load-class (:class-name expr))]
      (fail! "Unknown Java method."
             {:class-name (:class-name expr)
              :member-id (:member-id expr)
              :signature (:signature expr)}))))

(defn- validate-java-get-field
  [expr ctx decls imports]
  (let [receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
        klass (target-class (:target expr) ctx decls imports)]
    (when-not (java-members/resolve-instance-field receiver-type
                                                   (:field-name expr)
                                                   (:field-type expr))
      (fail! "Unknown Java field."
             {:class-name (.getName klass)
              :field-name (:field-name expr)
              :field-type (:field-type expr)}))))

(defn- validate-java-set-field
  [expr ctx decls imports]
  (let [receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
        klass (target-class (:target expr) ctx decls imports)]
    (when-not (java-members/resolve-instance-field receiver-type
                                                   (:field-name expr)
                                                   (:field-type expr))
      (fail! "Unknown Java field."
             {:class-name (.getName klass)
              :field-name (:field-name expr)
              :field-type (:field-type expr)}))))

(defn- handlers
  [decls imports]
  {:java-new (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx))
               (validate-java-new expr ctx decls imports))
   :java-call (fn [expr ctx walk]
                (walk (:target expr) ctx)
                (doseq [arg (:args expr)]
                  (walk arg ctx))
                (validate-java-call expr ctx decls imports))
   :java-static-call (fn [expr ctx walk]
                       (doseq [arg (:args expr)]
                         (walk arg ctx))
                       (validate-java-static-call expr ctx decls imports))
   :java-get-field (fn [expr ctx walk]
                     (walk (:target expr) ctx)
                     (validate-java-get-field expr ctx decls imports))
   :java-set-field (fn [expr ctx walk]
                     (walk (:target expr) ctx)
                     (walk (:expr expr) ctx)
                     (validate-java-set-field expr ctx decls imports))
   :call (fn [expr ctx walk]
           (walk (:callee expr) ctx)
           (doseq [arg (:args expr)]
             (walk arg ctx)))
   :construct (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :variant (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :record-get (fn [expr ctx walk]
                 (walk (:target expr) ctx))
   :if (fn [expr ctx walk]
         (walk (:test expr) ctx)
         (walk (:then expr) ctx)
         (walk (:else expr) ctx))
   :match (fn [expr ctx walk]
            (walk (:target expr) ctx)
            (doseq [case (:cases expr)]
              (walk (:body case) ctx)))
   :let (fn [expr ctx walk]
          (doseq [binding (:bindings expr)]
            (walk (:expr binding) ctx))
          (walk (:body expr) ctx))
   :seq (fn [expr ctx walk]
          (doseq [part (:exprs expr)]
            (walk part ctx)))
   :lambda (fn [expr ctx walk]
             (walk (:body expr) ctx))
   :try (fn [expr ctx walk]
          (walk (:body expr) ctx)
          (doseq [catch (:catches expr)]
            (walk (:body catch) ctx))
          (when-let [finally-expr (:finally expr)]
            (walk finally-expr ctx)))
   :var (fn [expr ctx walk]
          (walk (:init expr) ctx))
   :set (fn [expr ctx walk]
          (walk (:expr expr) ctx))
   :loop (fn [expr ctx walk]
           (doseq [binding (:bindings expr)]
             (walk (:expr binding) ctx))
           (walk (:body expr) ctx))
   :recur (fn [expr ctx walk]
            (doseq [arg (:args expr)]
              (walk arg ctx)))
   :raise (fn [expr ctx walk]
            (walk (:expr expr) ctx))})

(defn- fn-ctx
  [decl decls]
  {:locals (merge
            (into {}
                  (map (fn [param]
                         [(:name param) (:type param)])
                       (:params decl)))
            (into {}
                  (map (fn [fn-decl]
                         [(:name fn-decl)
                          {:op :fn-type
                           :params (mapv :type (:params fn-decl))
                           :return-type (:return-type fn-decl)
                           :effects (vec (:effects fn-decl))}])
                       (filter #(= :fn (:op %)) (vals decls)))))
   :mutable #{}})

(defn- check-fn-decl
  [decl decls imports]
  (expr-walker/walk-expr (:body decl)
                         (fn-ctx decl decls)
                         (handlers decls imports)))

(defn check-module
  [module]
  (let [decls (decl-map module)
        imports (imported-java-classes module)]
    (doseq [decl (:decls module)]
      (when (= :fn (:op decl))
        (check-fn-decl decl decls imports)))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T12:29:18.28011-05:00", :module-hash "1175166460", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "163065124"} {:id "defn-/fail!", :kind "defn-", :line 7, :end-line 9, :hash "879938479"} {:id "defn-/decl-map", :kind "defn-", :line 11, :end-line 13, :hash "1732448350"} {:id "defn-/imported-java-classes", :kind "defn-", :line 15, :end-line 20, :hash "-1488855454"} {:id "defn-/require-java-import!", :kind "defn-", :line 22, :end-line 26, :hash "-900571965"} {:id "defn-/load-class", :kind "defn-", :line 28, :end-line 34, :hash "-546840002"} {:id "defn/resolve-type", :kind "defn", :line 36, :end-line 40, :hash "-2002111120"} {:id "defn-/java-class-type?", :kind "defn-", :line 42, :end-line 44, :hash "-1231681719"} {:id "defn-/target-class", :kind "defn-", :line 46, :end-line 53, :hash "1738177563"} {:id "defn-/validate-java-new", :kind "defn-", :line 55, :end-line 64, :hash "1186244425"} {:id "defn-/validate-java-call", :kind "defn-", :line 66, :end-line 76, :hash "311048070"} {:id "defn-/validate-java-static-call", :kind "defn-", :line 78, :end-line 88, :hash "325162171"} {:id "defn-/validate-java-get-field", :kind "defn-", :line 90, :end-line 100, :hash "25960113"} {:id "defn-/validate-java-set-field", :kind "defn-", :line 102, :end-line 112, :hash "-434496599"} {:id "defn-/handlers", :kind "defn-", :line 114, :end-line 183, :hash "2092009341"} {:id "defn-/fn-ctx", :kind "defn-", :line 185, :end-line 200, :hash "-300822629"} {:id "defn-/check-fn-decl", :kind "defn-", :line 202, :end-line 206, :hash "-1251467863"} {:id "defn/check-module", :kind "defn", :line 208, :end-line 215, :hash "756817389"}]}
;; clj-mutate-manifest-end
