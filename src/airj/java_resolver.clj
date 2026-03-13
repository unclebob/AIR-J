(ns airj.java-resolver
  (:require [airj.expr-walker :as expr-walker]
            [airj.java-members :as java-members]
            [airj.java-types :as java-types]
            [airj.patterns :as patterns]
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

(defn- bind-local
  [ctx name type-expr]
  (assoc-in ctx [:locals name] type-expr))

(defn- bind-pattern-local
  [ctx pattern target-type _decls]
  (bind-local ctx (:name pattern) target-type))

(defn- bind-pattern-locals
  [ctx pattern target-type decls]
  (patterns/bind-pattern ctx
                         pattern
                         target-type
                         decls
                         {:bind-binder bind-pattern-local
                          :bind-literal (fn [current _pattern _target-type] current)
                          :fail! fail!}))

(defn- binding-ctx
  [ctx binding decls]
  (bind-local ctx
              (:name binding)
              (type-checker/infer-expr-type (:expr binding) ctx decls)))

(defn- let-handler
  [expr ctx decls walk]
  (let [body-ctx (reduce (fn [current binding]
                           (walk (:expr binding) current)
                           (binding-ctx current binding decls))
                         ctx
                         (:bindings expr))]
    (walk (:body expr) body-ctx)))

(defn- seq-handler
  [expr ctx walk]
  (reduce (fn [current part]
            (walk part current)
            (if (= :var (:op part))
              (bind-local current (:name part) (:type part))
              current))
          ctx
          (:exprs expr))
  nil)

(defn- loop-handler
  [expr ctx decls walk]
  (let [body-ctx (reduce (fn [current binding]
                           (walk (:expr binding) current)
                           (binding-ctx current binding decls))
                         ctx
                         (:bindings expr))]
    (walk (:body expr) body-ctx)))

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
            (let [target-type (type-checker/infer-expr-type (:target expr) ctx decls)]
              (doseq [case (:cases expr)]
                (walk (:body case)
                      (bind-pattern-locals ctx
                                           (:pattern case)
                                           target-type
                                           decls)))))
   :let (fn [expr ctx walk]
          (let-handler expr ctx decls walk))
   :seq (fn [expr ctx walk]
          (seq-handler expr ctx walk))
   :lambda (fn [expr ctx walk]
             (walk (:body expr)
                   (reduce (fn [current param]
                             (bind-local current (:name param) (:type param)))
                           ctx
                           (:params expr))))
   :try (fn [expr ctx walk]
          (walk (:body expr) ctx)
          (doseq [catch (:catches expr)]
            (walk (:body catch)
                  (bind-local ctx (:name catch) (:type catch))))
          (when-let [finally-expr (:finally expr)]
            (walk finally-expr ctx)))
   :var (fn [expr ctx walk]
          (walk (:init expr) ctx))
   :set (fn [expr ctx walk]
          (walk (:expr expr) ctx))
   :loop (fn [expr ctx walk]
           (loop-handler expr ctx decls walk))
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
;; {:version 1, :tested-at "2026-03-12T17:23:26.25694-05:00", :module-hash "1685599649", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "1958592209"} {:id "defn-/fail!", :kind "defn-", :line 8, :end-line 10, :hash "879938479"} {:id "defn-/decl-map", :kind "defn-", :line 12, :end-line 14, :hash "1732448350"} {:id "defn-/imported-java-classes", :kind "defn-", :line 16, :end-line 21, :hash "-1786178745"} {:id "defn-/require-java-import!", :kind "defn-", :line 23, :end-line 27, :hash "-900571965"} {:id "defn-/load-class", :kind "defn-", :line 29, :end-line 35, :hash "-546840002"} {:id "defn/resolve-type", :kind "defn", :line 37, :end-line 41, :hash "-2002111120"} {:id "defn-/java-class-type?", :kind "defn-", :line 43, :end-line 45, :hash "-1231681719"} {:id "defn-/target-class", :kind "defn-", :line 47, :end-line 54, :hash "1738177563"} {:id "defn-/validate-java-new", :kind "defn-", :line 56, :end-line 65, :hash "-1210115351"} {:id "defn-/validate-java-call", :kind "defn-", :line 67, :end-line 77, :hash "311048070"} {:id "defn-/validate-java-static-call", :kind "defn-", :line 79, :end-line 89, :hash "325162171"} {:id "defn-/validate-java-get-field", :kind "defn-", :line 91, :end-line 101, :hash "25960113"} {:id "defn-/validate-java-set-field", :kind "defn-", :line 103, :end-line 113, :hash "-434496599"} {:id "defn-/bind-local", :kind "defn-", :line 115, :end-line 117, :hash "1658319653"} {:id "defn-/bind-pattern-local", :kind "defn-", :line 119, :end-line 121, :hash "-357983377"} {:id "defn-/bind-pattern-locals", :kind "defn-", :line 123, :end-line 131, :hash "-954573492"} {:id "defn-/binding-ctx", :kind "defn-", :line 133, :end-line 137, :hash "959377042"} {:id "defn-/let-handler", :kind "defn-", :line 139, :end-line 146, :hash "1021191117"} {:id "defn-/seq-handler", :kind "defn-", :line 148, :end-line 157, :hash "176826092"} {:id "defn-/loop-handler", :kind "defn-", :line 159, :end-line 166, :hash "282383021"} {:id "defn-/handlers", :kind "defn-", :line 168, :end-line 242, :hash "-149407153"} {:id "defn-/fn-ctx", :kind "defn-", :line 244, :end-line 259, :hash "1188188464"} {:id "defn-/check-fn-decl", :kind "defn-", :line 261, :end-line 265, :hash "-1251467863"} {:id "defn/check-module", :kind "defn", :line 267, :end-line 274, :hash "756817389"}]}
;; clj-mutate-manifest-end
