(ns airj.compiler
  (:require [airj.effect-checker :as effect-checker]
            [airj.exhaustiveness-checker :as exhaustiveness-checker]
            [airj.java-resolver :as java-resolver]
            [airj.linker :as linker]
            [airj.module-interface :as module-interface]
            [airj.project-files :as project-files]
            [airj.project-sources :as project-sources]
            [airj.jvm-emitter :as jvm-emitter]
            [airj.jvm-lowerer :as jvm-lowerer]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]
            [airj.resolver :as resolver]
            [airj.type-checker :as type-checker]
            [clojure.java.io :as io])
  (:import (java.net URL URLClassLoader)))

(declare write-bundle!)

(def ^:private contract-result-name '__airj_result)
(def ^:private contract-unit-name '__airj_contract_unit)
(def ^:private invariant-self-name 'self)

(defn- contract-failure-expr
  [kind fn-name]
  {:op :raise
   :expr {:op :java-new
          :class-name 'java.lang.IllegalStateException
          :type-args []
          :args [(str kind " failed: " fn-name)]}})

(defn- contract-check-expr
  [contract kind fn-name]
  {:op :if
   :test contract
   :then true
   :else (contract-failure-expr kind fn-name)})

(defn- with-contract-checks
  [expr contracts kind fn-name]
  (if (seq contracts)
    {:op :seq
     :exprs (vec (concat (map #(contract-check-expr % kind fn-name) contracts)
                         [expr]))}
    expr))

(defn- with-postcondition-checks
  [decl]
  (let [body (:body decl)
        fn-name (:name decl)
        post-checks (map #(contract-check-expr % "Postcondition" fn-name)
                         (:ensures decl))]
    (cond
      (empty? post-checks)
      body

      (= 'Unit (:return-type decl))
      {:op :seq
       :exprs (vec (concat [body]
                           post-checks
                           [{:op :var
                             :name contract-unit-name
                             :type 'Int
                             :init 0}]))}

      :else
      {:op :let
       :bindings [{:name contract-result-name
                   :expr body}]
       :body {:op :seq
              :exprs (vec (concat post-checks
                                  [{:op :local
                                    :name contract-result-name}]))}})))

(defn- local-expr
  [name]
  {:op :local
   :name name})

(defn- instrument-data-invariants
  [expr decl]
  (if (seq (:invariants decl))
    (let [temp-bindings (mapv (fn [field arg]
                                {:name (gensym "__airj_field_")
                                 :expr arg})
                              (:fields decl)
                              (:args expr))
          field-bindings (mapv (fn [field temp-binding]
                                 {:name (:name field)
                                  :expr (local-expr (:name temp-binding))})
                               (:fields decl)
                               temp-bindings)
          rebound-expr (assoc expr
                              :args (mapv (fn [field]
                                            (local-expr (:name field)))
                                          (:fields decl)))]
      {:op :let
       :bindings (vec (concat temp-bindings field-bindings))
       :body (with-contract-checks rebound-expr
                                   (:invariants decl)
                                   "Invariant"
                                   (:name decl))})
    expr))

(defn- instrument-union-invariants
  [expr decl]
  (if (seq (:invariants decl))
    (let [temp-bindings (mapv (fn [_ arg]
                                {:name (gensym "__airj_variant_arg_")
                                 :expr arg})
                              (:args expr)
                              (:args expr))
          rebound-expr (assoc expr
                              :args (mapv (fn [binding]
                                            (local-expr (:name binding)))
                                          temp-bindings))]
      {:op :let
       :bindings (vec (concat temp-bindings
                              [{:name invariant-self-name
                                :expr rebound-expr}]))
       :body (with-contract-checks (local-expr invariant-self-name)
                                   (:invariants decl)
                                   "Invariant"
                                   (:name decl))})
    expr))

(declare instrument-expr)

(defn- instrument-exprs
  [exprs decls]
  (mapv #(instrument-expr % decls) exprs))

(defn- instrument-bindings
  [bindings decls]
  (mapv (fn [binding]
          (update binding :expr instrument-expr decls))
        bindings))

(defn- instrument-catches
  [catches decls]
  (mapv (fn [catch]
          (update catch :body instrument-expr decls))
        catches))

(defn- instrument-match-cases
  [cases decls]
  (mapv (fn [case]
          (update case :body instrument-expr decls))
        cases))

(defn- instrument-try-expr
  [expr decls]
  (-> expr
      (update :body instrument-expr decls)
      (update :catches instrument-catches decls)
      (update :finally #(when % (instrument-expr % decls)))))

(defn- instrument-java-call-expr
  [expr decls]
  (-> expr
      (update :target instrument-expr decls)
      (update :args instrument-exprs decls)))

(defn- instrument-java-set-field-expr
  [expr decls]
  (-> expr
      (update :target instrument-expr decls)
      (update :expr instrument-expr decls)))

(defn- instrument-call-expr
  [expr decls]
  (-> expr
      (update :callee instrument-expr decls)
      (update :args instrument-exprs decls)))

(defn- instrument-construct-expr
  [expr decls]
  (update expr :args instrument-exprs decls))

(defn- instrument-record-get-expr
  [expr decls]
  (update expr :target instrument-expr decls))

(defn- instrument-if-expr
  [expr decls]
  (-> expr
      (update :test instrument-expr decls)
      (update :then instrument-expr decls)
      (update :else instrument-expr decls)))

(defn- instrument-match-expr
  [expr decls]
  (-> expr
      (update :target instrument-expr decls)
      (update :cases instrument-match-cases decls)))

(defn- instrument-let-expr
  [expr decls]
  (-> expr
      (update :bindings instrument-bindings decls)
      (update :body instrument-expr decls)))

(defn- instrument-lambda-expr
  [expr decls]
  (update expr :body instrument-expr decls))

(defn- instrument-loop-expr
  [expr decls]
  (-> expr
      (update :bindings instrument-bindings decls)
      (update :body instrument-expr decls)))

(def ^:private expr-instrumenters
  {:call instrument-call-expr
   :int-add (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-sub (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-mul (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-div (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-mod (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-eq (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-lt (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-le (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-gt (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-ge (fn [expr decls] (update expr :args instrument-exprs decls))
   :bool-eq (fn [expr decls] (update expr :args instrument-exprs decls))
   :bool-not (fn [expr decls] (update expr :arg instrument-expr decls))
   :bool-and (fn [expr decls] (update expr :args instrument-exprs decls))
   :bool-or (fn [expr decls] (update expr :args instrument-exprs decls))
   :construct instrument-construct-expr
   :variant instrument-construct-expr
   :record-get instrument-record-get-expr
   :if instrument-if-expr
   :match instrument-match-expr
   :let instrument-let-expr
   :seq (fn [expr decls]
          (update expr :exprs instrument-exprs decls))
   :lambda instrument-lambda-expr
   :try instrument-try-expr
   :var (fn [expr decls]
          (update expr :init instrument-expr decls))
   :set (fn [expr decls]
          (update expr :expr instrument-expr decls))
   :loop instrument-loop-expr
   :recur (fn [expr decls]
            (update expr :args instrument-exprs decls))
   :raise (fn [expr decls]
            (update expr :expr instrument-expr decls))
   :java-new (fn [expr decls]
               (update expr :args instrument-exprs decls))
   :java-call instrument-java-call-expr
   :java-static-call (fn [expr decls]
                       (update expr :args instrument-exprs decls))
   :java-get-field instrument-record-get-expr
   :java-set-field instrument-java-set-field-expr
   :java-static-get-field (fn [expr _decls] expr)
   :java-static-set-field (fn [expr decls]
                            (update expr :expr instrument-expr decls))})

(defn- recursively-instrument-expr
  [expr decls]
  (if-let [instrumenter (get expr-instrumenters (:op expr))]
    (instrumenter expr decls)
    expr))

(defn- instrument-value-invariants
  [expr decls]
  (if-let [decl (get decls (:type expr))]
    (case [(:op expr) (:op decl)]
      [:construct :data] (instrument-data-invariants expr decl)
      [:variant :union] (instrument-union-invariants expr decl)
      expr)
    expr))

(defn- instrument-expr
  [expr decls]
  (if-not (map? expr)
    expr
    (-> expr
        (recursively-instrument-expr decls)
        (instrument-value-invariants decls))))

(defn- instrument-contracts
  [module]
  (let [decls-by-name (into {} (map (juxt :name identity) (:decls module)))]
    (update module
            :decls
            (fn [decls]
              (mapv (fn [decl]
                      (if (= :fn (:op decl))
                        (assoc decl
                               :body (-> decl
                                         with-postcondition-checks
                                         (with-contract-checks (:requires decl)
                                                               "Precondition"
                                                               (:name decl))
                                         (instrument-expr decls-by-name)))
                        decl))
                    decls)))))

(defn compile-module
  ([module]
   (compile-module module {}))
  ([module options]
   (let [checked (-> module
                     normalizer/normalize-module
                     (linker/link-module options)
                     resolver/resolve-module
                     effect-checker/check-module
                     type-checker/check-module
                     java-resolver/check-module
                     exhaustiveness-checker/check-module)
        lowered (jvm-lowerer/lower-module (instrument-contracts checked))]
     (jvm-emitter/emit-class-bytes lowered))))

(defn checked-module
  ([module]
   (checked-module module {}))
  ([module options]
   (-> module
       normalizer/normalize-module
       (linker/link-module options)
       resolver/resolve-module
       effect-checker/check-module
       type-checker/check-module
       java-resolver/check-module
       exhaustiveness-checker/check-module)))

(defn compile-source
  ([source]
   (compile-source source {}))
  ([source options]
   (-> source
       parser/parse-module
       (compile-module options))))

(defn compile-project-source
  [module-sources root-module-name]
  (let [sources (project-sources/reachable-source-map module-sources root-module-name)]
    (reduce (fn [bundle [module-name source]]
              (merge bundle
                     (compile-source source
                                     (project-sources/compiler-options sources module-name))))
            {}
            sources)))

(defn checked-project-module
  [module-sources root-module-name]
  (checked-module (parser/parse-module (project-sources/root-source module-sources root-module-name))
                  (project-sources/compiler-options module-sources root-module-name)))

(defn compile-project-dir
  [project-dir root-module-name]
  (compile-project-source (project-files/reachable-source-map project-dir root-module-name)
                          root-module-name))

(defn checked-project-dir-module
  [project-dir root-module-name]
  (checked-module (parser/parse-module (project-files/root-source project-dir root-module-name))
                  (project-files/compiler-options project-dir root-module-name)))

(defn build-project-source!
  [module-sources root-module-name output-dir]
  (-> (compile-project-source module-sources root-module-name)
      (write-bundle! output-dir)))

(defn build-project-dir!
  [project-dir root-module-name output-dir]
  (-> (compile-project-dir project-dir root-module-name)
      (write-bundle! output-dir)))

(defn interface-source
  [source]
  (-> source
      parser/parse-module
      normalizer/normalize-module
      module-interface/extract-interface))

(defn- class-file
  [output-dir internal-name]
  (io/file output-dir (str internal-name ".class")))

(defn write-bundle!
  [bundle output-dir]
  (doseq [[internal-name bytecode] bundle]
    (let [file (class-file output-dir internal-name)]
      (io/make-parents file)
      (with-open [out (io/output-stream file)]
        (.write out bytecode))))
  (into {}
        (map (fn [[internal-name _]]
               [internal-name (.getPath (class-file output-dir internal-name))]))
        bundle))

(defn build-source!
  ([source output-dir]
   (build-source! source output-dir {}))
  ([source output-dir options]
   (-> source
       (compile-source options)
       (write-bundle! output-dir))))

(defn- load-class-from-dir
  [output-dir class-name]
  (let [url (.toURL (.toURI (io/file output-dir)))
        loader (URLClassLoader. (into-array URL [url]))]
    (.loadClass loader class-name)))

(defn- class-name
  [module]
  (-> (:name module) str (.replace \/ \.)))

(defn- airj-main-method
  [klass]
  (or (try
        (.getMethod klass "airj_main" (into-array Class [(class (into-array String []))]))
        (catch NoSuchMethodException _ nil))
      (try
        (.getMethod klass "airj_main" (into-array Class []))
        (catch NoSuchMethodException _ nil))
      (throw (ex-info "Missing AIR-J main entrypoint."
                      {:class (.getName klass)}))))

(defn run-source!
  ([source]
   (run-source! source [] {}))
  ([source args]
   (run-source! source args {}))
  ([source args options]
   (let [module (parser/parse-module source)
         output-dir (.toString (java.nio.file.Files/createTempDirectory
                                "airj-run"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
         _ (-> module
               (compile-module options)
               (write-bundle! output-dir))
         klass (load-class-from-dir output-dir (class-name module))
         method (airj-main-method klass)]
     (if (= 1 (alength (.getParameterTypes method)))
       (.invoke method nil (object-array [(into-array String args)]))
       (.invoke method nil (object-array []))))))

(defn run-project-source!
  ([module-sources root-module-name]
   (run-project-source! module-sources root-module-name []))
  ([module-sources root-module-name args]
   (let [output-dir (.toString (java.nio.file.Files/createTempDirectory
                                "airj-run-project"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
         root-source (project-sources/root-source module-sources root-module-name)
         module (parser/parse-module root-source)
         _ (-> (compile-project-source module-sources root-module-name)
               (write-bundle! output-dir))
         klass (load-class-from-dir output-dir (class-name module))
         method (airj-main-method klass)]
     (if (= 1 (alength (.getParameterTypes method)))
       (.invoke method nil (object-array [(into-array String args)]))
       (.invoke method nil (object-array []))))))

(defn run-project-dir!
  ([project-dir root-module-name]
   (run-project-dir! project-dir root-module-name []))
  ([project-dir root-module-name args]
   (let [output-dir (.toString (java.nio.file.Files/createTempDirectory
                                "airj-run-project-dir"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
         root-source (project-files/root-source project-dir root-module-name)
         module (parser/parse-module root-source)
         _ (-> (compile-project-dir project-dir root-module-name)
               (write-bundle! output-dir))
         klass (load-class-from-dir output-dir (class-name module))
         method (airj-main-method klass)]
     (if (= 1 (alength (.getParameterTypes method)))
       (.invoke method nil (object-array [(into-array String args)]))
       (.invoke method nil (object-array []))))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T09:27:00.5695-05:00", :module-hash "2056829409", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 16, :hash "-1820704263"} {:id "form/1/declare", :kind "declare", :line 18, :end-line 18, :hash "2058848851"} {:id "def/contract-result-name", :kind "def", :line 20, :end-line 20, :hash "-105850544"} {:id "def/contract-unit-name", :kind "def", :line 21, :end-line 21, :hash "347759796"} {:id "def/invariant-self-name", :kind "def", :line 22, :end-line 22, :hash "-1593038412"} {:id "defn-/contract-failure-expr", :kind "defn-", :line 24, :end-line 30, :hash "101978921"} {:id "defn-/contract-check-expr", :kind "defn-", :line 32, :end-line 37, :hash "995434044"} {:id "defn-/with-contract-checks", :kind "defn-", :line 39, :end-line 45, :hash "1467639836"} {:id "defn-/with-postcondition-checks", :kind "defn-", :line 47, :end-line 73, :hash "-1464066269"} {:id "defn-/local-expr", :kind "defn-", :line 75, :end-line 78, :hash "1731534324"} {:id "defn-/instrument-data-invariants", :kind "defn-", :line 80, :end-line 103, :hash "570384672"} {:id "defn-/instrument-union-invariants", :kind "defn-", :line 105, :end-line 125, :hash "1766008045"} {:id "form/12/declare", :kind "declare", :line 127, :end-line 127, :hash "-1070001430"} {:id "defn-/instrument-exprs", :kind "defn-", :line 129, :end-line 131, :hash "1000382524"} {:id "defn-/instrument-bindings", :kind "defn-", :line 133, :end-line 137, :hash "850191118"} {:id "defn-/instrument-catches", :kind "defn-", :line 139, :end-line 143, :hash "-786001501"} {:id "defn-/instrument-match-cases", :kind "defn-", :line 145, :end-line 149, :hash "-1115947730"} {:id "defn-/instrument-try-expr", :kind "defn-", :line 151, :end-line 156, :hash "1049160216"} {:id "defn-/instrument-java-call-expr", :kind "defn-", :line 158, :end-line 162, :hash "1131125098"} {:id "defn-/instrument-java-set-field-expr", :kind "defn-", :line 164, :end-line 168, :hash "1627552813"} {:id "defn-/instrument-call-expr", :kind "defn-", :line 170, :end-line 174, :hash "-796600826"} {:id "defn-/instrument-construct-expr", :kind "defn-", :line 176, :end-line 178, :hash "-1298094645"} {:id "defn-/instrument-record-get-expr", :kind "defn-", :line 180, :end-line 182, :hash "1341046921"} {:id "defn-/instrument-if-expr", :kind "defn-", :line 184, :end-line 189, :hash "-1581702021"} {:id "defn-/instrument-match-expr", :kind "defn-", :line 191, :end-line 195, :hash "384899293"} {:id "defn-/instrument-let-expr", :kind "defn-", :line 197, :end-line 201, :hash "-1926311263"} {:id "defn-/instrument-lambda-expr", :kind "defn-", :line 203, :end-line 205, :hash "-1477337255"} {:id "defn-/instrument-loop-expr", :kind "defn-", :line 207, :end-line 211, :hash "494953759"} {:id "def/expr-instrumenters", :kind "def", :line 213, :end-line 257, :hash "-237241402"} {:id "defn-/recursively-instrument-expr", :kind "defn-", :line 259, :end-line 263, :hash "1761646999"} {:id "defn-/instrument-value-invariants", :kind "defn-", :line 265, :end-line 272, :hash "-803348989"} {:id "defn-/instrument-expr", :kind "defn-", :line 274, :end-line 280, :hash "-1857520944"} {:id "defn-/instrument-contracts", :kind "defn-", :line 282, :end-line 298, :hash "939158315"} {:id "defn/compile-module", :kind "defn", :line 300, :end-line 313, :hash "-303758079"} {:id "defn/checked-module", :kind "defn", :line 315, :end-line 326, :hash "-1523096844"} {:id "defn/compile-source", :kind "defn", :line 328, :end-line 334, :hash "-1796715147"} {:id "defn/compile-project-source", :kind "defn", :line 336, :end-line 344, :hash "-1920772816"} {:id "defn/checked-project-module", :kind "defn", :line 346, :end-line 349, :hash "1201666930"} {:id "defn/compile-project-dir", :kind "defn", :line 351, :end-line 354, :hash "-932599350"} {:id "defn/checked-project-dir-module", :kind "defn", :line 356, :end-line 359, :hash "213051726"} {:id "defn/build-project-source!", :kind "defn", :line 361, :end-line 364, :hash "434622430"} {:id "defn/build-project-dir!", :kind "defn", :line 366, :end-line 369, :hash "919358575"} {:id "defn/interface-source", :kind "defn", :line 371, :end-line 376, :hash "-277567554"} {:id "defn-/class-file", :kind "defn-", :line 378, :end-line 380, :hash "1900463827"} {:id "defn/write-bundle!", :kind "defn", :line 382, :end-line 392, :hash "-785230489"} {:id "defn/build-source!", :kind "defn", :line 394, :end-line 400, :hash "358574914"} {:id "defn-/load-class-from-dir", :kind "defn-", :line 402, :end-line 406, :hash "648982521"} {:id "defn-/class-name", :kind "defn-", :line 408, :end-line 410, :hash "-314160674"} {:id "defn-/airj-main-method", :kind "defn-", :line 412, :end-line 421, :hash "-130380503"} {:id "defn/run-source!", :kind "defn", :line 423, :end-line 440, :hash "-150944206"} {:id "defn/run-project-source!", :kind "defn", :line 442, :end-line 457, :hash "484953388"} {:id "defn/run-project-dir!", :kind "defn", :line 459, :end-line 474, :hash "-487228803"}]}
;; clj-mutate-manifest-end
