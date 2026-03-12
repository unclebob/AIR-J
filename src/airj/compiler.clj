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
        lowered (jvm-lowerer/lower-module checked)]
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
;; {:version 1, :tested-at "2026-03-12T14:23:56.131816-05:00", :module-hash "375696674", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 16, :hash "-1820704263"} {:id "form/1/declare", :kind "declare", :line 18, :end-line 18, :hash "2058848851"} {:id "defn/compile-module", :kind "defn", :line 20, :end-line 33, :hash "-429181673"} {:id "defn/checked-module", :kind "defn", :line 35, :end-line 46, :hash "-1523096844"} {:id "defn/compile-source", :kind "defn", :line 48, :end-line 54, :hash "-1796715147"} {:id "defn/compile-project-source", :kind "defn", :line 56, :end-line 64, :hash "22770127"} {:id "defn/checked-project-module", :kind "defn", :line 66, :end-line 69, :hash "1201666930"} {:id "defn/compile-project-dir", :kind "defn", :line 71, :end-line 73, :hash "1231329913"} {:id "defn/checked-project-dir-module", :kind "defn", :line 75, :end-line 78, :hash "213051726"} {:id "defn/build-project-source!", :kind "defn", :line 80, :end-line 83, :hash "434622430"} {:id "defn/build-project-dir!", :kind "defn", :line 85, :end-line 88, :hash "919358575"} {:id "defn/interface-source", :kind "defn", :line 90, :end-line 95, :hash "-277567554"} {:id "defn-/class-file", :kind "defn-", :line 97, :end-line 99, :hash "1900463827"} {:id "defn/write-bundle!", :kind "defn", :line 101, :end-line 111, :hash "-785230489"} {:id "defn/build-source!", :kind "defn", :line 113, :end-line 119, :hash "358574914"} {:id "defn-/load-class-from-dir", :kind "defn-", :line 121, :end-line 125, :hash "648982521"} {:id "defn-/class-name", :kind "defn-", :line 127, :end-line 129, :hash "-314160674"} {:id "defn-/airj-main-method", :kind "defn-", :line 131, :end-line 140, :hash "-130380503"} {:id "defn/run-source!", :kind "defn", :line 142, :end-line 159, :hash "-150944206"} {:id "defn/run-project-source!", :kind "defn", :line 161, :end-line 176, :hash "484953388"} {:id "defn/run-project-dir!", :kind "defn", :line 178, :end-line 193, :hash "-487228803"}]}
;; clj-mutate-manifest-end
