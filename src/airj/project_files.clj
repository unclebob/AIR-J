(ns airj.project-files
  (:require [airj.project-graph :as project-graph]
            [airj.project-sources :as project-sources]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- airj-file?
  [file]
  (and (.isFile file)
       (str/ends-with? (.getName file) ".airj")))

(defn source-map
  [project-dir]
  (->> (file-seq (io/file project-dir))
       (filter airj-file?)
       (map (fn [file]
              [nil (slurp file)]))
       project-sources/source-map))

(defn reachable-source-map
  [project-dir root-module-name]
  (project-graph/reachable-source-map (source-map project-dir) root-module-name))

(defn root-source
  [project-dir root-module-name]
  (project-sources/root-source (reachable-source-map project-dir root-module-name)
                               root-module-name))

(defn compiler-options
  [project-dir root-module-name]
  (project-sources/compiler-options (reachable-source-map project-dir root-module-name)
                                    root-module-name))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T15:34:34.205986-05:00", :module-hash "-307401598", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "1494359613"} {:id "defn-/airj-file?", :kind "defn-", :line 7, :end-line 10, :hash "995441606"} {:id "defn/source-map", :kind "defn", :line 12, :end-line 18, :hash "985501821"} {:id "defn/reachable-source-map", :kind "defn", :line 20, :end-line 22, :hash "1128531847"} {:id "defn/root-source", :kind "defn", :line 24, :end-line 27, :hash "-993581846"} {:id "defn/compiler-options", :kind "defn", :line 29, :end-line 32, :hash "-1127137074"}]}
;; clj-mutate-manifest-end
