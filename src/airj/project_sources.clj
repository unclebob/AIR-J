(ns airj.project-sources
  (:require [airj.interface-sources :as interface-sources]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]
            [airj.project-graph :as project-graph]))

(defn- parsed-module-name
  [source]
  (:name (normalizer/normalize-module (parser/parse-module source))))

(defn source-map
  [module-sources]
  (into {}
        (map (fn [[module-name source]]
               [(or module-name (parsed-module-name source)) source]))
        module-sources))

(defn root-source
  [module-sources root-module-name]
  (let [sources (source-map module-sources)]
    (or (get sources root-module-name)
        (throw (ex-info "Missing project root module source."
                        {:module root-module-name})))))

(defn reachable-source-map
  [module-sources root-module-name]
  (project-graph/reachable-source-map (source-map module-sources) root-module-name))

(defn imported-interfaces
  [module-sources root-module-name]
  (let [sources (reachable-source-map module-sources root-module-name)]
    (-> sources
        (dissoc root-module-name)
        interface-sources/sources->interfaces)))

(defn compiler-options
  [module-sources root-module-name]
  {:interfaces (imported-interfaces module-sources root-module-name)})

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T15:34:34.231803-05:00", :module-hash "-1398698871", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "564087341"} {:id "defn-/parsed-module-name", :kind "defn-", :line 7, :end-line 9, :hash "-860700766"} {:id "defn/source-map", :kind "defn", :line 11, :end-line 16, :hash "-1125280073"} {:id "defn/root-source", :kind "defn", :line 18, :end-line 23, :hash "586201105"} {:id "defn/reachable-source-map", :kind "defn", :line 25, :end-line 27, :hash "-550175061"} {:id "defn/imported-interfaces", :kind "defn", :line 29, :end-line 34, :hash "551457564"} {:id "defn/compiler-options", :kind "defn", :line 36, :end-line 38, :hash "-569601317"}]}
;; clj-mutate-manifest-end
