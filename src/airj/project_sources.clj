(ns airj.project-sources
  (:require [airj.interface-sources :as interface-sources]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]
            [airj.project-graph :as project-graph]
            [airj.stdlib :as stdlib]))

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

(defn- stdlib-source-map
  [sources]
  (reduce (fn [acc source]
            (merge acc
                   (stdlib/reachable-source-map (parser/parse-module source))))
          {}
          (vals sources)))

(defn compilation-source-map
  [module-sources root-module-name]
  (let [sources (reachable-source-map module-sources root-module-name)]
    (merge (stdlib-source-map sources)
           sources)))

(defn imported-interfaces
  [module-sources root-module-name]
  (let [sources (compilation-source-map module-sources root-module-name)]
    (-> sources
        (dissoc root-module-name)
        interface-sources/sources->interfaces)))

(defn compiler-options
  [module-sources root-module-name]
  (let [sources (compilation-source-map module-sources root-module-name)]
    {:interfaces (-> sources
                     (dissoc root-module-name)
                     interface-sources/sources->interfaces)
     :available-modules (set (keys sources))}))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T06:59:18.941072-05:00", :module-hash "35640496", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "-1422131983"} {:id "defn-/parsed-module-name", :kind "defn-", :line 8, :end-line 10, :hash "-860700766"} {:id "defn/source-map", :kind "defn", :line 12, :end-line 17, :hash "-1125280073"} {:id "defn/root-source", :kind "defn", :line 19, :end-line 24, :hash "586201105"} {:id "defn/reachable-source-map", :kind "defn", :line 26, :end-line 28, :hash "-550175061"} {:id "defn-/stdlib-source-map", :kind "defn-", :line 30, :end-line 36, :hash "-602861673"} {:id "defn/compilation-source-map", :kind "defn", :line 38, :end-line 42, :hash "-360832942"} {:id "defn/imported-interfaces", :kind "defn", :line 44, :end-line 49, :hash "1480804294"} {:id "defn/compiler-options", :kind "defn", :line 51, :end-line 57, :hash "-83450567"}]}
;; clj-mutate-manifest-end
