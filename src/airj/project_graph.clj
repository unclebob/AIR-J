(ns airj.project-graph
  (:require [airj.normalizer :as normalizer]
            [airj.parser :as parser]))

(defn direct-airj-imports
  [source]
  (->> source
       parser/parse-module
       normalizer/normalize-module
       :imports
       (filter #(= :airj-import (:op %)))
       (map :module)))

(defn- module-source
  [sources module-name]
  (or (get sources module-name)
      (throw (ex-info "Missing project module source."
                      {:module module-name}))))

(defn- fail-on-cycle!
  [module-name path]
  (let [cycle-start (.indexOf path module-name)
        cycle-path (conj (subvec path cycle-start) module-name)]
    (throw (ex-info "Project import cycle."
                    {:cycle cycle-path}))))

(defn- visit-module
  [sources module-name visited path reachable]
  (when (some #{module-name} path)
    (fail-on-cycle! module-name path))
  (if (visited module-name)
    [visited reachable]
    (let [source (module-source sources module-name)
          next-path (conj path module-name)
          imports (direct-airj-imports source)
          next-visited (conj visited module-name)
          next-reachable (assoc reachable module-name source)]
      (reduce (fn [[current-visited current-reachable] imported-module]
                (visit-module sources
                              imported-module
                              current-visited
                              next-path
                              current-reachable))
              [next-visited next-reachable]
              imports))))

(defn reachable-source-map
  [sources root-module-name]
  (second (visit-module sources root-module-name #{} [] {})))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T15:34:01.268527-05:00", :module-hash "-402421941", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-1184469576"} {:id "defn/direct-airj-imports", :kind "defn", :line 5, :end-line 12, :hash "1755463769"} {:id "defn-/module-source", :kind "defn-", :line 14, :end-line 18, :hash "476968309"} {:id "defn-/fail-on-cycle!", :kind "defn-", :line 20, :end-line 25, :hash "-134953179"} {:id "defn-/visit-module", :kind "defn-", :line 27, :end-line 45, :hash "-2024500094"} {:id "defn/reachable-source-map", :kind "defn", :line 47, :end-line 49, :hash "-894799540"}]}
;; clj-mutate-manifest-end
