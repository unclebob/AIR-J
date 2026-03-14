(ns airj.linker
  (:require [airj.interface-sources :as interface-sources]
            [airj.stdlib :as stdlib]))

(defn- resolved-interfaces
  [module {:keys [interface-sources interfaces]}]
  (merge (stdlib/interfaces-for-module module)
         (when interface-sources
           (interface-sources/sources->interfaces interface-sources))
         interfaces))

(defn link-module
  ([module]
   (link-module module {}))
  ([module {:keys [available-modules] :as options}]
   (let [interfaces (resolved-interfaces module options)]
     (cond-> module
       (seq interfaces)
       (assoc :interfaces interfaces)

       available-modules
       (assoc :available-modules available-modules)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T06:58:50.789797-05:00", :module-hash "-783213578", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "115419096"} {:id "defn-/resolved-interfaces", :kind "defn-", :line 5, :end-line 10, :hash "-2062751397"} {:id "defn/link-module", :kind "defn", :line 12, :end-line 22, :hash "-373008569"}]}
;; clj-mutate-manifest-end
