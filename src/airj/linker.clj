(ns airj.linker
  (:require [airj.interface-sources :as interface-sources]))

(defn- resolved-interfaces
  [{:keys [interface-sources interfaces]}]
  (merge (when interface-sources
           (interface-sources/sources->interfaces interface-sources))
         interfaces))

(defn link-module
  ([module]
   (link-module module {}))
  ([module options]
   (let [interfaces (resolved-interfaces options)]
   (cond-> module
     interfaces
     (assoc :interfaces interfaces)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T13:53:13.883425-05:00", :module-hash "-660075879", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-1017775687"} {:id "defn-/resolved-interfaces", :kind "defn-", :line 4, :end-line 8, :hash "-697885719"} {:id "defn/link-module", :kind "defn", :line 10, :end-line 17, :hash "-1590671615"}]}
;; clj-mutate-manifest-end
