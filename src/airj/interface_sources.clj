(ns airj.interface-sources
  (:require [airj.module-interface :as module-interface]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]))

(defn- source->interface
  [source]
  (-> source
      parser/parse-module
      normalizer/normalize-module
      module-interface/extract-interface))

(defn sources->interfaces
  [source-map]
  (into {}
        (map (fn [[module-name source]]
               [module-name (source->interface source)]))
        source-map))
