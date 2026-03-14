(ns airj.stdlib
  (:require [airj.interface-sources :as interface-sources]
            [airj.parser :as parser]))

(declare reachable-source-map)

(def ^:private standard-sources
  {'airj/core
   "(module airj/core
      (imports)
      (export Diagnostic Option Result)
      (data Diagnostic
        (field phase String)
        (field message String)
        (field detail String))
      (union Option
        (type-params T)
        (variant None)
        (variant Some
          (field value T)))
      (union Result
        (type-params Ok Err)
        (variant Ok
          (field value Ok))
        (variant Err
          (field error Err))))"

   'airj/file
   "(module airj/file
      (imports
        (java java.io.File)
        (java java.io.FileWriter)
        (java java.nio.file.Files))
      (export read-string write-string)
      (fn read-string
        (params (path String))
        (returns String)
        (effects (File.Read Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.nio.file.Files
          readString
          (signature ((Java java.nio.file.Path)) String)
          (java/call
            (java/new java.io.File (local path))
            toPath
            (signature () (Java java.nio.file.Path)))))
      (fn write-string
        (params (path String) (contents String))
        (returns Unit)
        (effects (File.Write Foreign.Throw))
        (requires true)
        (ensures true)
        (let ((writer
                (java/new java.io.FileWriter (local path))))
          (try
            (java/call
              (local writer)
              write
              (signature (String) Unit)
              (local contents))
            (finally
              (java/call
                (local writer)
                close
                (signature () Unit)))))))"})

(defn source-map
  []
  standard-sources)

(defn interfaces
  []
  (interface-sources/sources->interfaces standard-sources))

(defn interfaces-for-module
  [module]
  (interface-sources/sources->interfaces (reachable-source-map module)))

(defn stdlib-module?
  [module-name]
  (contains? standard-sources module-name))

(defn- imported-stdlib-modules
  [module]
  (->> (:imports module)
       (filter #(= :airj-import (:op %)))
       (map :module)
       (filter stdlib-module?)
       distinct))

(defn reachable-source-map
  [root-module]
  (loop [pending (vec (imported-stdlib-modules root-module))
         seen {}
         visited #{}]
    (if-let [module-name (first pending)]
      (if (visited module-name)
        (recur (subvec pending 1) seen visited)
        (let [source (get standard-sources module-name)
              module (parser/parse-module source)]
          (recur (into (subvec pending 1) (imported-stdlib-modules module))
                 (assoc seen module-name source)
                 (conj visited module-name))))
      seen)))
