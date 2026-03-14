(ns airj.stdlib
  (:require [airj.interface-sources :as interface-sources]
            [airj.parser :as parser]))

(declare reachable-source-map)

(def ^:private standard-sources
  {'airj/core
   "(module airj/core
      (imports
        (java java.lang.NumberFormatException))
      (export Diagnostic Interchange Option Result parse-int)
      (data Diagnostic
        (field phase String)
        (field message String)
        (field detail String))
      (union Interchange
        (variant Null)
        (variant BoolValue
          (field value Bool))
        (variant IntValue
          (field value Int))
        (variant DoubleValue
          (field value Double))
        (variant StringValue
          (field value String))
        (variant SeqValue
          (field value (Seq Interchange)))
        (variant MapValue
          (field value (Map String Interchange))))
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
          (field error Err)))
      (fn parse-int
        (params (text String))
        (returns (Result Int Diagnostic))
        (effects ())
        (requires true)
        (ensures true)
        (try
          (variant (Result Int Diagnostic)
                   Ok
                   (string->int (local text)))
          (catch (Java java.lang.NumberFormatException) ex
            (variant (Result Int Diagnostic)
                     Err
                     (construct Diagnostic
                                \"parse\"
                                \"Invalid integer.\"
                                (local text)))))))"

   'airj/json
   "(module airj/json
      (imports
        (airj airj/core Diagnostic Interchange Result)
        (java java.lang.RuntimeException))
      (export parse parse-result write write-result)
      (fn parse
        (params (text String))
        (returns Interchange)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (json-parse (local text)))
      (fn parse-result
        (params (text String))
        (returns (Result Interchange Diagnostic))
        (effects ())
        (requires true)
        (ensures true)
        (try
          (variant (Result Interchange Diagnostic)
                   Ok
                   (call (local parse) (local text)))
          (catch (Java java.lang.RuntimeException) ex
            (variant (Result Interchange Diagnostic)
                     Err
                     (construct Diagnostic
                                \"json\"
                                \"Parse failed.\"
                                (local text))))))
      (fn write
        (params (value Interchange))
        (returns String)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (json-write (local value)))
      (fn write-result
        (params (value Interchange))
        (returns (Result String Diagnostic))
        (effects ())
        (requires true)
        (ensures true)
        (try
          (variant (Result String Diagnostic)
                   Ok
                   (call (local write) (local value)))
          (catch (Java java.lang.RuntimeException) ex
            (variant (Result String Diagnostic)
                     Err
                     (construct Diagnostic
                                \"json\"
                                \"Write failed.\"
                                \"interchange\"))))))"

   'airj/file
   "(module airj/file
      (imports
        (airj airj/core Diagnostic Result)
        (java java.io.File)
        (java java.io.FileWriter)
        (java java.io.IOException)
        (java java.lang.String)
        (java java.nio.file.Files))
      (export exists?
              read-string
              read-string-result
              read-lines
              read-lines-result
              write-string
              write-string-result
              write-lines
              write-lines-result)
      (fn exists?
        (params (path String))
        (returns Bool)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (java/call
          (java/new java.io.File (local path))
          exists
          (signature () Bool)))
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
              (signature () Unit))))))
      (fn read-lines
        (params (path String))
        (returns (Seq String))
        (effects (File.Read Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.nio.file.Files
          readAllLines
          (signature ((Java java.nio.file.Path)) (Seq String))
          (java/call
            (java/new java.io.File (local path))
            toPath
            (signature () (Java java.nio.file.Path)))))
      (fn write-lines
        (params (path String) (lines (Seq String)))
        (returns Unit)
        (effects (File.Write Foreign.Throw))
        (requires true)
        (ensures true)
        (call (local write-string)
              (local path)
              (java/static-call
                java.lang.String
                join
                (signature ((Java java.lang.CharSequence) (Java java.lang.Iterable)) String)
                \"\n\"
                (local lines))))
      (fn read-string-result
        (params (path String))
        (returns (Result String Diagnostic))
        (effects (File.Read))
        (requires true)
        (ensures true)
        (try
          (variant (Result String Diagnostic)
                   Ok
                   (call (local read-string) (local path)))
          (catch (Java java.io.IOException) ex
            (variant (Result String Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Read failed.\"
                                (local path))))))
      (fn read-lines-result
        (params (path String))
        (returns (Result (Seq String) Diagnostic))
        (effects (File.Read))
        (requires true)
        (ensures true)
        (try
          (variant (Result (Seq String) Diagnostic)
                   Ok
                   (call (local read-lines) (local path)))
          (catch (Java java.io.IOException) ex
            (variant (Result (Seq String) Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Read lines failed.\"
                                (local path))))))
      (fn write-string-result
        (params (path String) (contents String))
        (returns (Result Bool Diagnostic))
        (effects (File.Write))
        (requires true)
        (ensures true)
        (try
          (variant (Result Bool Diagnostic)
                   Ok
                   (seq
                     (call (local write-string) (local path) (local contents))
                     true))
          (catch (Java java.io.IOException) ex
            (variant (Result Bool Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Write failed.\"
                                (local path))))))
      (fn write-lines-result
        (params (path String) (lines (Seq String)))
        (returns (Result Bool Diagnostic))
        (effects (File.Write))
        (requires true)
        (ensures true)
        (try
          (variant (Result Bool Diagnostic)
                   Ok
                   (seq
                     (call (local write-lines) (local path) (local lines))
                     true))
          (catch (Java java.io.IOException) ex
            (variant (Result Bool Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Write lines failed.\"
                                (local path)))))))"})

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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T10:19:46.646734-05:00", :module-hash "-1093319838", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "1849576384"} {:id "form/1/declare", :kind "declare", :line 5, :end-line 5, :hash "-1313016324"} {:id "def/standard-sources", :kind "def", :line 7, :end-line 275, :hash "-170213925"} {:id "defn/source-map", :kind "defn", :line 277, :end-line 279, :hash "981959532"} {:id "defn/interfaces", :kind "defn", :line 281, :end-line 283, :hash "801379587"} {:id "defn/interfaces-for-module", :kind "defn", :line 285, :end-line 287, :hash "-1218712190"} {:id "defn/stdlib-module?", :kind "defn", :line 289, :end-line 291, :hash "1879715354"} {:id "defn-/imported-stdlib-modules", :kind "defn-", :line 293, :end-line 299, :hash "667554956"} {:id "defn/reachable-source-map", :kind "defn", :line 301, :end-line 314, :hash "-1038087388"}]}
;; clj-mutate-manifest-end
