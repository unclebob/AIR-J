(ns airj.host-runtime
  (:require [clojure.java.io :as io]))

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- nested-class
  [^Class root suffix]
  (.loadClass (.getClassLoader root)
              (str (.getName root) "$" suffix)))

(defn- instantiate
  [^Class root suffix parameter-types values]
  (let [klass (if (= "" suffix) root (nested-class root suffix))
        ctor (.getConstructor klass (into-array Class parameter-types))]
    (.newInstance ctor (object-array values))))

(defn- option-none
  [^Class option-root]
  (instantiate option-root "None" [] []))

(defn- option-some
  [^Class option-root value]
  (instantiate option-root "Some" [Object] [value]))

(defn env-get
  [name ^Class option-root]
  (if-some [value (System/getenv ^String name)]
    (option-some option-root value)
    (option-none option-root)))

(defn- read-all-bytes
  [stream]
  (with-open [in stream
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn- await-bytes
  [stream]
  @(future (read-all-bytes stream)))

(defn- process-builder
  [command]
  (ProcessBuilder. ^java.util.List command))

(defn process-run
  [command stdin-bytes ^Class process-result-root]
  (try
    (let [builder (process-builder command)
          process (.start builder)
          _ (with-open [out (.getOutputStream process)]
              (.write out ^bytes stdin-bytes))
          stdout (await-bytes (.getInputStream process))
          stderr (await-bytes (.getErrorStream process))
          exit-code (.waitFor process)]
      (instantiate process-result-root
                   ""
                   [Integer/TYPE (Class/forName "[B") (Class/forName "[B")]
                   [exit-code stdout stderr]))
    (catch RuntimeException ex
      (throw ex))
    (catch Exception ex
      (throw (ex-info "Process execution failed."
                      {:command (vec command)}
                      ex)))))

(defn cwd
  []
  (or (System/getProperty "user.dir")
      (fail! "Working directory unavailable." {})))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T13:31:44.258303-05:00", :module-hash "212494878", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-502856135"} {:id "defn-/fail!", :kind "defn-", :line 4, :end-line 6, :hash "879938479"} {:id "defn-/nested-class", :kind "defn-", :line 8, :end-line 11, :hash "1735185178"} {:id "defn-/instantiate", :kind "defn-", :line 13, :end-line 17, :hash "-248836681"} {:id "defn-/option-none", :kind "defn-", :line 19, :end-line 21, :hash "-108896378"} {:id "defn-/option-some", :kind "defn-", :line 23, :end-line 25, :hash "353731076"} {:id "defn/env-get", :kind "defn", :line 27, :end-line 31, :hash "1771397497"} {:id "defn-/read-all-bytes", :kind "defn-", :line 33, :end-line 38, :hash "894071159"} {:id "defn-/await-bytes", :kind "defn-", :line 40, :end-line 42, :hash "506605214"} {:id "defn-/process-builder", :kind "defn-", :line 44, :end-line 46, :hash "-2053909289"} {:id "defn/process-run", :kind "defn", :line 48, :end-line 67, :hash "-990801009"} {:id "defn/cwd", :kind "defn", :line 69, :end-line 72, :hash "1149366332"}]}
;; clj-mutate-manifest-end
