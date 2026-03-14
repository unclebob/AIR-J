(ns airj.host-runtime-spec
  (:require [airj.compiler :as compiler]
            [airj.host-runtime :as sut]
            [speclj.core :refer :all]))

(defn- binary-name
  [internal-name]
  (.replace ^String internal-name \/ \.))

(defn- define-classes
  [bytecode-map]
  (let [loader (clojure.lang.DynamicClassLoader.)]
    (into {}
          (map (fn [[internal-name bytecode]]
                 [internal-name
                  (.defineClass loader (binary-name internal-name) bytecode nil)]))
          (sort-by key bytecode-map))))

(describe "host runtime"
  (let [bundle (delay (compiler/compile-source "(module example/host_bundle
                                                 (imports
                                                   (airj airj/core Option)
                                                   (airj airj/process ProcessResult))
                                                 (export main)
                                                 (fn main
                                                   (params)
                                                   (returns Int)
                                                   (effects ())
                                                   (requires true)
                                                   (ensures true)
                                                   0))"))
        classes (delay (define-classes @bundle))
        option-root (delay (get @classes "airj/core$Option"))
        process-root (delay (get @classes "airj/process$ProcessResult"))]
    (it "returns Some or None for environment lookup"
      (let [some-value (sut/env-get "PATH" @option-root)
            none-value (sut/env-get "__AIRJ_MISSING_ENV__" @option-root)]
        (should (.endsWith (.getName (class some-value)) "$Some"))
        (should (.endsWith (.getName (class none-value)) "$None"))))

    (it "executes subprocesses into canonical process results"
      (let [result (sut/process-run ["/bin/cat"]
                                    (.getBytes "{\"tool\":\"ok\"}" "UTF-8")
                                    @process-root)
            fields (vec (.getFields (class result)))
            exit-code (.get (first (filter #(= Integer/TYPE (.getType %)) fields)) result)
            byte-values (map #(.get % result)
                             (filter #(= (Class/forName "[B") (.getType %)) fields))]
        (should= 0 exit-code)
        (should= #{"{\"tool\":\"ok\"}" ""}
                 (set (map #(String. ^bytes % "UTF-8") byte-values)))))))
