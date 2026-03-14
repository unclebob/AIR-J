(ns airj.json-runtime-spec
  (:require [airj.compiler :as compiler]
            [airj.json-runtime :as sut]
            [clojure.data.json :as json]
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

(describe "json runtime"
  (let [bundle (delay (compiler/compile-source "(module example/json_bundle
                                                 (imports
                                                   (airj airj/core Interchange))
                                                 (export main)
                                                 (fn main
                                                   (params)
                                                   (returns Int)
                                                   (effects ())
                                                   (requires true)
                                                   (ensures true)
                                                   0))"))
        classes (delay (define-classes @bundle))
        root (delay (get @classes "airj/core$Interchange"))]
  (it "round-trips canonical interchange values through JSON text"
    (let [value (sut/parse "{\"planet\":\"earth\",\"count\":7,\"seen\":true}" @root)]
      (should= "{\"planet\":\"earth\",\"count\":7,\"seen\":true}"
               (sut/write value))))

  (it "rejects integers outside the AIR-J Int range"
    (let [root @root]
      (should-throw clojure.lang.ExceptionInfo
                    "JSON integer out of AIR-J Int range."
                    (sut/parse "{\"too-big\":2147483648}" root))))

  (it "round-trips null and double values"
    (should= "null"
             (sut/write (sut/parse "null" @root)))
    (should= "3.5"
             (sut/write (sut/parse "3.5" @root))))

  (it "round-trips arrays and nested maps"
    (should= "[1,true,\"x\"]"
             (sut/write (sut/parse "[1,true,\"x\"]" @root)))
    (should= "{\"items\":[1,2],\"meta\":{\"ok\":true}}"
             (sut/write (sut/parse "{\"items\":[1,2],\"meta\":{\"ok\":true}}" @root))))

  (it "accepts AIR-J Int boundary values"
    (should= "{\"min\":-2147483648,\"max\":2147483647}"
             (sut/write (sut/parse "{\"min\":-2147483648,\"max\":2147483647}" @root))))

  (it "converts scalar JSON values directly"
    (should= "null"
             (sut/write (#'sut/scalar-interchange @root nil)))
    (should= "true"
             (sut/write (#'sut/scalar-interchange @root true)))
    (should= "7"
             (sut/write (#'sut/scalar-interchange @root 7)))
    (should= "3.5"
             (sut/write (#'sut/scalar-interchange @root 3.5)))
    (should= "\"earth\""
             (sut/write (#'sut/scalar-interchange @root "earth"))))

  (it "returns nil for non-scalar scalar dispatch misses"
    (should= nil
             (#'sut/scalar-interchange @root [1 2])))

  (it "converts interchange variants directly to JSON values"
    (should= nil
             (sut/interchange->json (sut/parse "null" @root)))
    (should= true
             (sut/interchange->json (sut/parse "true" @root)))
    (should= 7
             (sut/interchange->json (sut/parse "7" @root)))
    (should= 3.5
             (sut/interchange->json (sut/parse "3.5" @root)))
    (should= "earth"
             (sut/interchange->json (sut/parse "\"earth\"" @root)))
    (should= [1 true]
             (sut/interchange->json (sut/parse "[1,true]" @root)))
    (should= {"planet" "earth"}
             (sut/interchange->json (sut/parse "{\"planet\":\"earth\"}" @root))))

  (it "converts sequence variants directly to JSON sequences"
    (should= [1 true "x"]
             (sut/interchange->json (sut/parse "[1,true,\"x\"]" @root))))

  (it "invokes instrumented private helpers through runtime var resolution"
    (let [seq-value->json (ns-resolve 'airj.json-runtime 'seq-value->json)
          unsupported-json-value! (ns-resolve 'airj.json-runtime 'unsupported-json-value!)]
      (should= [1 true "x"]
               (seq-value->json (sut/parse "[1,true,\"x\"]" @root)))
      (let [failure (try
                      (unsupported-json-value! (Object.))
                      nil
                      (catch clojure.lang.ExceptionInfo ex
                        ex))]
        (should-not= nil failure)
        (should= "Unsupported JSON value."
                 (ex-message failure)))))

  (it "rejects unsupported JSON runtime values"
    (with-redefs [json/read-str (fn [_] (Object.))]
      (let [failure (try
                      (sut/parse "{\"ignored\":true}" @root)
                      nil
                      (catch clojure.lang.ExceptionInfo ex
                        ex))]
        (should-not= nil failure)
        (should= "Unsupported JSON value."
                 (ex-message failure)))))

  (it "calls exported json runtime helpers directly"
    (should= [1 true]
             (sut/seq-value->json (sut/parse "[1,true]" @root)))
    (let [failure (try
                    (sut/unsupported-json-value! (Object.))
                    nil
                    (catch clojure.lang.ExceptionInfo ex
                      ex))]
      (should-not= nil failure)
      (should= "Unsupported JSON value."
               (ex-message failure))))

  (it "rejects unknown interchange variants on write"
    (let [unknown (reify Object
                    (toString [_] "unknown"))]
      (should-throw clojure.lang.ExceptionInfo
                    "Unsupported AIR-J interchange value."
                    (#'sut/interchange->json unknown))))

  (it "rejects unsupported runtime values on write"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported AIR-J interchange value."
                  (sut/write (Object.))))))
