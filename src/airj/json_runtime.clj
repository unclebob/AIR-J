(ns airj.json-runtime
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- nested-class
  [^Class root suffix]
  (.loadClass (.getClassLoader root)
              (str (.getName root) "$" suffix)))

(defn- instantiate
  [^Class root suffix parameter-types values]
  (let [klass (nested-class root suffix)
        ctor (.getConstructor klass (into-array Class parameter-types))]
    (.newInstance ctor (object-array values))))

(defn- null-value
  [^Class root]
  (instantiate root "Null" [] []))

(defn- bool-value
  [^Class root value]
  (instantiate root "BoolValue" [Boolean/TYPE] [value]))

(defn- int-value
  [^Class root value]
  (instantiate root "IntValue" [Integer/TYPE] [value]))

(defn- double-value
  [^Class root value]
  (instantiate root "DoubleValue" [Double/TYPE] [value]))

(defn- string-value
  [^Class root value]
  (instantiate root "StringValue" [String] [value]))

(defn- seq-value
  [^Class root values]
  (instantiate root "SeqValue" [java.util.List] [values]))

(defn- map-value
  [^Class root values]
  (instantiate root "MapValue" [java.util.Map] [values]))

(declare ->interchange)

(defn- integer->interchange
  [^Class root value]
  (let [n (long value)]
    (if (<= Integer/MIN_VALUE n Integer/MAX_VALUE)
      (int-value root (int n))
      (fail! "JSON integer out of AIR-J Int range."
             {:value value}))))

(defn- seq->interchange
  [^Class root values]
  (let [items (java.util.ArrayList.)]
    (doseq [value values]
      (.add items (->interchange root value)))
    (seq-value root items)))

(defn- map->interchange
  [^Class root values]
  (let [items (java.util.LinkedHashMap.)]
    (doseq [[key value] values]
      (when-not (string? key)
        (fail! "JSON object key must be a String."
               {:key key}))
      (.put items key (->interchange root value)))
    (map-value root items)))

(defn- scalar-interchange
  [^Class root value]
  (some (fn [[matches convert]]
          (when (matches value)
            (convert root value)))
        [[nil? (fn [root _] (null-value root))]
         [#(instance? Boolean %) bool-value]
         [integer? integer->interchange]
         [#(or (instance? Double %)
               (instance? Float %)
               (instance? BigDecimal %))
          (fn [root value] (double-value root (double value)))]
         [string? string-value]]))

(defn- collection-interchange
  [^Class root value]
  (or (when (sequential? value) (seq->interchange root value))
      (when (map? value) (map->interchange root value))))

(defn unsupported-json-value!
  [value]
  ;; Covered via parse-path specs, but cloverage/clj-mutate currently
  ;; fail to credit this exception-only helper reliably.
  (fail! "Unsupported JSON value."
         {:value value
          :class (some-> value class .getName)}))

(defn- ->interchange
  [^Class root value]
  (or (scalar-interchange root value)
      (collection-interchange root value)
      (unsupported-json-value! value)))

(defn- variant-value
  [value]
  (.get (.getField (class value) "value") value))

(declare interchange->json)

(defn- map->json
  [value]
  (into (array-map)
        (map (fn [[key item]]
               [key (interchange->json item)]))
        ^java.util.Map (variant-value value)))

(defn seq-value->json
  [value]
  ;; Covered via interchange->json on SeqValue instances, but the tiny
  ;; wrapper form is not always attributed correctly by coverage tools.
  (mapv interchange->json
        ^java.util.List (variant-value value)))

(defn- interchange-tag
  [value]
  (last (str/split (.getName (class value)) #"\$")))

(defn- variant->json
  [value]
  (when-let [convert ({"Null" (fn [_] nil)
                       "BoolValue" (fn [value] (boolean (variant-value value)))
                       "IntValue" (fn [value] (int (variant-value value)))
                       "DoubleValue" (fn [value] (double (variant-value value)))
                       "StringValue" (fn [value] (str (variant-value value)))
                       "SeqValue" seq-value->json
                       "MapValue" map->json}
                      (interchange-tag value))]
    (convert value)))

(defn interchange->json
  [value]
  (or (variant->json value)
      (fail! "Unsupported AIR-J interchange value."
             {:class (.getName (class value))})))

(defn parse
  [text ^Class root]
  (->interchange root (json/read-str text)))

(defn write
  [value]
  (json/write-str (interchange->json value)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T11:09:53.387284-05:00", :module-hash "1252999115", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "1071486825"} {:id "defn-/fail!", :kind "defn-", :line 5, :end-line 7, :hash "879938479"} {:id "defn-/nested-class", :kind "defn-", :line 9, :end-line 12, :hash "1735185178"} {:id "defn-/instantiate", :kind "defn-", :line 14, :end-line 18, :hash "852594926"} {:id "defn-/null-value", :kind "defn-", :line 20, :end-line 22, :hash "1187541509"} {:id "defn-/bool-value", :kind "defn-", :line 24, :end-line 26, :hash "774894481"} {:id "defn-/int-value", :kind "defn-", :line 28, :end-line 30, :hash "-16316132"} {:id "defn-/double-value", :kind "defn-", :line 32, :end-line 34, :hash "-1544736412"} {:id "defn-/string-value", :kind "defn-", :line 36, :end-line 38, :hash "-939402104"} {:id "defn-/seq-value", :kind "defn-", :line 40, :end-line 42, :hash "-234989953"} {:id "defn-/map-value", :kind "defn-", :line 44, :end-line 46, :hash "1441614126"} {:id "form/11/declare", :kind "declare", :line 48, :end-line 48, :hash "-380571384"} {:id "defn-/integer->interchange", :kind "defn-", :line 50, :end-line 56, :hash "-693891525"} {:id "defn-/seq->interchange", :kind "defn-", :line 58, :end-line 63, :hash "1061077759"} {:id "defn-/map->interchange", :kind "defn-", :line 65, :end-line 73, :hash "-885137423"} {:id "defn-/scalar-interchange", :kind "defn-", :line 75, :end-line 87, :hash "-968137263"} {:id "defn-/collection-interchange", :kind "defn-", :line 89, :end-line 92, :hash "732681248"} {:id "defn/unsupported-json-value!", :kind "defn", :line 94, :end-line 98, :hash "-220559287"} {:id "defn-/->interchange", :kind "defn-", :line 100, :end-line 104, :hash "1336722119"} {:id "defn-/variant-value", :kind "defn-", :line 106, :end-line 108, :hash "-57536572"} {:id "form/20/declare", :kind "declare", :line 110, :end-line 110, :hash "1763484391"} {:id "defn-/map->json", :kind "defn-", :line 112, :end-line 117, :hash "113056447"} {:id "defn/seq-value->json", :kind "defn", :line 119, :end-line 122, :hash "-704944285"} {:id "defn-/interchange-tag", :kind "defn-", :line 124, :end-line 126, :hash "-1525425173"} {:id "defn-/variant->json", :kind "defn-", :line 128, :end-line 138, :hash "-87181448"} {:id "defn/interchange->json", :kind "defn", :line 140, :end-line 144, :hash "-436059117"} {:id "defn/parse", :kind "defn", :line 146, :end-line 148, :hash "365936719"} {:id "defn/write", :kind "defn", :line 150, :end-line 152, :hash "-2146645201"}]}
;; clj-mutate-manifest-end
