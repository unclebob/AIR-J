(ns airj.parser
  (:require [clojure.edn :as edn]))

(declare parse-expr)
(declare parse-pattern)

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :parse))))

(defn- parse-import
  [[tag target & symbols :as form]]
  (case tag
    airj {:op :airj-import
          :module target
          :symbols (vec symbols)}
    java (do
           (when (seq symbols)
             (fail! "Unexpected java import form."
                    {:form form}))
           {:op :java-import
            :class-name target})
    (fail! "Unsupported import."
           {:form form})))

(defn- expect-tag
  [expected form]
  (when-not (= expected (first form))
    (fail! "Unexpected form tag."
           {:expected expected
            :actual (first form)
            :form form})))

(defn- parse-imports
  [form]
  (expect-tag 'imports form)
  (mapv parse-import (rest form)))

(defn- parse-exports
  [form]
  (expect-tag 'export form)
  (vec (rest form)))

(defn- parse-host
  [forms]
  (if (and (seq forms)
           (= 'host (ffirst forms)))
    [{:class-name (second (first forms))} (rest forms)]
    [nil forms]))

(defn- parse-param
  [[name type-expr]]
  {:name name
   :type type-expr})

(defn- parse-field
  [[tag name type-expr]]
  (expect-tag 'field [tag])
  {:name name
   :type type-expr})

(defn- parse-effects
  [form]
  (expect-tag 'effects form)
  (let [entries (rest form)]
    (cond
      (and (= 1 (count entries))
           (sequential? (first entries))
           (empty? (first entries)))
      []

      (and (= 1 (count entries))
           (sequential? (first entries)))
      (vec (first entries))

      :else
      (vec entries))))

(defn- parse-contract-clause
  [tag form]
  (expect-tag tag form)
  (vec (rest form)))

(defn- parse-invariants
  [forms]
  (if (and (seq forms)
           (= 'invariants (ffirst forms)))
    [(mapv parse-expr (rest (first forms))) (rest forms)]
    [[] forms]))

(defn- parse-type-params
  [forms]
  (if (and (seq forms)
           (= 'type-params (ffirst forms)))
    [(vec (rest (first forms))) (rest forms)]
    [[] forms]))

(defn- parse-data-decl
  [[_ name & forms]]
  (let [[type-params forms] (parse-type-params forms)
        [invariants forms] (parse-invariants forms)]
    {:op :data
     :name name
     :type-params type-params
     :invariants invariants
     :fields (mapv parse-field forms)}))

(defn- parse-enum-decl
  [[_ name & variants]]
  {:op :enum
   :name name
   :variants (vec variants)})

(defn- parse-variant-decl
  [[tag name & forms]]
  (expect-tag 'variant [tag])
  {:name name
   :fields (mapv parse-field forms)})

(defn- parse-case
  [[tag pattern body]]
  (expect-tag 'case [tag])
  {:pattern (parse-pattern pattern)
   :body (parse-expr body)})

(defn- parse-call-expr
  [form]
  {:op :call
   :callee (parse-expr (second form))
   :args (mapv parse-expr (drop 2 form))})

(defn- parse-construct-expr
  [form]
  {:op :construct
   :type (second form)
   :args (mapv parse-expr (drop 2 form))})

(defn- parse-variant-expr
  [form]
  {:op :variant
   :type (second form)
   :name (nth form 2)
   :args (mapv parse-expr (drop 3 form))})

(defn- parse-record-get-expr
  [form]
  {:op :record-get
   :target (parse-expr (second form))
   :field (nth form 2)})

(defn- parse-if-expr
  [form]
  {:op :if
   :test (parse-expr (second form))
   :then (parse-expr (nth form 2))
   :else (parse-expr (nth form 3))})

(defn- parse-match-expr
  [form]
  {:op :match
   :target (parse-expr (second form))
   :cases (mapv parse-case (drop 2 form))})

(defn- parse-binding
  [[name expr]]
  {:name name
   :expr (parse-expr expr)})

(defn- parse-let-expr
  [form]
  {:op :let
   :bindings (mapv parse-binding (second form))
   :body (parse-expr (nth form 2))})

(defn- parse-seq-expr
  [form]
  {:op :seq
   :exprs (mapv parse-expr (rest form))})

(defn- parse-lambda-expr
  [[_ params-form returns-form effects-form body-form]]
  {:op :lambda
   :params (mapv parse-param (rest params-form))
   :return-type (second returns-form)
   :effects (parse-effects effects-form)
   :body (parse-expr body-form)})

(defn- parse-catch-clause
  [[tag type-expr name body]]
  (expect-tag 'catch [tag])
  {:type type-expr
   :name name
   :body (parse-expr body)})

(defn- parse-finally-clause
  [[tag expr]]
  (expect-tag 'finally [tag])
  (parse-expr expr))

(defn- parse-try-expr
  [form]
  (let [parts (drop 2 form)
        catches (filter #(and (seq? %) (= 'catch (first %))) parts)
        finally-form (first (filter #(and (seq? %) (= 'finally (first %))) parts))]
    {:op :try
     :body (parse-expr (second form))
     :catches (mapv parse-catch-clause catches)
     :finally (when finally-form
                (parse-finally-clause finally-form))}))

(defn- parse-var-expr
  [[_ name type-expr init]]
  {:op :var
   :name name
   :type type-expr
   :init (parse-expr init)})

(defn- parse-set-expr
  [[_ name expr]]
  {:op :set
   :name name
   :expr (parse-expr expr)})

(defn- parse-loop-expr
  [form]
  {:op :loop
   :bindings (mapv parse-binding (second form))
   :body (parse-expr (nth form 2))})

(defn- parse-recur-expr
  [form]
  {:op :recur
   :args (mapv parse-expr (rest form))})

(defn- parse-raise-expr
  [form]
  {:op :raise
   :expr (parse-expr (second form))})

(defn- parse-primitive-unary-expr
  [op form]
  {:op op
   :arg (parse-expr (second form))})

(defn- parse-primitive-binary-expr
  [op form]
  {:op op
   :args (mapv parse-expr (rest form))})

(defn- parse-io-read-line-expr
  [_form]
  {:op :io-read-line})

(defn- parse-io-print-expr
  [form]
  {:op :io-print
   :arg (parse-expr (second form))})

(defn- parse-io-println-expr
  [form]
  {:op :io-println
   :arg (parse-expr (second form))})

(defn- parse-string-split-on-expr
  [form]
  {:op :string-split-on
   :args (mapv parse-expr (rest form))})

(defn- parse-seq-get-expr
  [form]
  {:op :seq-get
   :args (mapv parse-expr (rest form))})

(defn- parse-map-empty-expr
  [[_ value-type]]
  {:op :map-empty
   :value-type value-type})

(defn- parse-string-substring-expr
  [form]
  {:op :string-substring
   :args (mapv parse-expr (rest form))})

(defn- parse-type-args
  [form]
  (expect-tag 'type-args form)
  (vec (rest form)))

(defn- parse-signature
  [[tag params return-type]]
  (expect-tag 'signature [tag])
  {:params (vec params)
   :return-type return-type})

(defn- parse-java-new-expr
  [form]
  (let [[_ class-name & more] form
        [type-args args] (if (and (seq more)
                                  (seq? (first more))
                                  (= 'type-args (ffirst more)))
                           [(parse-type-args (first more)) (rest more)]
                           [[] more])]
    {:op :java-new
     :class-name class-name
     :type-args type-args
     :args (mapv parse-expr args)}))

(defn- parse-java-call-expr
  [[_ target member-id signature-form & args]]
  {:op :java-call
   :target (parse-expr target)
   :member-id member-id
   :signature (parse-signature signature-form)
   :args (mapv parse-expr args)})

(defn- parse-java-static-call-expr
  [[_ class-name member-id signature-form & args]]
  {:op :java-static-call
   :class-name class-name
   :member-id member-id
   :signature (parse-signature signature-form)
   :args (mapv parse-expr args)})

(defn- parse-java-get-field-expr
  [[_ target field-name field-type]]
  {:op :java-get-field
   :target (parse-expr target)
   :field-name field-name
   :field-type field-type})

(defn- parse-java-static-get-field-expr
  [[_ class-name field-name field-type]]
  {:op :java-static-get-field
   :class-name class-name
   :field-name field-name
   :field-type field-type})

(defn- parse-java-set-field-expr
  [[_ target field-name field-type expr]]
  {:op :java-set-field
   :target (parse-expr target)
   :field-name field-name
   :field-type field-type
   :expr (parse-expr expr)})

(defn- parse-java-static-set-field-expr
  [[_ class-name field-name field-type expr]]
  {:op :java-static-set-field
   :class-name class-name
   :field-name field-name
   :field-type field-type
   :expr (parse-expr expr)})

(def ^:private expr-handlers
  {'call parse-call-expr
   'construct parse-construct-expr
   'variant parse-variant-expr
   'record-get parse-record-get-expr
   'if parse-if-expr
   'match parse-match-expr
   'let parse-let-expr
   'seq parse-seq-expr
   'lambda parse-lambda-expr
   'try parse-try-expr
   'var parse-var-expr
   'set parse-set-expr
   'loop parse-loop-expr
   'recur parse-recur-expr
   'raise parse-raise-expr
   'int-add (partial parse-primitive-binary-expr :int-add)
   'int-sub (partial parse-primitive-binary-expr :int-sub)
   'int-mul (partial parse-primitive-binary-expr :int-mul)
   'int-div (partial parse-primitive-binary-expr :int-div)
   'int-mod (partial parse-primitive-binary-expr :int-mod)
   'int-eq (partial parse-primitive-binary-expr :int-eq)
   'int-lt (partial parse-primitive-binary-expr :int-lt)
   'int-le (partial parse-primitive-binary-expr :int-le)
   'int-gt (partial parse-primitive-binary-expr :int-gt)
   'int-ge (partial parse-primitive-binary-expr :int-ge)
   'float-add (partial parse-primitive-binary-expr :float-add)
   'float-sub (partial parse-primitive-binary-expr :float-sub)
   'float-mul (partial parse-primitive-binary-expr :float-mul)
   'float-div (partial parse-primitive-binary-expr :float-div)
   'float-eq (partial parse-primitive-binary-expr :float-eq)
   'float-lt (partial parse-primitive-binary-expr :float-lt)
   'float-le (partial parse-primitive-binary-expr :float-le)
   'float-gt (partial parse-primitive-binary-expr :float-gt)
   'float-ge (partial parse-primitive-binary-expr :float-ge)
   'double-add (partial parse-primitive-binary-expr :double-add)
   'double-sub (partial parse-primitive-binary-expr :double-sub)
   'double-mul (partial parse-primitive-binary-expr :double-mul)
   'double-div (partial parse-primitive-binary-expr :double-div)
   'double-eq (partial parse-primitive-binary-expr :double-eq)
   'double-lt (partial parse-primitive-binary-expr :double-lt)
   'double-le (partial parse-primitive-binary-expr :double-le)
   'double-gt (partial parse-primitive-binary-expr :double-gt)
   'double-ge (partial parse-primitive-binary-expr :double-ge)
   'bool-eq (partial parse-primitive-binary-expr :bool-eq)
   'int-ne (partial parse-primitive-binary-expr :int-ne)
   'string-eq (partial parse-primitive-binary-expr :string-eq)
   'string-concat (partial parse-primitive-binary-expr :string-concat)
   'string-split-on parse-string-split-on-expr
   'string-char-at (partial parse-primitive-binary-expr :string-char-at)
   'string-substring parse-string-substring-expr
   'int->string (partial parse-primitive-unary-expr :int->string)
   'int->float (partial parse-primitive-unary-expr :int->float)
   'int->double (partial parse-primitive-unary-expr :int->double)
   'float->double (partial parse-primitive-unary-expr :float->double)
   'double->float (partial parse-primitive-unary-expr :double->float)
   'json-parse (partial parse-primitive-unary-expr :json-parse)
   'json-write (partial parse-primitive-unary-expr :json-write)
   'string->int (partial parse-primitive-unary-expr :string->int)
   'string-length (partial parse-primitive-unary-expr :string-length)
   'string-trim (partial parse-primitive-unary-expr :string-trim)
   'string-empty? (partial parse-primitive-unary-expr :string-empty?)
   'seq-empty? (partial parse-primitive-unary-expr :seq-empty?)
   'seq-length (partial parse-primitive-unary-expr :seq-length)
   'seq-first (partial parse-primitive-unary-expr :seq-first)
   'seq-rest (partial parse-primitive-unary-expr :seq-rest)
   'seq-concat (partial parse-primitive-binary-expr :seq-concat)
   'seq-get parse-seq-get-expr
   'map-empty parse-map-empty-expr
   'map-set (partial parse-primitive-binary-expr :map-set)
   'map-get (partial parse-primitive-binary-expr :map-get)
   'map-contains? (partial parse-primitive-binary-expr :map-contains?)
   'map-keys (partial parse-primitive-unary-expr :map-keys)
   'bool-not (partial parse-primitive-unary-expr :bool-not)
   'bool-and (partial parse-primitive-binary-expr :bool-and)
   'bool-or (partial parse-primitive-binary-expr :bool-or)
   'io/read-line parse-io-read-line-expr
   'io/print parse-io-print-expr
   'io/println parse-io-println-expr
   'java/new parse-java-new-expr
   'java/call parse-java-call-expr
   'java/static-call parse-java-static-call-expr
   'java/get-field parse-java-get-field-expr
   'java/set-field parse-java-set-field-expr
   'java/get-static-field parse-java-static-get-field-expr
   'java/set-static-field parse-java-static-set-field-expr
   'local (fn [form]
            {:op :local
             :name (second form)})})

(defn- literal-pattern?
  [form]
  (or (true? form)
      (false? form)
      (integer? form)
      (string? form)))

(defn- wildcard-pattern?
  [form]
  (= '_ form))

(defn- record-pattern-form?
  [form]
  (and (seq? form) (= 'record (first form))))

(defn- union-pattern-form?
  [form]
  (seq? form))

(defn- parse-literal-pattern
  [form]
  {:op :literal-pattern
   :literal form})

(defn- parse-wildcard-pattern
  [_form]
  {:op :wildcard-pattern})

(defn- parse-binder-pattern
  [form]
  {:op :binder-pattern
   :name form})

(defn- parse-record-pattern
  [form]
  {:op :record-pattern
   :type (second form)
   :fields (mapv (fn [[name pattern]]
                   {:name name
                    :pattern (parse-pattern pattern)})
                 (drop 2 form))})

(defn- parse-union-pattern
  [form]
  {:op :union-pattern
   :name (first form)
   :args (mapv parse-pattern (rest form))})

(def ^:private pattern-parsers
  [[literal-pattern? parse-literal-pattern]
   [wildcard-pattern? parse-wildcard-pattern]
   [symbol? parse-binder-pattern]
   [record-pattern-form? parse-record-pattern]
   [union-pattern-form? parse-union-pattern]])

(defn parse-pattern
  [form]
  (if-let [[_ parser] (some (fn [[pred parser]]
                              (when (pred form)
                                [pred parser]))
                            pattern-parsers)]
    (parser form)
    (fail! "Unsupported pattern."
           {:form form})))

(defn- parse-union-decl
  [[_ name & forms]]
  (let [[type-params forms] (parse-type-params forms)
        [invariants forms] (parse-invariants forms)]
    {:op :union
     :name name
     :type-params type-params
     :invariants invariants
     :variants (mapv parse-variant-decl forms)}))

(defn- parse-fn-decl
  [[tag name params-form returns-form effects-form requires-form ensures-form body-form]]
  (expect-tag 'fn [tag])
  {:op :fn
   :name name
   :params (mapv parse-param (rest params-form))
   :return-type (second returns-form)
   :effects (parse-effects effects-form)
   :requires (parse-contract-clause 'requires requires-form)
   :ensures (parse-contract-clause 'ensures ensures-form)
   :body (parse-expr body-form)})

(defn- parse-decl
  [form]
  (case (first form)
    data (parse-data-decl form)
    enum (parse-enum-decl form)
    union (parse-union-decl form)
    fn (parse-fn-decl form)
    (fail! "Unsupported declaration."
           {:form form})))

(defn parse-expr
  [form]
  (if (seq? form)
    (if-let [handler (get expr-handlers (first form))]
      (handler form)
      (fail! "Unsupported expression."
             {:form form}))
    form))

(defn parse-module
  [source]
  (let [[tag name & forms] (edn/read-string source)
        [host forms] (parse-host forms)
        [imports-form export-form & decl-forms] forms]
    (expect-tag 'module [tag])
    (cond-> {:name name
             :imports (parse-imports imports-form)
             :exports (parse-exports export-form)
             :decls (mapv parse-decl decl-forms)}
      host
      (assoc :host host))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T10:18:43.742134-05:00", :module-hash "-1640288186", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1367613317"} {:id "form/1/declare", :kind "declare", :line 4, :end-line 4, :hash "1128659220"} {:id "form/2/declare", :kind "declare", :line 5, :end-line 5, :hash "-1446383543"} {:id "defn-/fail!", :kind "defn-", :line 7, :end-line 9, :hash "444631387"} {:id "defn-/parse-import", :kind "defn-", :line 11, :end-line 24, :hash "-1464173778"} {:id "defn-/expect-tag", :kind "defn-", :line 26, :end-line 32, :hash "265017371"} {:id "defn-/parse-imports", :kind "defn-", :line 34, :end-line 37, :hash "2124099339"} {:id "defn-/parse-exports", :kind "defn-", :line 39, :end-line 42, :hash "987718804"} {:id "defn-/parse-host", :kind "defn-", :line 44, :end-line 49, :hash "552281819"} {:id "defn-/parse-param", :kind "defn-", :line 51, :end-line 54, :hash "949945736"} {:id "defn-/parse-field", :kind "defn-", :line 56, :end-line 60, :hash "1251715611"} {:id "defn-/parse-effects", :kind "defn-", :line 62, :end-line 77, :hash "2129032603"} {:id "defn-/parse-contract-clause", :kind "defn-", :line 79, :end-line 82, :hash "2065410627"} {:id "defn-/parse-invariants", :kind "defn-", :line 84, :end-line 89, :hash "192681247"} {:id "defn-/parse-type-params", :kind "defn-", :line 91, :end-line 96, :hash "-1233585086"} {:id "defn-/parse-data-decl", :kind "defn-", :line 98, :end-line 106, :hash "240075906"} {:id "defn-/parse-enum-decl", :kind "defn-", :line 108, :end-line 112, :hash "53471563"} {:id "defn-/parse-variant-decl", :kind "defn-", :line 114, :end-line 118, :hash "545649087"} {:id "defn-/parse-case", :kind "defn-", :line 120, :end-line 124, :hash "505569259"} {:id "defn-/parse-call-expr", :kind "defn-", :line 126, :end-line 130, :hash "1364196914"} {:id "defn-/parse-construct-expr", :kind "defn-", :line 132, :end-line 136, :hash "587366883"} {:id "defn-/parse-variant-expr", :kind "defn-", :line 138, :end-line 143, :hash "-385235348"} {:id "defn-/parse-record-get-expr", :kind "defn-", :line 145, :end-line 149, :hash "1628847814"} {:id "defn-/parse-if-expr", :kind "defn-", :line 151, :end-line 156, :hash "-1106654283"} {:id "defn-/parse-match-expr", :kind "defn-", :line 158, :end-line 162, :hash "1800281827"} {:id "defn-/parse-binding", :kind "defn-", :line 164, :end-line 167, :hash "-1959265528"} {:id "defn-/parse-let-expr", :kind "defn-", :line 169, :end-line 173, :hash "-647963169"} {:id "defn-/parse-seq-expr", :kind "defn-", :line 175, :end-line 178, :hash "302220611"} {:id "defn-/parse-lambda-expr", :kind "defn-", :line 180, :end-line 186, :hash "-1706998005"} {:id "defn-/parse-catch-clause", :kind "defn-", :line 188, :end-line 193, :hash "1820817090"} {:id "defn-/parse-finally-clause", :kind "defn-", :line 195, :end-line 198, :hash "-378492021"} {:id "defn-/parse-try-expr", :kind "defn-", :line 200, :end-line 209, :hash "716102796"} {:id "defn-/parse-var-expr", :kind "defn-", :line 211, :end-line 216, :hash "1325393821"} {:id "defn-/parse-set-expr", :kind "defn-", :line 218, :end-line 222, :hash "1429713983"} {:id "defn-/parse-loop-expr", :kind "defn-", :line 224, :end-line 228, :hash "-198693472"} {:id "defn-/parse-recur-expr", :kind "defn-", :line 230, :end-line 233, :hash "-1009172251"} {:id "defn-/parse-raise-expr", :kind "defn-", :line 235, :end-line 238, :hash "-1091784450"} {:id "defn-/parse-primitive-unary-expr", :kind "defn-", :line 240, :end-line 243, :hash "1155615128"} {:id "defn-/parse-primitive-binary-expr", :kind "defn-", :line 245, :end-line 248, :hash "-981498976"} {:id "defn-/parse-io-read-line-expr", :kind "defn-", :line 250, :end-line 252, :hash "1866185537"} {:id "defn-/parse-io-print-expr", :kind "defn-", :line 254, :end-line 257, :hash "2139111722"} {:id "defn-/parse-io-println-expr", :kind "defn-", :line 259, :end-line 262, :hash "-369802684"} {:id "defn-/parse-string-split-on-expr", :kind "defn-", :line 264, :end-line 267, :hash "-549703719"} {:id "defn-/parse-seq-get-expr", :kind "defn-", :line 269, :end-line 272, :hash "683923185"} {:id "defn-/parse-map-empty-expr", :kind "defn-", :line 274, :end-line 277, :hash "-922121204"} {:id "defn-/parse-string-substring-expr", :kind "defn-", :line 279, :end-line 282, :hash "-2136231073"} {:id "defn-/parse-type-args", :kind "defn-", :line 284, :end-line 287, :hash "-266637052"} {:id "defn-/parse-signature", :kind "defn-", :line 289, :end-line 293, :hash "804347329"} {:id "defn-/parse-java-new-expr", :kind "defn-", :line 295, :end-line 306, :hash "-1667842733"} {:id "defn-/parse-java-call-expr", :kind "defn-", :line 308, :end-line 314, :hash "-1542269505"} {:id "defn-/parse-java-static-call-expr", :kind "defn-", :line 316, :end-line 322, :hash "1075951034"} {:id "defn-/parse-java-get-field-expr", :kind "defn-", :line 324, :end-line 329, :hash "1853237465"} {:id "defn-/parse-java-static-get-field-expr", :kind "defn-", :line 331, :end-line 336, :hash "-1799498212"} {:id "defn-/parse-java-set-field-expr", :kind "defn-", :line 338, :end-line 344, :hash "-2021484420"} {:id "defn-/parse-java-static-set-field-expr", :kind "defn-", :line 346, :end-line 352, :hash "-1416091135"} {:id "def/expr-handlers", :kind "def", :line 354, :end-line 442, :hash "1721849879"} {:id "defn-/literal-pattern?", :kind "defn-", :line 444, :end-line 449, :hash "635525918"} {:id "defn-/wildcard-pattern?", :kind "defn-", :line 451, :end-line 453, :hash "76497133"} {:id "defn-/record-pattern-form?", :kind "defn-", :line 455, :end-line 457, :hash "-1394925114"} {:id "defn-/union-pattern-form?", :kind "defn-", :line 459, :end-line 461, :hash "765247313"} {:id "defn-/parse-literal-pattern", :kind "defn-", :line 463, :end-line 466, :hash "148576341"} {:id "defn-/parse-wildcard-pattern", :kind "defn-", :line 468, :end-line 470, :hash "688434532"} {:id "defn-/parse-binder-pattern", :kind "defn-", :line 472, :end-line 475, :hash "-1102720389"} {:id "defn-/parse-record-pattern", :kind "defn-", :line 477, :end-line 484, :hash "785832603"} {:id "defn-/parse-union-pattern", :kind "defn-", :line 486, :end-line 490, :hash "1434576212"} {:id "def/pattern-parsers", :kind "def", :line 492, :end-line 497, :hash "1242457034"} {:id "defn/parse-pattern", :kind "defn", :line 499, :end-line 507, :hash "-258633312"} {:id "defn-/parse-union-decl", :kind "defn-", :line 509, :end-line 517, :hash "-1120328440"} {:id "defn-/parse-fn-decl", :kind "defn-", :line 519, :end-line 529, :hash "-1738991000"} {:id "defn-/parse-decl", :kind "defn-", :line 531, :end-line 539, :hash "900166689"} {:id "defn/parse-expr", :kind "defn", :line 541, :end-line 548, :hash "-1140331564"} {:id "defn/parse-module", :kind "defn", :line 550, :end-line 561, :hash "147032411"}]}
;; clj-mutate-manifest-end
