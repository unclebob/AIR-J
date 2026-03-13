(ns airj.parser
  (:require [clojure.edn :as edn]))

(declare parse-expr)
(declare parse-pattern)

(defn- parse-import
  [[tag target & symbols :as form]]
  (case tag
    airj {:op :airj-import
          :module target
          :symbols (vec symbols)}
    java (do
           (when (seq symbols)
             (throw (ex-info "Unexpected java import form."
                             {:form form})))
           {:op :java-import
            :class-name target})
    (throw (ex-info "Unsupported import."
                    {:form form}))))

(defn- expect-tag
  [expected form]
  (when-not (= expected (first form))
    (throw (ex-info "Unexpected form tag."
                    {:expected expected
                     :actual (first form)
                     :form form}))))

(defn- parse-imports
  [form]
  (expect-tag 'imports form)
  (mapv parse-import (rest form)))

(defn- parse-exports
  [form]
  (expect-tag 'export form)
  (vec (rest form)))

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
   'bool-eq (partial parse-primitive-binary-expr :bool-eq)
   'int-ne (partial parse-primitive-binary-expr :int-ne)
   'string-eq (partial parse-primitive-binary-expr :string-eq)
   'string-concat (partial parse-primitive-binary-expr :string-concat)
   'string-split-on parse-string-split-on-expr
   'int->string (partial parse-primitive-unary-expr :int->string)
   'string->int (partial parse-primitive-unary-expr :string->int)
   'string-length (partial parse-primitive-unary-expr :string-length)
   'string-trim (partial parse-primitive-unary-expr :string-trim)
   'string-empty? (partial parse-primitive-unary-expr :string-empty?)
   'seq-length (partial parse-primitive-unary-expr :seq-length)
   'seq-get parse-seq-get-expr
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
    (throw (ex-info "Unsupported pattern."
                    {:form form}))))

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
    (throw (ex-info "Unsupported declaration."
                    {:form form}))))

(defn parse-expr
  [form]
  (if (seq? form)
    (if-let [handler (get expr-handlers (first form))]
      (handler form)
      (throw (ex-info "Unsupported expression."
                      {:form form})))
    form))

(defn parse-module
  [source]
  (let [[tag name imports-form export-form & decl-forms] (edn/read-string source)]
    (expect-tag 'module [tag])
    {:name name
     :imports (parse-imports imports-form)
     :exports (parse-exports export-form)
     :decls (mapv parse-decl decl-forms)}))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T13:45:08.132797-05:00", :module-hash "-2090169151", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1367613317"} {:id "form/1/declare", :kind "declare", :line 4, :end-line 4, :hash "1128659220"} {:id "form/2/declare", :kind "declare", :line 5, :end-line 5, :hash "-1446383543"} {:id "defn-/parse-import", :kind "defn-", :line 7, :end-line 20, :hash "-1572688366"} {:id "defn-/expect-tag", :kind "defn-", :line 22, :end-line 28, :hash "-1315660753"} {:id "defn-/parse-imports", :kind "defn-", :line 30, :end-line 33, :hash "2124099339"} {:id "defn-/parse-exports", :kind "defn-", :line 35, :end-line 38, :hash "987718804"} {:id "defn-/parse-param", :kind "defn-", :line 40, :end-line 43, :hash "949945736"} {:id "defn-/parse-field", :kind "defn-", :line 45, :end-line 49, :hash "1251715611"} {:id "defn-/parse-effects", :kind "defn-", :line 51, :end-line 66, :hash "2129032603"} {:id "defn-/parse-contract-clause", :kind "defn-", :line 68, :end-line 71, :hash "2065410627"} {:id "defn-/parse-invariants", :kind "defn-", :line 73, :end-line 78, :hash "192681247"} {:id "defn-/parse-type-params", :kind "defn-", :line 80, :end-line 85, :hash "-1233585086"} {:id "defn-/parse-data-decl", :kind "defn-", :line 87, :end-line 95, :hash "240075906"} {:id "defn-/parse-enum-decl", :kind "defn-", :line 97, :end-line 101, :hash "53471563"} {:id "defn-/parse-variant-decl", :kind "defn-", :line 103, :end-line 107, :hash "545649087"} {:id "defn-/parse-case", :kind "defn-", :line 109, :end-line 113, :hash "505569259"} {:id "defn-/parse-call-expr", :kind "defn-", :line 115, :end-line 119, :hash "1364196914"} {:id "defn-/parse-construct-expr", :kind "defn-", :line 121, :end-line 125, :hash "587366883"} {:id "defn-/parse-variant-expr", :kind "defn-", :line 127, :end-line 132, :hash "-385235348"} {:id "defn-/parse-record-get-expr", :kind "defn-", :line 134, :end-line 138, :hash "1628847814"} {:id "defn-/parse-if-expr", :kind "defn-", :line 140, :end-line 145, :hash "-1106654283"} {:id "defn-/parse-match-expr", :kind "defn-", :line 147, :end-line 151, :hash "1800281827"} {:id "defn-/parse-binding", :kind "defn-", :line 153, :end-line 156, :hash "-1959265528"} {:id "defn-/parse-let-expr", :kind "defn-", :line 158, :end-line 162, :hash "-647963169"} {:id "defn-/parse-seq-expr", :kind "defn-", :line 164, :end-line 167, :hash "302220611"} {:id "defn-/parse-lambda-expr", :kind "defn-", :line 169, :end-line 175, :hash "-1706998005"} {:id "defn-/parse-catch-clause", :kind "defn-", :line 177, :end-line 182, :hash "1820817090"} {:id "defn-/parse-finally-clause", :kind "defn-", :line 184, :end-line 187, :hash "-378492021"} {:id "defn-/parse-try-expr", :kind "defn-", :line 189, :end-line 198, :hash "-1089530614"} {:id "defn-/parse-var-expr", :kind "defn-", :line 200, :end-line 205, :hash "1325393821"} {:id "defn-/parse-set-expr", :kind "defn-", :line 207, :end-line 211, :hash "1429713983"} {:id "defn-/parse-loop-expr", :kind "defn-", :line 213, :end-line 217, :hash "-198693472"} {:id "defn-/parse-recur-expr", :kind "defn-", :line 219, :end-line 222, :hash "-1009172251"} {:id "defn-/parse-raise-expr", :kind "defn-", :line 224, :end-line 227, :hash "-1091784450"} {:id "defn-/parse-primitive-unary-expr", :kind "defn-", :line 229, :end-line 232, :hash "1155615128"} {:id "defn-/parse-primitive-binary-expr", :kind "defn-", :line 234, :end-line 237, :hash "-981498976"} {:id "defn-/parse-io-read-line-expr", :kind "defn-", :line 239, :end-line 241, :hash "1866185537"} {:id "defn-/parse-io-print-expr", :kind "defn-", :line 243, :end-line 246, :hash "2139111722"} {:id "defn-/parse-io-println-expr", :kind "defn-", :line 248, :end-line 251, :hash "-369802684"} {:id "defn-/parse-string-split-on-expr", :kind "defn-", :line 253, :end-line 256, :hash "-549703719"} {:id "defn-/parse-seq-get-expr", :kind "defn-", :line 258, :end-line 261, :hash "683923185"} {:id "defn-/parse-type-args", :kind "defn-", :line 263, :end-line 266, :hash "-266637052"} {:id "defn-/parse-signature", :kind "defn-", :line 268, :end-line 272, :hash "804347329"} {:id "defn-/parse-java-new-expr", :kind "defn-", :line 274, :end-line 285, :hash "-1667842733"} {:id "defn-/parse-java-call-expr", :kind "defn-", :line 287, :end-line 293, :hash "-1542269505"} {:id "defn-/parse-java-static-call-expr", :kind "defn-", :line 295, :end-line 301, :hash "1075951034"} {:id "defn-/parse-java-get-field-expr", :kind "defn-", :line 303, :end-line 308, :hash "1853237465"} {:id "defn-/parse-java-static-get-field-expr", :kind "defn-", :line 310, :end-line 315, :hash "-1799498212"} {:id "defn-/parse-java-set-field-expr", :kind "defn-", :line 317, :end-line 323, :hash "-2021484420"} {:id "defn-/parse-java-static-set-field-expr", :kind "defn-", :line 325, :end-line 331, :hash "-1416091135"} {:id "def/expr-handlers", :kind "def", :line 333, :end-line 386, :hash "1244824756"} {:id "defn-/literal-pattern?", :kind "defn-", :line 388, :end-line 393, :hash "635525918"} {:id "defn-/wildcard-pattern?", :kind "defn-", :line 395, :end-line 397, :hash "76497133"} {:id "defn-/record-pattern-form?", :kind "defn-", :line 399, :end-line 401, :hash "-1394925114"} {:id "defn-/union-pattern-form?", :kind "defn-", :line 403, :end-line 405, :hash "765247313"} {:id "defn-/parse-literal-pattern", :kind "defn-", :line 407, :end-line 410, :hash "148576341"} {:id "defn-/parse-wildcard-pattern", :kind "defn-", :line 412, :end-line 414, :hash "688434532"} {:id "defn-/parse-binder-pattern", :kind "defn-", :line 416, :end-line 419, :hash "-1102720389"} {:id "defn-/parse-record-pattern", :kind "defn-", :line 421, :end-line 428, :hash "785832603"} {:id "defn-/parse-union-pattern", :kind "defn-", :line 430, :end-line 434, :hash "1434576212"} {:id "def/pattern-parsers", :kind "def", :line 436, :end-line 441, :hash "1242457034"} {:id "defn/parse-pattern", :kind "defn", :line 443, :end-line 451, :hash "-1975440247"} {:id "defn-/parse-union-decl", :kind "defn-", :line 453, :end-line 461, :hash "-1120328440"} {:id "defn-/parse-fn-decl", :kind "defn-", :line 463, :end-line 473, :hash "-1738991000"} {:id "defn-/parse-decl", :kind "defn-", :line 475, :end-line 483, :hash "-268353563"} {:id "defn/parse-expr", :kind "defn", :line 485, :end-line 492, :hash "-1503518551"} {:id "defn/parse-module", :kind "defn", :line 494, :end-line 501, :hash "52551823"}]}
;; clj-mutate-manifest-end
