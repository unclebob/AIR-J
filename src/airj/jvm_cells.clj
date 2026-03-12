(ns airj.jvm-cells)

(def ^:private type-descriptors
  {:int "I"
   :boolean "Z"
   :void "V"})

(defn descriptor
  [jvm-type]
  (or (get type-descriptors jvm-type)
      (if (and (string? jvm-type)
               (.startsWith ^String jvm-type "["))
        jvm-type
        (str "L" jvm-type ";"))))

(defn cell-jvm-type
  [value-jvm-type]
  (str "[" (descriptor value-jvm-type)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T11:57:45.122172-05:00", :module-hash "-248800953", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "1567578919"} {:id "def/type-descriptors", :kind "def", :line 3, :end-line 6, :hash "-1455056336"} {:id "defn/descriptor", :kind "defn", :line 8, :end-line 14, :hash "1037649920"} {:id "defn/cell-jvm-type", :kind "defn", :line 16, :end-line 18, :hash "-1449053872"}]}
;; clj-mutate-manifest-end
