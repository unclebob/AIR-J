(ns airj.jvm-emit-cells
  (:import (clojure.asm MethodVisitor Opcodes)))

(def ^:private primitive-newarray-opcodes
  {:int Opcodes/T_INT
   :boolean Opcodes/T_BOOLEAN})

(def ^:private array-load-opcodes
  {:int Opcodes/IALOAD
   :boolean Opcodes/BALOAD})

(def ^:private array-store-opcodes
  {:int Opcodes/IASTORE
   :boolean Opcodes/BASTORE})

(defn- array-load-opcode
  [value-jvm-type]
  (get array-load-opcodes value-jvm-type Opcodes/AALOAD))

(defn- array-store-opcode
  [value-jvm-type]
  (get array-store-opcodes value-jvm-type Opcodes/AASTORE))

(defn- newarray-opcode
  [value-jvm-type]
  (get primitive-newarray-opcodes value-jvm-type))

(defn emit-cell-new
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (.visitInsn mv Opcodes/ICONST_1)
  (if-let [opcode (newarray-opcode (:value-jvm-type expr))]
    (.visitIntInsn mv Opcodes/NEWARRAY opcode)
    (.visitTypeInsn mv
                    Opcodes/ANEWARRAY
                    (if (and (string? (:value-jvm-type expr))
                             (.startsWith ^String (:value-jvm-type expr) "["))
                      (:value-jvm-type expr)
                      (:value-jvm-type expr))))
  (.visitInsn mv Opcodes/DUP)
  (.visitInsn mv Opcodes/ICONST_0)
  (emit-expr mv (:init expr) env)
  (.visitInsn mv (array-store-opcode (:value-jvm-type expr))))

(defn emit-cell-get
  [^MethodVisitor mv expr env]
  (let [{:keys [slot]} (get-in env [:slots (:name expr)])]
    (.visitVarInsn mv Opcodes/ALOAD slot)
    (.visitInsn mv Opcodes/ICONST_0)
    (.visitInsn mv (array-load-opcode (:jvm-type expr)))))

(defn emit-cell-set
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (let [{:keys [slot]} (get-in env [:slots (:name expr)])]
    (.visitVarInsn mv Opcodes/ALOAD slot)
    (.visitInsn mv Opcodes/ICONST_0)
    (emit-expr mv (:expr expr) env)
    (.visitInsn mv (array-store-opcode (:value-jvm-type expr)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T11:59:11.961459-05:00", :module-hash "-481784414", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "247380547"} {:id "def/primitive-newarray-opcodes", :kind "def", :line 4, :end-line 6, :hash "-1673864911"} {:id "def/array-load-opcodes", :kind "def", :line 8, :end-line 10, :hash "-1931661073"} {:id "def/array-store-opcodes", :kind "def", :line 12, :end-line 14, :hash "-2090231035"} {:id "defn-/array-load-opcode", :kind "defn-", :line 16, :end-line 18, :hash "-1597442426"} {:id "defn-/array-store-opcode", :kind "defn-", :line 20, :end-line 22, :hash "830235746"} {:id "defn-/newarray-opcode", :kind "defn-", :line 24, :end-line 26, :hash "-366247229"} {:id "defn/emit-cell-new", :kind "defn", :line 28, :end-line 42, :hash "226234000"} {:id "defn/emit-cell-get", :kind "defn", :line 44, :end-line 49, :hash "-136463429"} {:id "defn/emit-cell-set", :kind "defn", :line 51, :end-line 57, :hash "-739709251"}]}
;; clj-mutate-manifest-end
