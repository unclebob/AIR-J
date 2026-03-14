(ns airj.jvm-emit-data
  (:import (clojure.asm Label MethodVisitor Opcodes)))

(defn emit-construct
  [^MethodVisitor mv expr env {:keys [emit-expr emit-box-if-needed constructor-descriptor-for-types]}]
  (let [parameter-types (or (:parameter-types expr)
                            (mapv :jvm-type (:args expr)))]
    (.visitTypeInsn mv Opcodes/NEW (:class-name expr))
    (.visitInsn mv Opcodes/DUP)
    (doseq [[arg parameter-type] (map vector (:args expr) parameter-types)]
      (emit-expr mv arg env)
      (emit-box-if-needed mv (:jvm-type arg) parameter-type))
    (.visitMethodInsn mv
                      Opcodes/INVOKESPECIAL
                      (:class-name expr)
                      "<init>"
                      (constructor-descriptor-for-types parameter-types)
                      false)))

(defn emit-record-get
  [^MethodVisitor mv expr env {:keys [emit-expr emit-cast-or-unbox descriptor]}]
  (let [field-jvm-type (or (:field-jvm-type expr) (:jvm-type expr))]
    (emit-expr mv (:target expr) env)
    (.visitFieldInsn mv
                     Opcodes/GETFIELD
                     (:jvm-type (:target expr))
                     (str (:field expr))
                     (descriptor field-jvm-type))
    (emit-cast-or-unbox mv field-jvm-type (:jvm-type expr))))

(defn emit-java-get-field
  [^MethodVisitor mv expr env {:keys [emit-expr descriptor]}]
  (emit-expr mv (:target expr) env)
  (.visitFieldInsn mv
                   Opcodes/GETFIELD
                   (:jvm-type (:target expr))
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn emit-java-static-get-field
  [^MethodVisitor mv expr {:keys [descriptor]}]
  (.visitFieldInsn mv
                   Opcodes/GETSTATIC
                   (:class-name expr)
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn emit-java-set-field
  [^MethodVisitor mv expr env {:keys [emit-expr descriptor]}]
  (emit-expr mv (:target expr) env)
  (emit-expr mv (:expr expr) env)
  (.visitFieldInsn mv
                   Opcodes/PUTFIELD
                   (:jvm-type (:target expr))
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn emit-java-static-set-field
  [^MethodVisitor mv expr env {:keys [emit-expr descriptor]}]
  (emit-expr mv (:expr expr) env)
  (.visitFieldInsn mv
                   Opcodes/PUTSTATIC
                   (:class-name expr)
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn emit-variant
  [^MethodVisitor mv expr env opts]
  (emit-construct mv expr env opts))

(defn emit-map-empty
  [^MethodVisitor mv]
  (.visitTypeInsn mv Opcodes/NEW "java/util/LinkedHashMap")
  (.visitInsn mv Opcodes/DUP)
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/LinkedHashMap"
                    "<init>"
                    "()V"
                    false))

(defn emit-map-set
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (let [value-expr (nth (:args expr) 2)]
    (.visitTypeInsn mv Opcodes/NEW "java/util/LinkedHashMap")
    (.visitInsn mv Opcodes/DUP)
    (emit-expr mv (first (:args expr)) env)
    (.visitMethodInsn mv
                      Opcodes/INVOKESPECIAL
                      "java/util/LinkedHashMap"
                      "<init>"
                      "(Ljava/util/Map;)V"
                      false)
    (.visitInsn mv Opcodes/DUP)
    (emit-expr mv (second (:args expr)) env)
    (emit-expr mv value-expr env)
    (case (:jvm-type value-expr)
      :int (.visitMethodInsn mv
                             Opcodes/INVOKESTATIC
                             "java/lang/Integer"
                             "valueOf"
                             "(I)Ljava/lang/Integer;"
                             false)
      :boolean (.visitMethodInsn mv
                                 Opcodes/INVOKESTATIC
                                 "java/lang/Boolean"
                                 "valueOf"
                                 "(Z)Ljava/lang/Boolean;"
                                 false)
      :float (.visitMethodInsn mv
                               Opcodes/INVOKESTATIC
                               "java/lang/Float"
                               "valueOf"
                               "(F)Ljava/lang/Float;"
                               false)
      :double (.visitMethodInsn mv
                                Opcodes/INVOKESTATIC
                                "java/lang/Double"
                                "valueOf"
                                "(D)Ljava/lang/Double;"
                                false)
      nil)
    (.visitMethodInsn mv
                      Opcodes/INVOKEINTERFACE
                      "java/util/Map"
                      "put"
                      "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                      true)
    (.visitInsn mv Opcodes/POP)))

(defn emit-map-get
  [^MethodVisitor mv expr env {:keys [emit-expr constructor-descriptor-for-types]}]
  (let [some-label (Label.)
        end-label (Label.)]
    (emit-expr mv (first (:args expr)) env)
    (emit-expr mv (second (:args expr)) env)
    (.visitInsn mv Opcodes/DUP2)
    (.visitMethodInsn mv
                      Opcodes/INVOKEINTERFACE
                      "java/util/Map"
                      "containsKey"
                      "(Ljava/lang/Object;)Z"
                      true)
    (.visitJumpInsn mv Opcodes/IFNE some-label)
    (.visitInsn mv Opcodes/POP2)
    (.visitTypeInsn mv Opcodes/NEW (:none-class-name expr))
    (.visitInsn mv Opcodes/DUP)
    (.visitMethodInsn mv
                      Opcodes/INVOKESPECIAL
                      (:none-class-name expr)
                      "<init>"
                      "()V"
                      false)
    (.visitJumpInsn mv Opcodes/GOTO end-label)
    (.visitLabel mv some-label)
    (.visitMethodInsn mv
                      Opcodes/INVOKEINTERFACE
                      "java/util/Map"
                      "get"
                      "(Ljava/lang/Object;)Ljava/lang/Object;"
                      true)
    (.visitTypeInsn mv Opcodes/NEW (:some-class-name expr))
    (.visitInsn mv Opcodes/DUP_X1)
    (.visitInsn mv Opcodes/SWAP)
    (.visitMethodInsn mv
                      Opcodes/INVOKESPECIAL
                      (:some-class-name expr)
                      "<init>"
                      (constructor-descriptor-for-types (:some-parameter-types expr))
                      false)
    (.visitLabel mv end-label)))

(defn emit-map-contains
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "java/util/Map"
                    "containsKey"
                    "(Ljava/lang/Object;)Z"
                    true))

(defn emit-map-keys
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
  (.visitInsn mv Opcodes/DUP)
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "java/util/Map"
                    "keySet"
                    "()Ljava/util/Set;"
                    true)
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/ArrayList"
                    "<init>"
                    "(Ljava/util/Collection;)V"
                    false))

(defn emit-instance-of
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:target expr) env)
  (.visitTypeInsn mv Opcodes/INSTANCEOF (:class-name expr)))

(defn emit-variant-field
  [^MethodVisitor mv expr env {:keys [emit-expr emit-cast-or-unbox descriptor]}]
  (let [field-jvm-type (or (:field-jvm-type expr) (:jvm-type expr))]
    (emit-expr mv (:target expr) env)
    (.visitTypeInsn mv Opcodes/CHECKCAST (:class-name expr))
    (.visitFieldInsn mv
                     Opcodes/GETFIELD
                     (:class-name expr)
                     (str (:field expr))
                     (descriptor field-jvm-type))
    (emit-cast-or-unbox mv field-jvm-type (:jvm-type expr))))

(defn emit-literal-test
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:target expr) env)
  (emit-expr mv (:literal expr) env)
  (let [false-label (Label.)
        end-label (Label.)]
    (.visitJumpInsn mv Opcodes/IF_ICMPNE false-label)
    (.visitInsn mv Opcodes/ICONST_1)
    (.visitJumpInsn mv Opcodes/GOTO end-label)
    (.visitLabel mv false-label)
    (.visitInsn mv Opcodes/ICONST_0)
    (.visitLabel mv end-label)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T09:34:25.342413-05:00", :module-hash "-599899254", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-732582226"} {:id "defn/emit-construct", :kind "defn", :line 4, :end-line 18, :hash "881575364"} {:id "defn/emit-record-get", :kind "defn", :line 20, :end-line 29, :hash "1105717458"} {:id "defn/emit-java-get-field", :kind "defn", :line 31, :end-line 38, :hash "72788422"} {:id "defn/emit-java-static-get-field", :kind "defn", :line 40, :end-line 46, :hash "1885727713"} {:id "defn/emit-java-set-field", :kind "defn", :line 48, :end-line 56, :hash "-972448071"} {:id "defn/emit-java-static-set-field", :kind "defn", :line 58, :end-line 65, :hash "-198808029"} {:id "defn/emit-variant", :kind "defn", :line 67, :end-line 69, :hash "1550034548"} {:id "defn/emit-map-empty", :kind "defn", :line 71, :end-line 80, :hash "-1855517679"} {:id "defn/emit-map-set", :kind "defn", :line 82, :end-line 129, :hash "1728533096"} {:id "defn/emit-map-get", :kind "defn", :line 131, :end-line 171, :hash "289727427"} {:id "defn/emit-map-contains", :kind "defn", :line 173, :end-line 182, :hash "-1455959828"} {:id "defn/emit-map-keys", :kind "defn", :line 184, :end-line 200, :hash "723777927"} {:id "defn/emit-instance-of", :kind "defn", :line 202, :end-line 205, :hash "1626769910"} {:id "defn/emit-variant-field", :kind "defn", :line 207, :end-line 217, :hash "2081961259"} {:id "defn/emit-literal-test", :kind "defn", :line 219, :end-line 230, :hash "-1971300425"}]}
;; clj-mutate-manifest-end
