(ns airj.jvm-emit-seq
  (:import (clojure.asm Label MethodVisitor Opcodes)))

(defn- emit-seq-element-cast
  [^MethodVisitor mv jvm-type]
  (case jvm-type
    :int (do
           (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Integer")
           (.visitMethodInsn mv
                             Opcodes/INVOKEVIRTUAL
                             "java/lang/Integer"
                             "intValue"
                             "()I"
                             false))
    :boolean (do
               (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Boolean")
               (.visitMethodInsn mv
                                 Opcodes/INVOKEVIRTUAL
                                 "java/lang/Boolean"
                                 "booleanValue"
                                 "()Z"
                                 false))
    :float (do
             (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Float")
             (.visitMethodInsn mv
                               Opcodes/INVOKEVIRTUAL
                               "java/lang/Float"
                               "floatValue"
                               "()F"
                               false))
    :double (do
              (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Double")
              (.visitMethodInsn mv
                                Opcodes/INVOKEVIRTUAL
                                "java/lang/Double"
                                "doubleValue"
                                "()D"
                                false))
    (when (string? jvm-type)
      (.visitTypeInsn mv Opcodes/CHECKCAST jvm-type))))

(defn emit-seq-empty
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (let [true-label (Label.)
          end-label (Label.)]
      (emit-expr mv (:arg expr) env)
      (.visitInsn mv Opcodes/ARRAYLENGTH)
      (.visitJumpInsn mv Opcodes/IFEQ true-label)
      (.visitLdcInsn mv false)
      (.visitJumpInsn mv Opcodes/GOTO end-label)
      (.visitLabel mv true-label)
      (.visitLdcInsn mv true)
      (.visitLabel mv end-label))
    (do
      (emit-expr mv (:arg expr) env)
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "isEmpty"
                        "()Z"
                        true))))

(defn emit-seq-empty-new
  [^MethodVisitor mv _expr _env]
  (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
  (.visitInsn mv Opcodes/DUP)
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/ArrayList"
                    "<init>"
                    "()V"
                    false))

(defn emit-seq-length
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (.visitInsn mv Opcodes/ARRAYLENGTH)
    (.visitMethodInsn mv
                      Opcodes/INVOKEINTERFACE
                      "java/util/List"
                      "size"
                      "()I"
                      true)))

(defn emit-seq-first
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitInsn mv Opcodes/ICONST_0)
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (.visitInsn mv Opcodes/AALOAD)
    (do
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "get"
                        "(I)Ljava/lang/Object;"
                        true)
      (emit-seq-element-cast mv (:jvm-type expr)))))

(defn emit-seq-get
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (if (= "[Ljava/lang/String;" (:jvm-type (first (:args expr))))
    (.visitInsn mv Opcodes/AALOAD)
    (do
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "get"
                        "(I)Ljava/lang/Object;"
                        true)
      (emit-seq-element-cast mv (:jvm-type expr)))))

(defn- emit-seq-as-list
  [^MethodVisitor mv expr env emit-expr]
  (emit-expr mv expr env)
  (when (= "[Ljava/lang/String;" (:jvm-type expr))
    (.visitMethodInsn mv
                      Opcodes/INVOKESTATIC
                      "java/util/Arrays"
                      "asList"
                      "([Ljava/lang/Object;)Ljava/util/List;"
                      false)))

(defn emit-seq-rest
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (do
      (emit-expr mv (:arg expr) env)
      (.visitInsn mv Opcodes/DUP)
      (.visitInsn mv Opcodes/ARRAYLENGTH)
      (.visitInsn mv Opcodes/DUP)
      (.visitInsn mv Opcodes/ICONST_1)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/lang/Math"
                        "min"
                        "(II)I"
                        false)
      (.visitInsn mv Opcodes/SWAP)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/util/Arrays"
                        "copyOfRange"
                        "([Ljava/lang/Object;II)[Ljava/lang/Object;"
                        false)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/util/Arrays"
                        "asList"
                        "([Ljava/lang/Object;)Ljava/util/List;"
                        false))
    (do
      (emit-expr mv (:arg expr) env)
      (.visitInsn mv Opcodes/DUP)
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "size"
                        "()I"
                        true)
      (.visitInsn mv Opcodes/DUP)
      (.visitInsn mv Opcodes/ICONST_1)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/lang/Math"
                        "min"
                        "(II)I"
                        false)
      (.visitInsn mv Opcodes/SWAP)
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "subList"
                        "(II)Ljava/util/List;"
                        true)
      (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
      (.visitInsn mv Opcodes/DUP_X1)
      (.visitInsn mv Opcodes/SWAP)
      (.visitMethodInsn mv
                        Opcodes/INVOKESPECIAL
                        "java/util/ArrayList"
                        "<init>"
                        "(Ljava/util/Collection;)V"
                        false))))

(defn emit-seq-concat
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
  (.visitInsn mv Opcodes/DUP)
  (emit-seq-as-list mv (first (:args expr)) env emit-expr)
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/ArrayList"
                    "<init>"
                    "(Ljava/util/Collection;)V"
                    false)
  (.visitInsn mv Opcodes/DUP)
  (emit-seq-as-list mv (second (:args expr)) env emit-expr)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/util/ArrayList"
                    "addAll"
                    "(Ljava/util/Collection;)Z"
                    false)
  (.visitInsn mv Opcodes/POP))

(defn emit-seq-append
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
  (.visitInsn mv Opcodes/DUP)
  (emit-seq-as-list mv (first (:args expr)) env emit-expr)
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/ArrayList"
                    "<init>"
                    "(Ljava/util/Collection;)V"
                    false)
  (.visitInsn mv Opcodes/DUP)
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/util/ArrayList"
                    "add"
                    "(Ljava/lang/Object;)Z"
                    false)
  (.visitInsn mv Opcodes/POP))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T12:34:57.42655-05:00", :module-hash "1098274364", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-159648526"} {:id "defn-/emit-seq-element-cast", :kind "defn-", :line 4, :end-line 40, :hash "-321324069"} {:id "defn/emit-seq-empty", :kind "defn", :line 42, :end-line 62, :hash "-1697564915"} {:id "defn/emit-seq-empty-new", :kind "defn", :line 64, :end-line 73, :hash "2136217911"} {:id "defn/emit-seq-length", :kind "defn", :line 75, :end-line 85, :hash "-473209208"} {:id "defn/emit-seq-first", :kind "defn", :line 87, :end-line 100, :hash "1654991180"} {:id "defn/emit-seq-get", :kind "defn", :line 102, :end-line 115, :hash "1066037583"} {:id "defn-/emit-seq-as-list", :kind "defn-", :line 117, :end-line 126, :hash "599544557"} {:id "defn/emit-seq-rest", :kind "defn", :line 128, :end-line 188, :hash "-1409108672"} {:id "defn/emit-seq-concat", :kind "defn", :line 190, :end-line 209, :hash "-1653446077"} {:id "defn/emit-seq-append", :kind "defn", :line 211, :end-line 230, :hash "-882299449"}]}
;; clj-mutate-manifest-end
