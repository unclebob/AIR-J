(ns airj.jvm-emit-closures
  (:import (clojure.asm ClassWriter MethodVisitor Opcodes)))

(defn emit-closure-new
  [^MethodVisitor mv expr env {:keys [emit-expr constructor-descriptor]}]
  (.visitTypeInsn mv Opcodes/NEW (:class-name expr))
  (.visitInsn mv Opcodes/DUP)
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    (:class-name expr)
                    "<init>"
                    (constructor-descriptor (:args expr))
                    false))

(defn emit-closure-call
  [^MethodVisitor mv expr env {:keys [emit-expr descriptor]}]
  (emit-expr mv (:callee expr) env)
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    (:interface-name expr)
                    (:method-name expr)
                    (str "("
                         (apply str (map descriptor (:parameter-types expr)))
                         ")"
                         (descriptor (:return-type expr)))
                    true))

(defn emit-closure-interface-bytes
  [^ClassWriter cw interface method-descriptor]
  (.visit cw
          Opcodes/V1_8
          (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE)
          (:class-name interface)
          nil
          "java/lang/Object"
          nil)
  (.visitMethod cw
                (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
                (:method-name interface)
                (method-descriptor (mapv (fn [jvm-type]
                                           {:jvm-type jvm-type})
                                         (:params interface))
                                   (:return-type interface))
                nil
                nil)
  (.visitEnd cw)
  (.toByteArray cw))

(defn emit-closure-class-bytes
  [^ClassWriter cw closure
   {:keys [descriptor init-descriptor field-slot load-opcode store-opcode
           method-descriptor instance-local-slots bind-typed-slots emit-expr
           return-opcode]}]
  (let [interfaces (into-array String [(:interface-name closure)])]
    (.visit cw
            Opcodes/V1_8
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
            (:class-name closure)
            nil
            "java/lang/Object"
            interfaces)
    (doseq [capture (:captures closure)]
      (.visitField cw
                   (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL)
                   (str (:name capture))
                   (descriptor (:jvm-type capture))
                   nil
                   nil))
    (let [mv (.visitMethod cw
                           Opcodes/ACC_PUBLIC
                           "<init>"
                           (init-descriptor (:captures closure))
                           nil
                           nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V" false)
      (doseq [[slot capture] (field-slot (:captures closure))]
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (.visitVarInsn mv (load-opcode (:jvm-type capture)) slot)
        (.visitFieldInsn mv
                         Opcodes/PUTFIELD
                         (:class-name closure)
                         (str (:name capture))
                         (descriptor (:jvm-type capture))))
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    (let [mv (.visitMethod cw
                           Opcodes/ACC_PUBLIC
                           (:method-name closure)
                           (method-descriptor (:params closure) (:return-type closure))
                           nil
                           nil)
          env (bind-typed-slots (instance-local-slots (:params closure))
                                (:captures closure)
                                :jvm-type)]
      (.visitCode mv)
      (doseq [capture (:captures closure)]
        (let [{:keys [slot jvm-type]} (get-in env [:slots (:name capture)])]
          (.visitVarInsn mv Opcodes/ALOAD 0)
          (.visitFieldInsn mv
                           Opcodes/GETFIELD
                           (:class-name closure)
                           (str (:name capture))
                           (descriptor (:jvm-type capture)))
          (.visitVarInsn mv (store-opcode jvm-type) slot)))
      (emit-expr mv (:body closure) env)
      (.visitInsn mv (return-opcode (:return-type closure)))
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    (.visitEnd cw)
    (.toByteArray cw)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T10:54:40.294081-05:00", :module-hash "-1621012136", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-1924671779"} {:id "defn/emit-closure-new", :kind "defn", :line 4, :end-line 15, :hash "319722235"} {:id "defn/emit-closure-call", :kind "defn", :line 17, :end-line 30, :hash "-1064396013"} {:id "defn/emit-closure-interface-bytes", :kind "defn", :line 32, :end-line 51, :hash "1799793276"} {:id "defn/emit-closure-class-bytes", :kind "defn", :line 53, :end-line 117, :hash "-708110112"}]}
;; clj-mutate-manifest-end
