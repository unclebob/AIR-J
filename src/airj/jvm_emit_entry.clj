(ns airj.jvm-emit-entry
  (:import (clojure.asm ClassWriter Opcodes)))

(defn exported-main
  [module]
  (when (some #{'main} (:exports module))
    (some (fn [method]
            (when (and (= 'main (:name method))
                       (or (empty? (:params method))
                           (= [{:name 'args
                                :jvm-type "[Ljava/lang/String;"}]
                              (:params method))))
              method))
          (:methods module))))

(defn emit-main-wrapper
  [^ClassWriter cw module {:keys [method-name method-descriptor pop-opcode]}]
  (when-let [airj-main (exported-main module)]
    (let [mv (.visitMethod cw
                           (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                           "main"
                           "([Ljava/lang/String;)V"
                           nil
                           nil)]
      (.visitCode mv)
      (when (= [{:name 'args
                 :jvm-type "[Ljava/lang/String;"}]
               (:params airj-main))
        (.visitVarInsn mv Opcodes/ALOAD 0))
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        (:internal-name module)
                        (method-name (:name airj-main))
                        (method-descriptor (:params airj-main) (:return-type airj-main))
                        false)
      (case (:return-type airj-main)
        :int (.visitMethodInsn mv
                               Opcodes/INVOKESTATIC
                               "java/lang/System"
                               "exit"
                               "(I)V"
                               false)
        (when-let [opcode (pop-opcode (:return-type airj-main))]
          (.visitInsn mv opcode)))
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T10:54:40.123932-05:00", :module-hash "657254104", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-1211713187"} {:id "defn/exported-main", :kind "defn", :line 4, :end-line 14, :hash "-1641205178"} {:id "defn/emit-main-wrapper", :kind "defn", :line 16, :end-line 47, :hash "-614754525"}]}
;; clj-mutate-manifest-end
