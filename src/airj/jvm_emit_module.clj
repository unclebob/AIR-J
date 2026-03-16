(ns airj.jvm-emit-module
  (:require [airj.jvm-class-writer :as jvm-class-writer]
            [airj.jvm-emit-closures :as jvm-emit-closures]
            [airj.jvm-emit-entry :as jvm-emit-entry])
  (:import (clojure.asm ClassWriter MethodVisitor Opcodes)))

(defn- emit-default-constructor
  ([^ClassWriter cw]
   (emit-default-constructor cw "java/lang/Object"))
  ([^ClassWriter cw super-name]
   (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
     (.visitCode mv)
     (.visitVarInsn mv Opcodes/ALOAD 0)
     (.visitMethodInsn mv Opcodes/INVOKESPECIAL super-name "<init>" "()V" false)
     (.visitInsn mv Opcodes/RETURN)
     (.visitMaxs mv 0 0)
     (.visitEnd mv))))

(defn- emit-record-constructor
  [^ClassWriter cw record {:keys [init-descriptor field-slot load-opcode descriptor]}]
  (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" (init-descriptor (:fields record)) nil nil)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V" false)
    (doseq [[slot field] (field-slot (:fields record))]
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitVarInsn mv (load-opcode (:jvm-type field)) slot)
      (.visitFieldInsn mv
                       Opcodes/PUTFIELD
                       (:class-name record)
                       (str (:name field))
                       (descriptor (:jvm-type field))))
    (.visitInsn mv Opcodes/RETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-record-class-bytes
  [module record {:keys [descriptor]
                  :as opts}]
  (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
    (.visit cw
            Opcodes/V1_8
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
            (:class-name record)
            nil
            "java/lang/Object"
            nil)
    (doseq [field (:fields record)]
      (.visitField cw
                   (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
                   (str (:name field))
                   (descriptor (:jvm-type field))
                   nil
                   nil))
    (emit-default-constructor cw)
    (emit-record-constructor cw record opts)
    (.visitEnd cw)
    (.toByteArray cw)))

(defn- emit-union-base-class-bytes
  [module union]
  (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
    (.visit cw
            Opcodes/V1_8
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
            (:base-class union)
            nil
            "java/lang/Object"
            nil)
    (emit-default-constructor cw)
    (.visitEnd cw)
    (.toByteArray cw)))

(defn- emit-union-variant-class-bytes
  [module union variant {:keys [descriptor init-descriptor field-slot load-opcode]}]
  (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
    (.visit cw
            Opcodes/V1_8
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
            (:class-name variant)
            nil
            (:base-class union)
            nil)
    (doseq [field (:fields variant)]
      (.visitField cw
                   (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
                   (str (:name field))
                   (descriptor (:jvm-type field))
                   nil
                   nil))
    (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" (init-descriptor (:fields variant)) nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL (:base-class union) "<init>" "()V" false)
      (doseq [[slot field] (field-slot (:fields variant))]
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (.visitVarInsn mv (load-opcode (:jvm-type field)) slot)
        (.visitFieldInsn mv
                         Opcodes/PUTFIELD
                         (:class-name variant)
                         (str (:name field))
                         (descriptor (:jvm-type field))))
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    (.visitEnd cw)
    (.toByteArray cw)))

(defn- emit-method
  [^ClassWriter cw method {:keys [method-descriptor method-name local-slots emit-expr emit-cast-or-unbox return-opcode]}]
  (let [descriptor (method-descriptor (:params method) (:return-type method))
        mv (.visitMethod cw
                         (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                         (method-name (:name method))
                         descriptor
                         nil
                         nil)
        env (local-slots (:params method))]
    (.visitCode mv)
    (emit-expr mv (:body method) env)
    (emit-cast-or-unbox mv (:jvm-type (:body method)) (:return-type method))
    (.visitInsn mv (return-opcode (:return-type method)))
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-instance-method
  [^ClassWriter cw method {:keys [method-descriptor method-name load-opcode descriptor return-opcode]}]
  (let [method-desc (method-descriptor (:params method) (:return-type method))
        mv (.visitMethod cw
                         Opcodes/ACC_PUBLIC
                         (method-name (:name method))
                         method-desc
                         nil
                         nil)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (doseq [[index param] (map-indexed vector (:params method))]
      (.visitVarInsn mv (load-opcode (:jvm-type param)) (inc index)))
    (.visitMethodInsn mv
                      Opcodes/INVOKESTATIC
                      (get-in method [:target :owner])
                      (method-name (get-in method [:target :name]))
                      (str "("
                           (apply str (map descriptor (get-in method [:target :parameter-types])))
                           ")"
                           (descriptor (get-in method [:target :return-type])))
                      false)
    (.visitInsn mv (return-opcode (:return-type method)))
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-module-bytes
  [module {:keys [method-name method-descriptor pop-opcode]
           :as opts}]
  (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
    (.visit cw
            Opcodes/V1_8
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
            (:internal-name module)
            nil
            (or (get-in module [:host :class-name])
                "java/lang/Object")
            nil)
    (emit-default-constructor cw (or (get-in module [:host :class-name])
                                     "java/lang/Object"))
    (doseq [method (:methods module)]
      (emit-method cw method opts))
    (doseq [method (:instance-methods module)]
      (emit-instance-method cw method opts))
    (jvm-emit-entry/emit-main-wrapper cw module
                                      {:method-name method-name
                                       :method-descriptor method-descriptor
                                       :pop-opcode pop-opcode})
    (.visitEnd cw)
    (.toByteArray cw)))

(defn emit-class-bytes
  [module {:keys [descriptor init-descriptor field-slot load-opcode store-opcode method-descriptor instance-local-slots
                  bind-typed-slots emit-expr return-opcode emit-cast-or-unbox method-name pop-opcode local-slots]}]
  (merge {(:internal-name module) (emit-module-bytes module {:method-name method-name
                                                             :method-descriptor method-descriptor
                                                             :pop-opcode pop-opcode
                                                             :local-slots local-slots
                                                             :emit-expr emit-expr
                                                             :emit-cast-or-unbox emit-cast-or-unbox
                                                             :return-opcode return-opcode
                                                             :load-opcode load-opcode
                                                             :descriptor descriptor})}
         (into {}
               (map (fn [interface]
                      [(:class-name interface)
                       (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
                         (jvm-emit-closures/emit-closure-interface-bytes
                          cw
                          interface
                          method-descriptor))]))
               (:closure-interfaces module))
         (into {}
               (map (fn [closure]
                      [(:class-name closure)
                       (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
                         (jvm-emit-closures/emit-closure-class-bytes
                          cw
                          closure
                          {:descriptor descriptor
                           :init-descriptor init-descriptor
                           :field-slot field-slot
                           :load-opcode load-opcode
                           :store-opcode store-opcode
                           :method-descriptor method-descriptor
                           :instance-local-slots instance-local-slots
                           :bind-typed-slots bind-typed-slots
                           :emit-expr emit-expr
                           :return-opcode return-opcode}))]))
               (:closures module))
         (into {}
               (map (fn [record]
                      [(:class-name record) (emit-record-class-bytes module record {:descriptor descriptor
                                                                                    :init-descriptor init-descriptor
                                                                                    :field-slot field-slot
                                                                                    :load-opcode load-opcode})]))
               (:records module))
         (into {}
               (mapcat
                (fn [union]
                  (concat [[(:base-class union) (emit-union-base-class-bytes module union)]]
                          (map (fn [variant]
                                 [(:class-name variant)
                                  (emit-union-variant-class-bytes module union variant {:descriptor descriptor
                                                                                        :init-descriptor init-descriptor
                                                                                        :field-slot field-slot
                                                                                        :load-opcode load-opcode})])
                               (:variants union))))
               (:unions module)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T08:16:03.178574-05:00", :module-hash "694636045", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-146378488"} {:id "defn-/emit-default-constructor", :kind "defn-", :line 7, :end-line 17, :hash "240160418"} {:id "defn-/emit-record-constructor", :kind "defn-", :line 19, :end-line 35, :hash "-1948852432"} {:id "defn-/emit-record-class-bytes", :kind "defn-", :line 37, :end-line 58, :hash "805782924"} {:id "defn-/emit-union-base-class-bytes", :kind "defn-", :line 60, :end-line 72, :hash "861277052"} {:id "defn-/emit-union-variant-class-bytes", :kind "defn-", :line 74, :end-line 107, :hash "721690522"} {:id "defn-/emit-method", :kind "defn-", :line 109, :end-line 124, :hash "2011174467"} {:id "defn-/emit-instance-method", :kind "defn-", :line 126, :end-line 150, :hash "-792834049"} {:id "defn-/emit-module-bytes", :kind "defn-", :line 152, :end-line 175, :hash "1304969446"} {:id "defn/emit-class-bytes", :kind "defn", :line 177, :end-line 234, :hash "1001080615"}]}
;; clj-mutate-manifest-end
