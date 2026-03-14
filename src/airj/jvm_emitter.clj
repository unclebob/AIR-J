(ns airj.jvm-emitter
  (:require [airj.jvm-cells :as jvm-cells]
            [airj.jvm-class-writer :as jvm-class-writer]
            [airj.jvm-emit-cells :as jvm-emit-cells]
            [airj.jvm-emit-closures :as jvm-emit-closures]
            [airj.jvm-emit-entry :as jvm-emit-entry]
            [airj.jvm-emit-text :as jvm-emit-text]
            [airj.jvm-emit-try :as jvm-emit-try])
  (:import (clojure.asm ClassWriter Label MethodVisitor Opcodes)))

(declare emit-expr)
(declare descriptor)
(declare constructor-descriptor)
(declare emit-expr-handlers)
(declare emit-binding)
(declare bind-var-slot)

(def ^:private type-descriptors
  {:int "I"
   :float "F"
   :double "D"
   :boolean "Z"
   :void "V"})

(def ^:private load-opcodes
  {:int Opcodes/ILOAD
   :float Opcodes/FLOAD
   :double Opcodes/DLOAD
   :boolean Opcodes/ILOAD})

(def ^:private store-opcodes
  {:int Opcodes/ISTORE
   :float Opcodes/FSTORE
   :double Opcodes/DSTORE
   :boolean Opcodes/ISTORE})

(def ^:private return-opcodes
  {:int Opcodes/IRETURN
   :float Opcodes/FRETURN
   :double Opcodes/DRETURN
   :boolean Opcodes/IRETURN
   :void Opcodes/RETURN})

(def ^:private boxed-types
  {:int "java/lang/Integer"
   :boolean "java/lang/Boolean"
   :float "java/lang/Float"
   :double "java/lang/Double"})

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- method-name
  [name]
  (if (= 'main name)
    "airj_main"
    (-> name str (.replace "-" "_"))))

(defn- init-descriptor
  [fields]
  (str "("
       (apply str (map (comp descriptor :jvm-type) fields))
       ")V"))

(defn- descriptor
  [jvm-type]
  (jvm-cells/descriptor jvm-type))

(defn- method-descriptor
  [params return-type]
  (str "("
       (apply str (map (comp descriptor :jvm-type) params))
       ")"
       (descriptor return-type)))

(defn- load-opcode
  [jvm-type]
  (get load-opcodes jvm-type Opcodes/ALOAD))

(defn- store-opcode
  [jvm-type]
  (get store-opcodes jvm-type Opcodes/ASTORE))

(defn- return-opcode
  [jvm-type]
  (get return-opcodes jvm-type Opcodes/ARETURN))

(defn- slot-width
  [jvm-type]
  (case jvm-type
    :double 2
    1))

(defn- local-slots
  [params]
  (reduce (fn [acc param]
            (-> acc
                (assoc-in [:slots (:name param)]
                          {:slot (:next-slot acc)
                           :jvm-type (:jvm-type param)})
                (update :next-slot + (slot-width (:jvm-type param)))))
          {:slots {}
           :next-slot 0}
          params))

(defn- instance-local-slots
  [params]
  (reduce (fn [acc param]
            (-> acc
                (assoc-in [:slots (:name param)]
                          {:slot (:next-slot acc)
                           :jvm-type (:jvm-type param)})
                (update :next-slot + (slot-width (:jvm-type param)))))
          {:slots {}
           :next-slot 1}
          params))

(defn- emit-int
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv (int (:value expr))))

(defn- emit-float
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv (float (:value expr))))

(defn- emit-double
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv (double (:value expr))))

(defn- emit-string
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv ^String (:value expr)))

(defn- emit-boolean
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv (boolean (:value expr))))

(defn- box-descriptor
  [jvm-type]
  (case jvm-type
    :int "(I)Ljava/lang/Integer;"
    :boolean "(Z)Ljava/lang/Boolean;"
    :float "(F)Ljava/lang/Float;"
    :double "(D)Ljava/lang/Double;"))

(defn- emit-box-if-needed
  [^MethodVisitor mv actual-jvm-type runtime-jvm-type]
  (when (and (= "java/lang/Object" runtime-jvm-type)
             (contains? boxed-types actual-jvm-type))
    (.visitMethodInsn mv
                      Opcodes/INVOKESTATIC
                      (get boxed-types actual-jvm-type)
                      "valueOf"
                      (box-descriptor actual-jvm-type)
                      false)))

(defn- emit-primitive-unbox
  [^MethodVisitor mv boxed-type method-name descriptor]
  (.visitTypeInsn mv Opcodes/CHECKCAST boxed-type)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    boxed-type
                    method-name
                    descriptor
                    false))

(defn- emit-object-cast-or-unbox
  [^MethodVisitor mv actual-jvm-type]
  (case actual-jvm-type
    :int (emit-primitive-unbox mv "java/lang/Integer" "intValue" "()I")
    :boolean (emit-primitive-unbox mv "java/lang/Boolean" "booleanValue" "()Z")
    :float (emit-primitive-unbox mv "java/lang/Float" "floatValue" "()F")
    :double (emit-primitive-unbox mv "java/lang/Double" "doubleValue" "()D")
    (when (string? actual-jvm-type)
      (.visitTypeInsn mv Opcodes/CHECKCAST actual-jvm-type))))

(defn- emit-cast-or-unbox
  [^MethodVisitor mv runtime-jvm-type actual-jvm-type]
  (when-not (= runtime-jvm-type actual-jvm-type)
    (if (= "java/lang/Object" runtime-jvm-type)
      (emit-object-cast-or-unbox mv actual-jvm-type)
      (when (string? actual-jvm-type)
        (.visitTypeInsn mv Opcodes/CHECKCAST actual-jvm-type)))))

(defn- emit-local
  [^MethodVisitor mv expr env]
  (let [{:keys [slot jvm-type]} (or (get-in env [:slots (:name expr)])
                                    (fail! "Unknown emitted local."
                                           {:name (:name expr)}))]
    (.visitVarInsn mv (load-opcode jvm-type) slot)))

(defn- emit-invoke-static
  [^MethodVisitor mv expr env]
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    (:owner expr)
                    (method-name (:name expr))
                    (str "("
                         (apply str (map descriptor (:parameter-types expr)))
                         ")"
                         (descriptor (:return-type expr)))
                    false))

(defn- emit-java-static-call
  [^MethodVisitor mv expr env]
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    (:class-name expr)
                    (str (:member-id expr))
                    (str "("
                         (apply str (map descriptor (:parameter-types expr)))
                         ")"
                    (descriptor (:return-type expr)))
                    false))

(defn- emit-java-call
  [^MethodVisitor mv expr env]
  (emit-expr mv (:target expr) env)
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    (:jvm-type (:target expr))
                    (str (:member-id expr))
                    (str "("
                         (apply str (map descriptor (:parameter-types expr)))
                         ")"
                         (descriptor (:return-type expr)))
                    false))

(defn- emit-java-new
  [^MethodVisitor mv expr env]
  (.visitTypeInsn mv Opcodes/NEW (:class-name expr))
  (.visitInsn mv Opcodes/DUP)
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    (:class-name expr)
                    "<init>"
                    (str "("
                         (apply str (map descriptor (:parameter-types expr)))
                         ")V")
                    false))

(defn- emit-if
  [^MethodVisitor mv expr env]
  (let [else-label (Label.)
        end-label (Label.)]
    (emit-expr mv (:test expr) env)
    (.visitJumpInsn mv Opcodes/IFEQ else-label)
    (emit-expr mv (:then expr) env)
    (.visitJumpInsn mv Opcodes/GOTO end-label)
    (.visitLabel mv else-label)
    (emit-expr mv (:else expr) env)
    (.visitLabel mv end-label)))

(defn- emit-primitive-binary
  [^MethodVisitor mv expr env opcode]
  (doseq [arg (:args expr)]
    (emit-expr mv arg env))
  (.visitInsn mv opcode))

(defn- emit-primitive-comparison
  [^MethodVisitor mv expr env jump-opcode]
  (let [true-label (Label.)
        end-label (Label.)]
    (emit-expr mv (first (:args expr)) env)
    (emit-expr mv (second (:args expr)) env)
    (.visitJumpInsn mv jump-opcode true-label)
    (.visitLdcInsn mv false)
    (.visitJumpInsn mv Opcodes/GOTO end-label)
    (.visitLabel mv true-label)
    (.visitLdcInsn mv true)
    (.visitLabel mv end-label)))

(defn- emit-floating-comparison
  [^MethodVisitor mv expr env compare-opcode jump-opcode]
  (let [true-label (Label.)
        end-label (Label.)]
    (emit-expr mv (first (:args expr)) env)
    (emit-expr mv (second (:args expr)) env)
    (.visitInsn mv compare-opcode)
    (.visitJumpInsn mv jump-opcode true-label)
    (.visitLdcInsn mv false)
    (.visitJumpInsn mv Opcodes/GOTO end-label)
    (.visitLabel mv true-label)
    (.visitLdcInsn mv true)
    (.visitLabel mv end-label)))

(defn- emit-primitive-conversion
  [^MethodVisitor mv expr env opcode]
  (emit-expr mv (:arg expr) env)
  (.visitInsn mv opcode))

(defn- emit-bool-not
  [^MethodVisitor mv expr env]
  (emit-expr mv (:arg expr) env)
  (.visitInsn mv Opcodes/ICONST_1)
  (.visitInsn mv Opcodes/IXOR))

(defn- pop-opcode
  [jvm-type]
  (case jvm-type
    :void nil
    :double Opcodes/POP2
    Opcodes/POP))

(defn- emit-discarded
  [^MethodVisitor mv expr env]
  (emit-expr mv expr env)
  (when-let [opcode (pop-opcode (:jvm-type expr))]
    (.visitInsn mv opcode)))

(defn- emit-seq
  [^MethodVisitor mv expr env]
  (let [[_ final-env]
        (reduce (fn [[_ current-env] subexpr]
                  (emit-expr mv subexpr current-env)
                  (when-let [opcode (pop-opcode (:jvm-type subexpr))]
                    (.visitInsn mv opcode))
                  [nil
                   (if (= :jvm-var (:op subexpr))
                     (bind-var-slot current-env subexpr)
                     current-env)])
                [nil env]
                (butlast (:exprs expr)))]
    (emit-expr mv (last (:exprs expr)) final-env)))

(defn- bind-let-slots
  [env bindings]
  (reduce (fn [acc binding]
            (assoc-in (update acc :next-slot + (slot-width (:jvm-type (:expr binding))))
                      [:slots (:name binding)]
                      {:slot (:next-slot acc)
                       :jvm-type (:jvm-type (:expr binding))}))
          env
          bindings))

(defn- bind-typed-slots
  [env bindings type-key]
  (reduce (fn [acc binding]
            (assoc-in (update acc :next-slot + (slot-width (get binding type-key)))
                      [:slots (:name binding)]
                      {:slot (:next-slot acc)
                       :jvm-type (get binding type-key)}))
          env
          bindings))

(defn- bind-var-slot
  [env expr]
  (let [jvm-type (or (:cell-jvm-type expr)
                     (:jvm-type (:init expr)))]
    (assoc-in (update env :next-slot + (slot-width jvm-type))
            [:slots (:name expr)]
            {:slot (:next-slot env)
             :jvm-type jvm-type})))

(defn- emit-var
  [^MethodVisitor mv expr env]
  (let [body-env (bind-var-slot env expr)
        {:keys [slot jvm-type]} (get-in body-env [:slots (:name expr)])]
    (if (:cell-jvm-type expr)
      (jvm-emit-cells/emit-cell-new mv expr env {:emit-expr emit-expr})
      (emit-expr mv (:init expr) env))
    (.visitVarInsn mv (store-opcode jvm-type) slot)))

(defn- emit-set
  [^MethodVisitor mv expr env]
  (if (:cell-jvm-type expr)
    (jvm-emit-cells/emit-cell-set mv expr env {:emit-expr emit-expr})
    (let [{:keys [slot jvm-type]} (or (get-in env [:slots (:name expr)])
                                      (fail! "Unknown emitted mutable local."
                                             {:name (:name expr)}))]
      (emit-expr mv (:expr expr) env)
      (.visitVarInsn mv (store-opcode jvm-type) slot))))

(defn- emit-let
  [^MethodVisitor mv expr env]
  (let [body-env (bind-let-slots env (:bindings expr))]
    (doseq [binding (:bindings expr)]
      (emit-binding mv binding body-env))
    (emit-expr mv (:body expr) body-env)))

(defn- constructor-descriptor
  [args]
  (str "("
       (apply str (map (comp descriptor :jvm-type) args))
       ")V"))

(defn- constructor-descriptor-for-types
  [jvm-types]
  (str "("
       (apply str (map descriptor jvm-types))
       ")V"))

(defn- emit-construct
  [^MethodVisitor mv expr env]
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

(defn- emit-record-get
  [^MethodVisitor mv expr env]
  (let [field-jvm-type (or (:field-jvm-type expr) (:jvm-type expr))]
    (emit-expr mv (:target expr) env)
    (.visitFieldInsn mv
                     Opcodes/GETFIELD
                     (:jvm-type (:target expr))
                     (str (:field expr))
                     (descriptor field-jvm-type))
    (emit-cast-or-unbox mv field-jvm-type (:jvm-type expr))))

(defn- emit-java-get-field
  [^MethodVisitor mv expr env]
  (emit-expr mv (:target expr) env)
  (.visitFieldInsn mv
                   Opcodes/GETFIELD
                   (:jvm-type (:target expr))
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn- emit-java-static-get-field
  [^MethodVisitor mv expr]
  (.visitFieldInsn mv
                   Opcodes/GETSTATIC
                   (:class-name expr)
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn- emit-java-set-field
  [^MethodVisitor mv expr env]
  (emit-expr mv (:target expr) env)
  (emit-expr mv (:expr expr) env)
  (.visitFieldInsn mv
                   Opcodes/PUTFIELD
                   (:jvm-type (:target expr))
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn- emit-java-static-set-field
  [^MethodVisitor mv expr env]
  (emit-expr mv (:expr expr) env)
  (.visitFieldInsn mv
                   Opcodes/PUTSTATIC
                   (:class-name expr)
                   (str (:field-name expr))
                   (descriptor (:field-type expr))))

(defn- emit-variant
  [^MethodVisitor mv expr env]
  (emit-construct mv expr env))

(defn- emit-instance-of
  [^MethodVisitor mv expr env]
  (emit-expr mv (:target expr) env)
  (.visitTypeInsn mv Opcodes/INSTANCEOF (:class-name expr)))

(defn- emit-variant-field
  [^MethodVisitor mv expr env]
  (let [field-jvm-type (or (:field-jvm-type expr) (:jvm-type expr))]
    (emit-expr mv (:target expr) env)
    (.visitTypeInsn mv Opcodes/CHECKCAST (:class-name expr))
    (.visitFieldInsn mv
                     Opcodes/GETFIELD
                     (:class-name expr)
                     (str (:field expr))
                     (descriptor field-jvm-type))
    (emit-cast-or-unbox mv field-jvm-type (:jvm-type expr))))

(defn- emit-literal-test
  [^MethodVisitor mv expr env]
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

(defn- emit-always-true
  [^MethodVisitor mv]
  (.visitInsn mv Opcodes/ICONST_1))

(defn- bind-local-slots
  [env bindings]
  (reduce (fn [acc binding]
            (assoc-in (update acc :next-slot inc)
                      [:slots (:name binding)]
                      {:slot (:next-slot acc)
                       :jvm-type (:jvm-type (:expr binding))}))
          env
          bindings))

(defn- bind-loop-slots
  [env bindings]
  (bind-typed-slots env bindings :jvm-type))

(defn- emit-binding
  [^MethodVisitor mv binding env]
  (let [{:keys [slot jvm-type]} (get-in env [:slots (:name binding)])]
    (emit-expr mv (:expr binding) env)
    (.visitVarInsn mv (store-opcode jvm-type) slot)))

(defn- emit-match
  [^MethodVisitor mv expr env]
  (let [end-label (Label.)
        case-labels (repeatedly (count (:cases expr)) #(Label.))
        next-labels (concat (rest case-labels) [nil])]
    (doseq [[[case current-label] next-label] (map vector (map vector (:cases expr) case-labels) next-labels)]
      (.visitLabel mv current-label)
      (emit-expr mv (:test case) env)
      (when next-label
        (.visitJumpInsn mv Opcodes/IFEQ next-label))
      (when-not next-label
        (.visitInsn mv Opcodes/POP))
      (let [body-env (bind-local-slots env (:bindings case))]
        (doseq [binding (:bindings case)]
          (emit-binding mv binding body-env))
        (emit-expr mv (:body case) body-env))
      (.visitJumpInsn mv Opcodes/GOTO end-label))
    (.visitLabel mv end-label)))

(defn- recur-frame
  [env]
  (or (peek (:loop-stack env))
      (fail! "Recur emitted outside loop."
             {:env env})))

(defn- temp-slots
  [env args]
  (reduce (fn [{:keys [slots next-slot]} arg]
            {:slots (conj slots {:slot next-slot
                                 :jvm-type (:jvm-type arg)})
             :next-slot (+ next-slot (slot-width (:jvm-type arg)))})
          {:slots []
           :next-slot (:next-slot env)}
          args))

(defn- emit-recur
  [^MethodVisitor mv expr env]
  (let [{:keys [start-label bindings]} (recur-frame env)
        {:keys [slots]} (temp-slots env (:args expr))]
    (doseq [[arg {:keys [slot jvm-type]}] (map vector (:args expr) slots)]
      (emit-expr mv arg env)
      (.visitVarInsn mv (store-opcode jvm-type) slot))
    (doseq [[{:keys [slot jvm-type]} {:keys [binding-slot]}]
            (map vector
                 slots
                 (map (fn [binding]
                        {:binding-slot (:slot binding)
                         :jvm-type (:jvm-type binding)})
                      bindings))]
      (.visitVarInsn mv (load-opcode jvm-type) slot)
      (.visitVarInsn mv (store-opcode jvm-type) binding-slot))
    (.visitJumpInsn mv Opcodes/GOTO start-label)))

(defn- temp-slot
  [env jvm-type]
  {:slot (:next-slot env)
   :jvm-type jvm-type
   :env (update env :next-slot + (slot-width jvm-type))})

(defn- emit-raise
  [^MethodVisitor mv expr env]
  (emit-expr mv (:expr expr) env)
  (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Throwable")
  (.visitInsn mv Opcodes/ATHROW))

(defn- emit-try
  [^MethodVisitor mv expr env]
  (jvm-emit-try/emit-try mv
                         expr
                         env
                         {:emit-expr emit-expr
                          :emit-discarded emit-discarded
                          :store-opcode store-opcode
                          :load-opcode load-opcode
                          :temp-slot temp-slot}))

(defn- emit-loop
  [^MethodVisitor mv expr env]
  (let [start-label (Label.)
        loop-env (bind-loop-slots env (:bindings expr))
        binding-state (mapv (fn [binding]
                              (merge binding
                                     (get-in loop-env [:slots (:name binding)])))
                            (:bindings expr))
        body-env (update loop-env :loop-stack (fnil conj []) {:start-label start-label
                                                              :bindings binding-state})]
    (reduce (fn [current-env binding]
              (emit-expr mv (:init binding) current-env)
              (.visitVarInsn mv
                             (store-opcode (:jvm-type binding))
                             (:slot binding))
              (assoc-in current-env
                        [:slots (:name binding)]
                        {:slot (:slot binding)
                         :jvm-type (:jvm-type binding)}))
            env
            binding-state)
    (.visitLabel mv start-label)
    (emit-expr mv (:body expr) body-env)))

(def ^:private literal-emitters
  {:jvm-int emit-int
   :jvm-float emit-float
   :jvm-double emit-double
   :jvm-string emit-string
   :jvm-boolean emit-boolean})

(defn- emit-literal
  [^MethodVisitor mv expr]
  (when-let [emit-literal* (get literal-emitters (:op expr))]
    (emit-literal* mv expr)
    true))

(def ^:private emit-expr-handlers
  {:jvm-local emit-local
   :jvm-invoke-static emit-invoke-static
   :jvm-int-add (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IADD))
   :jvm-int-sub (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/ISUB))
   :jvm-int-mul (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IMUL))
   :jvm-int-div (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IDIV))
   :jvm-int-mod (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IREM))
   :jvm-int-eq (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPEQ))
   :jvm-int-lt (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPLT))
   :jvm-int-le (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPLE))
   :jvm-int-gt (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPGT))
   :jvm-int-ge (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPGE))
   :jvm-float-add (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/FADD))
   :jvm-float-sub (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/FSUB))
   :jvm-float-mul (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/FMUL))
   :jvm-float-div (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/FDIV))
   :jvm-float-eq (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/FCMPL Opcodes/IFEQ))
   :jvm-float-lt (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/FCMPG Opcodes/IFLT))
   :jvm-float-le (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/FCMPG Opcodes/IFLE))
   :jvm-float-gt (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/FCMPL Opcodes/IFGT))
   :jvm-float-ge (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/FCMPL Opcodes/IFGE))
   :jvm-double-add (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/DADD))
   :jvm-double-sub (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/DSUB))
   :jvm-double-mul (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/DMUL))
   :jvm-double-div (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/DDIV))
   :jvm-double-eq (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/DCMPL Opcodes/IFEQ))
   :jvm-double-lt (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/DCMPG Opcodes/IFLT))
   :jvm-double-le (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/DCMPG Opcodes/IFLE))
   :jvm-double-gt (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/DCMPL Opcodes/IFGT))
   :jvm-double-ge (fn [mv expr env] (emit-floating-comparison mv expr env Opcodes/DCMPL Opcodes/IFGE))
   :jvm-bool-eq (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPEQ))
   :jvm-int-ne (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPNE))
   :jvm-string-eq (fn [mv expr env]
                    (jvm-emit-text/emit-string-eq mv expr env {:emit-expr emit-expr}))
   :jvm-string-concat (fn [mv expr env]
                        (jvm-emit-text/emit-string-concat mv expr env {:emit-expr emit-expr}))
   :jvm-string-split-on (fn [mv expr env]
                          (jvm-emit-text/emit-string-split-on mv expr env {:emit-expr emit-expr}))
   :jvm-string-char-at (fn [mv expr env]
                         (jvm-emit-text/emit-string-char-at mv expr env {:emit-expr emit-expr}))
   :jvm-string-substring (fn [mv expr env]
                           (jvm-emit-text/emit-string-substring mv expr env {:emit-expr emit-expr}))
   :jvm-int->string (fn [mv expr env]
                      (jvm-emit-text/emit-int->string mv expr env {:emit-expr emit-expr}))
   :jvm-int->float (fn [mv expr env] (emit-primitive-conversion mv expr env Opcodes/I2F))
   :jvm-int->double (fn [mv expr env] (emit-primitive-conversion mv expr env Opcodes/I2D))
   :jvm-float->double (fn [mv expr env] (emit-primitive-conversion mv expr env Opcodes/F2D))
   :jvm-double->float (fn [mv expr env] (emit-primitive-conversion mv expr env Opcodes/D2F))
   :jvm-string->int (fn [mv expr env]
                      (jvm-emit-text/emit-string->int mv expr env {:emit-expr emit-expr}))
   :jvm-string-length (fn [mv expr env]
                        (jvm-emit-text/emit-string-length mv expr env {:emit-expr emit-expr}))
   :jvm-string-trim (fn [mv expr env]
                      (jvm-emit-text/emit-string-trim mv expr env {:emit-expr emit-expr}))
   :jvm-string-empty (fn [mv expr env]
                       (jvm-emit-text/emit-string-empty mv expr env {:emit-expr emit-expr}))
   :jvm-seq-empty (fn [mv expr env]
                    (jvm-emit-text/emit-seq-empty mv expr env {:emit-expr emit-expr}))
   :jvm-seq-length (fn [mv expr env]
                     (jvm-emit-text/emit-seq-length mv expr env {:emit-expr emit-expr}))
   :jvm-seq-first (fn [mv expr env]
                    (jvm-emit-text/emit-seq-first mv expr env {:emit-expr emit-expr}))
   :jvm-seq-get (fn [mv expr env]
                  (jvm-emit-text/emit-seq-get mv expr env {:emit-expr emit-expr}))
   :jvm-io-read-line (fn [mv expr env]
                       (jvm-emit-text/emit-io-read-line mv expr env nil))
   :jvm-io-print (fn [mv expr env]
                   (jvm-emit-text/emit-io-print mv expr env
                                                {:emit-expr emit-expr
                                                 :method-name "print"}))
   :jvm-bool-not emit-bool-not
   :jvm-bool-and (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IAND))
   :jvm-bool-or (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IOR))
   :jvm-io-println (fn [mv expr env]
                     (jvm-emit-text/emit-io-print mv expr env
                                                  {:emit-expr emit-expr
                                                   :method-name "println"}))
   :jvm-java-call emit-java-call
   :jvm-java-static-call emit-java-static-call
   :jvm-java-new emit-java-new
   :jvm-java-get-field emit-java-get-field
   :jvm-java-set-field emit-java-set-field
   :jvm-java-static-get-field (fn [mv expr _env] (emit-java-static-get-field mv expr))
   :jvm-java-static-set-field emit-java-static-set-field
   :jvm-closure-new (fn [mv expr env]
                      (jvm-emit-closures/emit-closure-new
                       mv
                       expr
                       env
                       {:emit-expr emit-expr
                        :constructor-descriptor constructor-descriptor}))
   :jvm-closure-call (fn [mv expr env]
                       (jvm-emit-closures/emit-closure-call
                        mv
                        expr
                        env
                        {:emit-expr emit-expr
                         :descriptor descriptor}))
   :jvm-cell-get (fn [mv expr env]
                   (jvm-emit-cells/emit-cell-get mv expr env))
   :jvm-if emit-if
   :jvm-seq emit-seq
   :jvm-let emit-let
   :jvm-var emit-var
   :jvm-set emit-set
   :jvm-loop emit-loop
   :jvm-recur emit-recur
   :jvm-try emit-try
   :jvm-raise emit-raise
   :jvm-construct emit-construct
   :jvm-record-get emit-record-get
   :jvm-variant emit-variant
   :jvm-instance-of emit-instance-of
   :jvm-variant-field emit-variant-field
   :jvm-literal-test emit-literal-test
   :jvm-always-true (fn [mv _ _] (emit-always-true mv))
   :jvm-match emit-match})

(defn- emit-expr
  [^MethodVisitor mv expr env]
  (or (emit-literal mv expr)
      (if-let [handler (get emit-expr-handlers (:op expr))]
        (handler mv expr env)
        (fail! "Unsupported JVM emission expression."
               {:expr expr}))))

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

(defn- field-slot
  [fields]
  (map-indexed (fn [index field]
                 [(inc index) field])
               fields))

(defn- emit-record-constructor
  [^ClassWriter cw record]
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
  [module record]
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
    (emit-record-constructor cw record)
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
  [module union variant]
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
  [^ClassWriter cw method]
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
    (.visitInsn mv (return-opcode (:return-type method)))
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-instance-method
  [^ClassWriter cw method]
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

(defn emit-module-bytes
  [module]
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
      (emit-method cw method))
    (doseq [method (:instance-methods module)]
      (emit-instance-method cw method))
    (jvm-emit-entry/emit-main-wrapper cw module
                                      {:method-name method-name
                                       :method-descriptor method-descriptor
                                       :pop-opcode pop-opcode})
    (.visitEnd cw)
    (.toByteArray cw)))

(defn emit-class-bytes
  [module]
  (merge {(:internal-name module) (emit-module-bytes module)}
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
                      [(:class-name record) (emit-record-class-bytes module record)]))
               (:records module))
         (into {}
               (mapcat
                (fn [union]
                  (concat [[(:base-class union) (emit-union-base-class-bytes module union)]]
                          (map (fn [variant]
                                 [(:class-name variant)
                                  (emit-union-variant-class-bytes module union variant)])
                               (:variants union))))
               (:unions module)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T06:55:18.94259-05:00", :module-hash "-1382625486", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 9, :hash "716573190"} {:id "form/1/declare", :kind "declare", :line 11, :end-line 11, :hash "-351869103"} {:id "form/2/declare", :kind "declare", :line 12, :end-line 12, :hash "1069961042"} {:id "form/3/declare", :kind "declare", :line 13, :end-line 13, :hash "-2053214757"} {:id "form/4/declare", :kind "declare", :line 14, :end-line 14, :hash "2098266977"} {:id "form/5/declare", :kind "declare", :line 15, :end-line 15, :hash "963731763"} {:id "form/6/declare", :kind "declare", :line 16, :end-line 16, :hash "-1905695466"} {:id "def/type-descriptors", :kind "def", :line 18, :end-line 23, :hash "-1275703727"} {:id "def/load-opcodes", :kind "def", :line 25, :end-line 29, :hash "700860168"} {:id "def/store-opcodes", :kind "def", :line 31, :end-line 35, :hash "656288957"} {:id "def/return-opcodes", :kind "def", :line 37, :end-line 42, :hash "1446178414"} {:id "def/boxed-types", :kind "def", :line 44, :end-line 48, :hash "-1247089229"} {:id "defn-/fail!", :kind "defn-", :line 50, :end-line 52, :hash "879938479"} {:id "defn-/method-name", :kind "defn-", :line 54, :end-line 58, :hash "1072728380"} {:id "defn-/init-descriptor", :kind "defn-", :line 60, :end-line 64, :hash "-591391110"} {:id "defn-/descriptor", :kind "defn-", :line 66, :end-line 68, :hash "799112538"} {:id "defn-/method-descriptor", :kind "defn-", :line 70, :end-line 75, :hash "-1082052598"} {:id "defn-/load-opcode", :kind "defn-", :line 77, :end-line 79, :hash "501460056"} {:id "defn-/store-opcode", :kind "defn-", :line 81, :end-line 83, :hash "234024341"} {:id "defn-/return-opcode", :kind "defn-", :line 85, :end-line 87, :hash "1071593654"} {:id "defn-/slot-width", :kind "defn-", :line 89, :end-line 93, :hash "644765418"} {:id "defn-/local-slots", :kind "defn-", :line 95, :end-line 105, :hash "1286710434"} {:id "defn-/instance-local-slots", :kind "defn-", :line 107, :end-line 117, :hash "-234993241"} {:id "defn-/emit-int", :kind "defn-", :line 119, :end-line 121, :hash "682918505"} {:id "defn-/emit-float", :kind "defn-", :line 123, :end-line 125, :hash "-404581045"} {:id "defn-/emit-double", :kind "defn-", :line 127, :end-line 129, :hash "-299228739"} {:id "defn-/emit-string", :kind "defn-", :line 131, :end-line 133, :hash "-364387159"} {:id "defn-/emit-boolean", :kind "defn-", :line 135, :end-line 137, :hash "-818404463"} {:id "defn-/box-descriptor", :kind "defn-", :line 139, :end-line 145, :hash "1143194916"} {:id "defn-/emit-box-if-needed", :kind "defn-", :line 147, :end-line 156, :hash "-177508175"} {:id "defn-/emit-primitive-unbox", :kind "defn-", :line 158, :end-line 166, :hash "-1739292644"} {:id "defn-/emit-object-cast-or-unbox", :kind "defn-", :line 168, :end-line 176, :hash "472309751"} {:id "defn-/emit-cast-or-unbox", :kind "defn-", :line 178, :end-line 184, :hash "-1711668189"} {:id "defn-/emit-local", :kind "defn-", :line 186, :end-line 191, :hash "435457023"} {:id "defn-/emit-invoke-static", :kind "defn-", :line 193, :end-line 205, :hash "1482752229"} {:id "defn-/emit-java-static-call", :kind "defn-", :line 207, :end-line 219, :hash "-738935078"} {:id "defn-/emit-java-call", :kind "defn-", :line 221, :end-line 234, :hash "19941248"} {:id "defn-/emit-java-new", :kind "defn-", :line 236, :end-line 249, :hash "1581399000"} {:id "defn-/emit-if", :kind "defn-", :line 251, :end-line 261, :hash "-2016458095"} {:id "defn-/emit-primitive-binary", :kind "defn-", :line 263, :end-line 267, :hash "853246371"} {:id "defn-/emit-primitive-comparison", :kind "defn-", :line 269, :end-line 280, :hash "328881796"} {:id "defn-/emit-floating-comparison", :kind "defn-", :line 282, :end-line 294, :hash "-239382277"} {:id "defn-/emit-primitive-conversion", :kind "defn-", :line 296, :end-line 299, :hash "1990636722"} {:id "defn-/emit-bool-not", :kind "defn-", :line 301, :end-line 305, :hash "-644687160"} {:id "defn-/pop-opcode", :kind "defn-", :line 307, :end-line 312, :hash "391217719"} {:id "defn-/emit-discarded", :kind "defn-", :line 314, :end-line 318, :hash "-373415048"} {:id "defn-/emit-seq", :kind "defn-", :line 320, :end-line 333, :hash "1569148183"} {:id "defn-/bind-let-slots", :kind "defn-", :line 335, :end-line 343, :hash "-1742644809"} {:id "defn-/bind-typed-slots", :kind "defn-", :line 345, :end-line 353, :hash "-1647294951"} {:id "defn-/bind-var-slot", :kind "defn-", :line 355, :end-line 362, :hash "459677485"} {:id "defn-/emit-var", :kind "defn-", :line 364, :end-line 371, :hash "879920388"} {:id "defn-/emit-set", :kind "defn-", :line 373, :end-line 381, :hash "1326112789"} {:id "defn-/emit-let", :kind "defn-", :line 383, :end-line 388, :hash "-1597708766"} {:id "defn-/constructor-descriptor", :kind "defn-", :line 390, :end-line 394, :hash "1030246641"} {:id "defn-/constructor-descriptor-for-types", :kind "defn-", :line 396, :end-line 400, :hash "410465"} {:id "defn-/emit-construct", :kind "defn-", :line 402, :end-line 416, :hash "1569475595"} {:id "defn-/emit-record-get", :kind "defn-", :line 418, :end-line 427, :hash "1771867957"} {:id "defn-/emit-java-get-field", :kind "defn-", :line 429, :end-line 436, :hash "1574815784"} {:id "defn-/emit-java-static-get-field", :kind "defn-", :line 438, :end-line 444, :hash "-1082357157"} {:id "defn-/emit-java-set-field", :kind "defn-", :line 446, :end-line 454, :hash "-551071161"} {:id "defn-/emit-java-static-set-field", :kind "defn-", :line 456, :end-line 463, :hash "1083253080"} {:id "defn-/emit-variant", :kind "defn-", :line 465, :end-line 467, :hash "897327167"} {:id "defn-/emit-instance-of", :kind "defn-", :line 469, :end-line 472, :hash "1555891405"} {:id "defn-/emit-variant-field", :kind "defn-", :line 474, :end-line 484, :hash "1844987982"} {:id "defn-/emit-literal-test", :kind "defn-", :line 486, :end-line 497, :hash "1011164584"} {:id "defn-/emit-always-true", :kind "defn-", :line 499, :end-line 501, :hash "415683512"} {:id "defn-/bind-local-slots", :kind "defn-", :line 503, :end-line 511, :hash "844729401"} {:id "defn-/bind-loop-slots", :kind "defn-", :line 513, :end-line 515, :hash "-1190078791"} {:id "defn-/emit-binding", :kind "defn-", :line 517, :end-line 521, :hash "-167414695"} {:id "defn-/emit-match", :kind "defn-", :line 523, :end-line 540, :hash "1586201505"} {:id "defn-/recur-frame", :kind "defn-", :line 542, :end-line 546, :hash "-1921183736"} {:id "defn-/temp-slots", :kind "defn-", :line 548, :end-line 556, :hash "1830022320"} {:id "defn-/emit-recur", :kind "defn-", :line 558, :end-line 574, :hash "-640909028"} {:id "defn-/temp-slot", :kind "defn-", :line 576, :end-line 580, :hash "1829218103"} {:id "defn-/emit-raise", :kind "defn-", :line 582, :end-line 586, :hash "476518000"} {:id "defn-/emit-try", :kind "defn-", :line 588, :end-line 597, :hash "1119462693"} {:id "defn-/emit-loop", :kind "defn-", :line 599, :end-line 621, :hash "193454205"} {:id "def/literal-emitters", :kind "def", :line 623, :end-line 628, :hash "1324913079"} {:id "defn-/emit-literal", :kind "defn-", :line 630, :end-line 634, :hash "-1669064898"} {:id "def/emit-expr-handlers", :kind "def", :line 636, :end-line 753, :hash "-1094434468"} {:id "defn-/emit-expr", :kind "defn-", :line 755, :end-line 761, :hash "749449684"} {:id "defn-/emit-default-constructor", :kind "defn-", :line 763, :end-line 773, :hash "240160418"} {:id "defn-/field-slot", :kind "defn-", :line 775, :end-line 779, :hash "-1567189554"} {:id "defn-/emit-record-constructor", :kind "defn-", :line 781, :end-line 797, :hash "-316497366"} {:id "defn-/emit-record-class-bytes", :kind "defn-", :line 799, :end-line 819, :hash "-382850934"} {:id "defn-/emit-union-base-class-bytes", :kind "defn-", :line 821, :end-line 833, :hash "861277052"} {:id "defn-/emit-union-variant-class-bytes", :kind "defn-", :line 835, :end-line 868, :hash "944748988"} {:id "defn-/emit-method", :kind "defn-", :line 870, :end-line 884, :hash "-1558709303"} {:id "defn-/emit-instance-method", :kind "defn-", :line 886, :end-line 910, :hash "-128623171"} {:id "defn/emit-module-bytes", :kind "defn", :line 912, :end-line 934, :hash "582395298"} {:id "defn/emit-class-bytes", :kind "defn", :line 936, :end-line 978, :hash "-1403240972"}]}
;; clj-mutate-manifest-end
