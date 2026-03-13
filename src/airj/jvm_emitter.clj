(ns airj.jvm-emitter
  (:require [airj.jvm-cells :as jvm-cells]
            [airj.jvm-class-writer :as jvm-class-writer]
            [airj.jvm-emit-cells :as jvm-emit-cells]
            [airj.jvm-emit-closures :as jvm-emit-closures]
            [airj.jvm-emit-entry :as jvm-emit-entry]
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
   :boolean "Z"
   :void "V"})

(def ^:private load-opcodes
  {:int Opcodes/ILOAD
   :boolean Opcodes/ILOAD})

(def ^:private store-opcodes
  {:int Opcodes/ISTORE
   :boolean Opcodes/ISTORE})

(def ^:private return-opcodes
  {:int Opcodes/IRETURN
   :boolean Opcodes/IRETURN
   :void Opcodes/RETURN})

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

(defn- local-slots
  [params]
  {:slots (into {}
                (map-indexed (fn [index param]
                               [(:name param) {:slot index
                                               :jvm-type (:jvm-type param)}])
                             params))
   :next-slot (count params)})

(defn- instance-local-slots
  [params]
  {:slots (into {}
                (map-indexed (fn [index param]
                               [(:name param) {:slot (inc index)
                                               :jvm-type (:jvm-type param)}])
                             params))
   :next-slot (inc (count params))})

(defn- emit-int
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv (int (:value expr))))

(defn- emit-string
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv ^String (:value expr)))

(defn- emit-boolean
  [^MethodVisitor mv expr]
  (.visitLdcInsn mv (boolean (:value expr))))

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

(defn- emit-bool-not
  [^MethodVisitor mv expr env]
  (emit-expr mv (:arg expr) env)
  (.visitInsn mv Opcodes/ICONST_1)
  (.visitInsn mv Opcodes/IXOR))

(defn- pop-opcode
  [jvm-type]
  (case jvm-type
    :void nil
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
            (assoc-in (update acc :next-slot inc)
                      [:slots (:name binding)]
                      {:slot (:next-slot acc)
                       :jvm-type (:jvm-type (:expr binding))}))
          env
          bindings))

(defn- bind-typed-slots
  [env bindings type-key]
  (reduce (fn [acc binding]
            (assoc-in (update acc :next-slot inc)
                      [:slots (:name binding)]
                      {:slot (:next-slot acc)
                       :jvm-type (get binding type-key)}))
          env
          bindings))

(defn- bind-var-slot
  [env expr]
  (assoc-in (update env :next-slot inc)
            [:slots (:name expr)]
            {:slot (:next-slot env)
             :jvm-type (or (:cell-jvm-type expr)
                           (:jvm-type (:init expr)))}))

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

(defn- emit-construct
  [^MethodVisitor mv expr env]
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

(defn- emit-record-get
  [^MethodVisitor mv expr env]
  (emit-expr mv (:target expr) env)
  (.visitFieldInsn mv
                   Opcodes/GETFIELD
                   (:jvm-type (:target expr))
                   (str (:field expr))
                   (descriptor (:jvm-type expr))))

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
  (emit-expr mv (:target expr) env)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:class-name expr))
  (.visitFieldInsn mv
                   Opcodes/GETFIELD
                   (:class-name expr)
                   (str (:field expr))
                   (descriptor (:jvm-type expr))))

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
             :next-slot (inc next-slot)})
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
   :env (update env :next-slot inc)})

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

(defn- emit-literal
  [^MethodVisitor mv expr]
  (when (contains? #{:jvm-int :jvm-string :jvm-boolean} (:op expr))
    (case (:op expr)
      :jvm-int (emit-int mv expr)
      :jvm-string (emit-string mv expr)
      :jvm-boolean (emit-boolean mv expr))
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
   :jvm-bool-eq (fn [mv expr env] (emit-primitive-comparison mv expr env Opcodes/IF_ICMPEQ))
   :jvm-bool-not emit-bool-not
   :jvm-bool-and (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IAND))
   :jvm-bool-or (fn [mv expr env] (emit-primitive-binary mv expr env Opcodes/IOR))
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

(defn emit-module-bytes
  [module]
  (let [^ClassWriter cw (jvm-class-writer/class-writer module)]
    (.visit cw
            Opcodes/V1_8
            (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
            (:internal-name module)
            nil
            "java/lang/Object"
            nil)
    (doseq [method (:methods module)]
      (emit-method cw method))
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
;; {:version 1, :tested-at "2026-03-13T09:30:10.326797-05:00", :module-hash "-874513435", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 8, :hash "-1034326435"} {:id "form/1/declare", :kind "declare", :line 10, :end-line 10, :hash "-351869103"} {:id "form/2/declare", :kind "declare", :line 11, :end-line 11, :hash "1069961042"} {:id "form/3/declare", :kind "declare", :line 12, :end-line 12, :hash "-2053214757"} {:id "form/4/declare", :kind "declare", :line 13, :end-line 13, :hash "2098266977"} {:id "form/5/declare", :kind "declare", :line 14, :end-line 14, :hash "963731763"} {:id "form/6/declare", :kind "declare", :line 15, :end-line 15, :hash "-1905695466"} {:id "def/type-descriptors", :kind "def", :line 17, :end-line 20, :hash "-1455056336"} {:id "def/load-opcodes", :kind "def", :line 22, :end-line 24, :hash "-950703340"} {:id "def/store-opcodes", :kind "def", :line 26, :end-line 28, :hash "-997842127"} {:id "def/return-opcodes", :kind "def", :line 30, :end-line 33, :hash "1506270588"} {:id "defn-/fail!", :kind "defn-", :line 35, :end-line 37, :hash "879938479"} {:id "defn-/method-name", :kind "defn-", :line 39, :end-line 43, :hash "1072728380"} {:id "defn-/init-descriptor", :kind "defn-", :line 45, :end-line 49, :hash "-591391110"} {:id "defn-/descriptor", :kind "defn-", :line 51, :end-line 53, :hash "799112538"} {:id "defn-/method-descriptor", :kind "defn-", :line 55, :end-line 60, :hash "-1082052598"} {:id "defn-/load-opcode", :kind "defn-", :line 62, :end-line 64, :hash "501460056"} {:id "defn-/store-opcode", :kind "defn-", :line 66, :end-line 68, :hash "234024341"} {:id "defn-/return-opcode", :kind "defn-", :line 70, :end-line 72, :hash "1071593654"} {:id "defn-/local-slots", :kind "defn-", :line 74, :end-line 81, :hash "-1480289965"} {:id "defn-/instance-local-slots", :kind "defn-", :line 83, :end-line 90, :hash "-1377007891"} {:id "defn-/emit-int", :kind "defn-", :line 92, :end-line 94, :hash "682918505"} {:id "defn-/emit-string", :kind "defn-", :line 96, :end-line 98, :hash "-364387159"} {:id "defn-/emit-boolean", :kind "defn-", :line 100, :end-line 102, :hash "-818404463"} {:id "defn-/emit-local", :kind "defn-", :line 104, :end-line 109, :hash "435457023"} {:id "defn-/emit-invoke-static", :kind "defn-", :line 111, :end-line 123, :hash "1482752229"} {:id "defn-/emit-java-static-call", :kind "defn-", :line 125, :end-line 137, :hash "-738935078"} {:id "defn-/emit-java-call", :kind "defn-", :line 139, :end-line 152, :hash "19941248"} {:id "defn-/emit-java-new", :kind "defn-", :line 154, :end-line 167, :hash "1581399000"} {:id "defn-/emit-if", :kind "defn-", :line 169, :end-line 179, :hash "-2016458095"} {:id "defn-/emit-primitive-binary", :kind "defn-", :line 181, :end-line 185, :hash "853246371"} {:id "defn-/emit-primitive-comparison", :kind "defn-", :line 187, :end-line 198, :hash "328881796"} {:id "defn-/emit-bool-not", :kind "defn-", :line 200, :end-line 204, :hash "-644687160"} {:id "defn-/pop-opcode", :kind "defn-", :line 206, :end-line 210, :hash "-1145579462"} {:id "defn-/emit-discarded", :kind "defn-", :line 212, :end-line 216, :hash "-373415048"} {:id "defn-/emit-seq", :kind "defn-", :line 218, :end-line 231, :hash "1569148183"} {:id "defn-/bind-let-slots", :kind "defn-", :line 233, :end-line 241, :hash "1114237883"} {:id "defn-/bind-typed-slots", :kind "defn-", :line 243, :end-line 251, :hash "-21831128"} {:id "defn-/bind-var-slot", :kind "defn-", :line 253, :end-line 259, :hash "-1136329942"} {:id "defn-/emit-var", :kind "defn-", :line 261, :end-line 268, :hash "879920388"} {:id "defn-/emit-set", :kind "defn-", :line 270, :end-line 278, :hash "1326112789"} {:id "defn-/emit-let", :kind "defn-", :line 280, :end-line 285, :hash "-1597708766"} {:id "defn-/constructor-descriptor", :kind "defn-", :line 287, :end-line 291, :hash "1030246641"} {:id "defn-/emit-construct", :kind "defn-", :line 293, :end-line 304, :hash "642978394"} {:id "defn-/emit-record-get", :kind "defn-", :line 306, :end-line 313, :hash "-1829411674"} {:id "defn-/emit-java-get-field", :kind "defn-", :line 315, :end-line 322, :hash "1574815784"} {:id "defn-/emit-java-static-get-field", :kind "defn-", :line 324, :end-line 330, :hash "-1082357157"} {:id "defn-/emit-java-set-field", :kind "defn-", :line 332, :end-line 340, :hash "-551071161"} {:id "defn-/emit-java-static-set-field", :kind "defn-", :line 342, :end-line 349, :hash "1083253080"} {:id "defn-/emit-variant", :kind "defn-", :line 351, :end-line 353, :hash "897327167"} {:id "defn-/emit-instance-of", :kind "defn-", :line 355, :end-line 358, :hash "1555891405"} {:id "defn-/emit-variant-field", :kind "defn-", :line 360, :end-line 368, :hash "828168694"} {:id "defn-/emit-literal-test", :kind "defn-", :line 370, :end-line 381, :hash "1011164584"} {:id "defn-/emit-always-true", :kind "defn-", :line 383, :end-line 385, :hash "415683512"} {:id "defn-/bind-local-slots", :kind "defn-", :line 387, :end-line 395, :hash "844729401"} {:id "defn-/bind-loop-slots", :kind "defn-", :line 397, :end-line 399, :hash "-1190078791"} {:id "defn-/emit-binding", :kind "defn-", :line 401, :end-line 405, :hash "-167414695"} {:id "defn-/emit-match", :kind "defn-", :line 407, :end-line 424, :hash "1586201505"} {:id "defn-/recur-frame", :kind "defn-", :line 426, :end-line 430, :hash "-1921183736"} {:id "defn-/temp-slots", :kind "defn-", :line 432, :end-line 440, :hash "-1804676158"} {:id "defn-/emit-recur", :kind "defn-", :line 442, :end-line 458, :hash "-640909028"} {:id "defn-/temp-slot", :kind "defn-", :line 460, :end-line 464, :hash "-730167867"} {:id "defn-/emit-raise", :kind "defn-", :line 466, :end-line 470, :hash "476518000"} {:id "defn-/emit-try", :kind "defn-", :line 472, :end-line 481, :hash "1119462693"} {:id "defn-/emit-loop", :kind "defn-", :line 483, :end-line 505, :hash "193454205"} {:id "defn-/emit-literal", :kind "defn-", :line 507, :end-line 514, :hash "596586998"} {:id "def/emit-expr-handlers", :kind "def", :line 516, :end-line 572, :hash "1041141499"} {:id "defn-/emit-expr", :kind "defn-", :line 574, :end-line 580, :hash "749449684"} {:id "defn-/emit-default-constructor", :kind "defn-", :line 582, :end-line 592, :hash "240160418"} {:id "defn-/field-slot", :kind "defn-", :line 594, :end-line 598, :hash "-1567189554"} {:id "defn-/emit-record-constructor", :kind "defn-", :line 600, :end-line 616, :hash "-316497366"} {:id "defn-/emit-record-class-bytes", :kind "defn-", :line 618, :end-line 638, :hash "-382850934"} {:id "defn-/emit-union-base-class-bytes", :kind "defn-", :line 640, :end-line 652, :hash "861277052"} {:id "defn-/emit-union-variant-class-bytes", :kind "defn-", :line 654, :end-line 687, :hash "944748988"} {:id "defn-/emit-method", :kind "defn-", :line 689, :end-line 703, :hash "-1558709303"} {:id "defn/emit-module-bytes", :kind "defn", :line 705, :end-line 722, :hash "1831727313"} {:id "defn/emit-class-bytes", :kind "defn", :line 724, :end-line 766, :hash "-1403240972"}]}
;; clj-mutate-manifest-end
