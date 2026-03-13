(ns airj.jvm-emit-text
  (:import (clojure.asm MethodVisitor Opcodes)))

(defn emit-int->string
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/lang/String"
                    "valueOf"
                    "(I)Ljava/lang/String;"
                    false))

(defn emit-string-eq
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "equals"
                    "(Ljava/lang/Object;)Z"
                    false))

(defn emit-string-concat
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "concat"
                    "(Ljava/lang/String;)Ljava/lang/String;"
                    false))

(defn emit-string->int
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/lang/Integer"
                    "parseInt"
                    "(Ljava/lang/String;)I"
                    false))

(defn emit-string-length
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "length"
                    "()I"
                    false))

(defn emit-io-read-line
  [^MethodVisitor mv _expr _env _]
  (.visitTypeInsn mv Opcodes/NEW "java/util/Scanner")
  (.visitInsn mv Opcodes/DUP)
  (.visitFieldInsn mv
                   Opcodes/GETSTATIC
                   "java/lang/System"
                   "in"
                   "Ljava/io/InputStream;")
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/Scanner"
                    "<init>"
                    "(Ljava/io/InputStream;)V"
                    false)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/util/Scanner"
                    "nextLine"
                    "()Ljava/lang/String;"
                    false))

(defn emit-io-print
  [^MethodVisitor mv expr env {:keys [emit-expr method-name]}]
  (.visitFieldInsn mv
                   Opcodes/GETSTATIC
                   "java/lang/System"
                   "out"
                   "Ljava/io/PrintStream;")
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/io/PrintStream"
                    method-name
                    "(Ljava/lang/String;)V"
                    false))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T13:05:40.435351-05:00", :module-hash "2027261832", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1178648239"} {:id "defn/emit-int->string", :kind "defn", :line 4, :end-line 12, :hash "726986209"} {:id "defn/emit-string-eq", :kind "defn", :line 14, :end-line 23, :hash "-592416675"} {:id "defn/emit-string-concat", :kind "defn", :line 25, :end-line 34, :hash "30394137"} {:id "defn/emit-string->int", :kind "defn", :line 36, :end-line 44, :hash "219712908"} {:id "defn/emit-string-length", :kind "defn", :line 46, :end-line 54, :hash "411227571"} {:id "defn/emit-io-read-line", :kind "defn", :line 56, :end-line 76, :hash "518289273"} {:id "defn/emit-io-print", :kind "defn", :line 78, :end-line 91, :hash "-1466442218"}]}
;; clj-mutate-manifest-end
