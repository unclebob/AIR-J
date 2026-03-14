(ns airj.jvm-emit-host
  (:import (clojure.asm MethodVisitor Opcodes Type)))

(defn- emit-clojure-var
  [^MethodVisitor mv ns-name var-name]
  (.visitLdcInsn mv ^String ns-name)
  (.visitLdcInsn mv ^String var-name)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "clojure/lang/RT"
                    "var"
                    "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"
                    false))

(defn emit-env-get
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-clojure-var mv "airj.host-runtime" "env-get")
  (emit-expr mv (:arg expr) env)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

(defn emit-process-run
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-clojure-var mv "airj.host-runtime" "process-run")
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T14:34:07.26213-05:00", :module-hash "367337837", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1892005050"} {:id "defn-/emit-clojure-var", :kind "defn-", :line 4, :end-line 13, :hash "487263659"} {:id "defn/emit-env-get", :kind "defn", :line 15, :end-line 26, :hash "1247850418"} {:id "defn/emit-process-run", :kind "defn", :line 28, :end-line 40, :hash "1787825420"}]}
;; clj-mutate-manifest-end
