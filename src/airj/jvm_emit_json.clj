(ns airj.jvm-emit-json
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

(defn emit-json-parse
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-clojure-var mv "airj.json-runtime" "parse")
  (emit-expr mv (:arg expr) env)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

(defn emit-json-write
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-clojure-var mv "airj.json-runtime" "write")
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/String"))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T12:43:51.561099-05:00", :module-hash "-621493555", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-1494685321"} {:id "defn-/emit-clojure-var", :kind "defn-", :line 4, :end-line 13, :hash "487263659"} {:id "defn/emit-json-parse", :kind "defn", :line 15, :end-line 26, :hash "152759191"} {:id "defn/emit-json-write", :kind "defn", :line 28, :end-line 38, :hash "-729661747"}]}
;; clj-mutate-manifest-end
