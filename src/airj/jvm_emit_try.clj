(ns airj.jvm-emit-try
  (:import (clojure.asm Label MethodVisitor Opcodes)))

(defn- try-layout
  [expr env temp-slot]
  (let [result-needed? (not= :void (:jvm-type expr))
        result-slot (when result-needed?
                      (temp-slot env (:jvm-type expr)))
        env-after-result (or (:env result-slot) env)
        throwable-slot (when (:finally expr)
                         (temp-slot env-after-result "java/lang/Throwable"))
        catch-count (count (:catches expr))]
    {:start-label (Label.)
     :end-label (Label.)
     :done-label (Label.)
     :finally-label (when (:finally expr) (Label.))
     :catch-labels (vec (repeatedly catch-count #(Label.)))
     :catch-ranges (vec (repeatedly catch-count
                                    #(when (:finally expr)
                                       [(Label.) (Label.)])))
     :result-slot result-slot
     :throwable-slot throwable-slot
     :base-env (or (:env throwable-slot) env-after-result env)}))

(defn- emit-try-registrations
  [^MethodVisitor mv expr {:keys [start-label end-label finally-label catch-labels catch-ranges]}]
  (doseq [[handler catch-clause] (map vector catch-labels (:catches expr))]
    (.visitTryCatchBlock mv start-label end-label handler (:type catch-clause)))
  (when finally-label
    (.visitTryCatchBlock mv start-label end-label finally-label nil)
    (doseq [[catch-start catch-end] catch-ranges]
      (.visitTryCatchBlock mv catch-start catch-end finally-label nil))))

(defn- emit-result-store
  [^MethodVisitor mv result-slot jvm-type store-opcode]
  (when result-slot
    (.visitVarInsn mv (store-opcode jvm-type) (:slot result-slot))))

(defn- emit-result-load
  [^MethodVisitor mv result-slot jvm-type load-opcode]
  (when result-slot
    (.visitVarInsn mv (load-opcode jvm-type) (:slot result-slot))))

(defn- catch-env
  [base-env catch-clause throwable-slot]
  (let [slot-index (or (:slot throwable-slot)
                       (:next-slot base-env))
        env-with-slot (if throwable-slot
                        base-env
                        (update base-env :next-slot inc))]
    {:slot-index slot-index
     :env (assoc-in env-with-slot
                    [:slots (:name catch-clause)]
                    {:slot slot-index
                     :jvm-type (:type catch-clause)})}))

(defn- emit-try-success
  [^MethodVisitor mv expr layout tools]
  (.visitLabel mv (:start-label layout))
  ((:emit-expr tools) mv (:body expr) (:base-env layout))
  (emit-result-store mv (:result-slot layout) (:jvm-type expr) (:store-opcode tools))
  (.visitLabel mv (:end-label layout))
  (when-let [finally-expr (:finally expr)]
    ((:emit-discarded tools) mv finally-expr (:base-env layout)))
  (emit-result-load mv (:result-slot layout) (:jvm-type expr) (:load-opcode tools))
  (.visitJumpInsn mv Opcodes/GOTO (:done-label layout)))

(defn- emit-try-catch
  [^MethodVisitor mv expr layout catch-clause handler-label catch-range tools]
  (.visitLabel mv handler-label)
  (let [{:keys [slot-index env]} (catch-env (:base-env layout)
                                            catch-clause
                                            (:throwable-slot layout))]
    (.visitVarInsn mv Opcodes/ASTORE slot-index)
    (when-let [[catch-start _] catch-range]
      (.visitLabel mv catch-start))
    ((:emit-expr tools) mv (:body catch-clause) env)
    (emit-result-store mv (:result-slot layout) (:jvm-type expr) (:store-opcode tools))
    (when-let [[_ catch-end] catch-range]
      (.visitLabel mv catch-end))
    (when-let [finally-expr (:finally expr)]
      ((:emit-discarded tools) mv finally-expr env))
    (emit-result-load mv (:result-slot layout) (:jvm-type expr) (:load-opcode tools))
    (.visitJumpInsn mv Opcodes/GOTO (:done-label layout))))

(defn- emit-try-finally-handler
  [^MethodVisitor mv expr layout tools]
  (when-let [finally-expr (:finally expr)]
    (.visitLabel mv (:finally-label layout))
    (.visitVarInsn mv Opcodes/ASTORE (:slot (:throwable-slot layout)))
    ((:emit-discarded tools) mv finally-expr (:base-env layout))
    (.visitVarInsn mv Opcodes/ALOAD (:slot (:throwable-slot layout)))
    (.visitInsn mv Opcodes/ATHROW)))

(defn emit-try
  [^MethodVisitor mv expr env tools]
  (let [layout (try-layout expr env (:temp-slot tools))]
    (emit-try-registrations mv expr layout)
    (emit-try-success mv expr layout tools)
    (doseq [[catch-clause handler-label catch-range]
            (map vector
                 (:catches expr)
                 (:catch-labels layout)
                 (:catch-ranges layout))]
      (emit-try-catch mv expr layout catch-clause handler-label catch-range tools))
    (emit-try-finally-handler mv expr layout tools)
    (.visitLabel mv (:done-label layout))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T10:22:10.984689-05:00", :module-hash "-1033971699", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1679852136"} {:id "defn-/try-layout", :kind "defn-", :line 4, :end-line 23, :hash "21833502"} {:id "defn-/emit-try-registrations", :kind "defn-", :line 25, :end-line 32, :hash "-1993258922"} {:id "defn-/emit-result-store", :kind "defn-", :line 34, :end-line 37, :hash "-1701649778"} {:id "defn-/emit-result-load", :kind "defn-", :line 39, :end-line 42, :hash "201184486"} {:id "defn-/catch-env", :kind "defn-", :line 44, :end-line 55, :hash "1751921815"} {:id "defn-/emit-try-success", :kind "defn-", :line 57, :end-line 66, :hash "1875174106"} {:id "defn-/emit-try-catch", :kind "defn-", :line 68, :end-line 84, :hash "-667113241"} {:id "defn-/emit-try-finally-handler", :kind "defn-", :line 86, :end-line 93, :hash "-1070537968"} {:id "defn/emit-try", :kind "defn", :line 95, :end-line 107, :hash "1018454071"}]}
;; clj-mutate-manifest-end
