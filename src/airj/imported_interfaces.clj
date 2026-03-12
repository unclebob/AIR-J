(ns airj.imported-interfaces)

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn imported-decls
  [module]
  (let [interfaces (:interfaces module)]
    (reduce (fn [acc import]
              (if-not (= :airj-import (:op import))
                acc
                (let [interface (get interfaces (:module import))]
                  (when-not interface
                    (fail! "Missing AIR-J interface."
                           {:module (:name module)
                            :imported-module (:module import)}))
                  (reduce (fn [current symbol]
                            (let [decl (some #(when (= symbol (:name %)) %) (:decls interface))]
                              (when-not (and (some #{symbol} (:exports interface)) decl)
                                (fail! "Unresolved imported symbol."
                                       {:module (:name module)
                                        :imported-module (:module import)
                                        :symbol symbol}))
                              (assoc current symbol
                                     {:module (:module import)
                                      :decl decl})))
                          acc
                          (:symbols import)))))
            {}
            (:imports module))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T12:48:30.579001-05:00", :module-hash "803235671", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "199070276"} {:id "defn-/fail!", :kind "defn-", :line 3, :end-line 5, :hash "879938479"} {:id "defn/imported-decls", :kind "defn", :line 7, :end-line 31, :hash "-1673304585"}]}
;; clj-mutate-manifest-end
