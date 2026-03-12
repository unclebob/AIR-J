(ns airj.normalizer
  (:require [clojure.string :as str]))

(def ^:private decl-order
  {:data 0
   :enum 1
   :union 2
   :fn 3})

(defn- sort-symbols
  [symbols]
  (sort symbols))

(defn- sort-imports
  [imports]
  (sort-by (fn [import]
             (case (:op import)
               :airj-import (str (pr-str (:module import))
                                 ":"
                                 (str/join "," (map pr-str (:symbols import))))
               :java-import (pr-str (:class-name import))
               [99 (pr-str import)]))
           imports))

(defn- normalize-decl
  [decl]
  (if (= :fn (:op decl))
    (update decl :effects #(vec (sort-symbols %)))
    decl))

(defn normalize-module
  [module]
  (-> module
      (update :imports #(vec (sort-imports %)))
      (update :exports #(vec (sort-symbols %)))
      (update :decls #(->> %
                           (map normalize-decl)
                           (sort-by (fn [decl]
                                      [(get decl-order (:op decl) 99)
                                       (pr-str (:name decl))]))
                           vec))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-12T12:23:29.246155-05:00", :module-hash "1067752515", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "144684559"} {:id "def/decl-order", :kind "def", :line 4, :end-line 8, :hash "1339233876"} {:id "defn-/sort-symbols", :kind "defn-", :line 10, :end-line 12, :hash "-614792892"} {:id "defn-/sort-imports", :kind "defn-", :line 14, :end-line 23, :hash "-991055912"} {:id "defn-/normalize-decl", :kind "defn-", :line 25, :end-line 29, :hash "-48949182"} {:id "defn/normalize-module", :kind "defn", :line 31, :end-line 41, :hash "1571641215"}]}
;; clj-mutate-manifest-end
