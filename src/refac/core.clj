(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.syntax)
  (use refac.comp)
  (:require [clojure.string :as s]))

(declare do-declaration)
(declare do-body)

(defn ignore-to [[first & others] stop-at]
  {:pre [(not (nil? first))]}
  (if (= first stop-at) others
    (recur others stop-at)))

(defn nested-ignore-to [items open close]
  (loop [items items
         indent 1]

    (let [[next & others] items
          indent (+ indent
                    (condp = next
                      open   1
                      close -1
                      0))]
      (if (= indent 0)
        others
        (do
          (if (nil? items) (throw (IllegalArgumentException. "Did not find closing.")))
          (recur others indent))))))

(defn ignore-to-eol [items] (ignore-to items ";"))

(defn do-package [items] [(ignore-to-eol items) {}])
(defn do-import  [items] [(ignore-to-eol items) {}])

(defn do-method [items]
  (let [[fst & others] items]
    [(case fst
      "(" (nested-ignore-to others "(" ")")
      items)
     {:is-method (= fst "(")}
    ]))

(defn do-assignment [items] (ignore-to-eol items))

(defn do-body [items]
  (let [[next & others] items]
    (case next
      "}" others
      (recur
        (let [[remaining _] (get-type items)]
          remaining)))))

(defn do-declaration-assign [[assignment & others]]
  (let [remaining (case assignment
                    "{" (do-body others)
                    "(" (do-method others)
                    "=" (do-assignment others)
                    ";" others)]
    [remaining {}]))

(defn get-type-declaration-body [[first & rest :as items]]
  (if (is-declaration items) nil))

(defn get-method-declaration [items]
  (optional-chain-merge items
               get-declaration-modifiers :modifiers
               get-generic-type :generic-symbol
               get-type :return-type
               get-full-name :name
               get-parameters :params))

(defn if-remaining [match matched-fn]
  (fn [[fst & rest]]
      (condp = fst
        match (matched-fn rest)
        nil)))

(defn get-var-declaration [items]
  (optional-chain-merge items
                        get-declaration-modifiers :modifiers
                        get-type :type
                        get-full-name :name
                        (if-remaining "=" get-value) :value))

(defn get-type-declaration [items]
  (optional-chain-merge items
               get-declaration-modifiers :modifiers
               get-type :type
               get-full-name :name
               get-type-declaration-body :members))

(defn do-root [all-items]
  (loop [[items _] [all-items {}]]
    (let [[next & others] items]
      (if next
        (recur
          (case next
            "package" (do-package others)
            "import"  (do-import others)
            (get-type-declaration items)))
        (println "done")))))

(defn process-symbols [symbols] (do-root symbols))

(defn -main [] (process-symbols (get-symbol-stream)))