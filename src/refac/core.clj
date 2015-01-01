(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.syntax)
  (:require [clojure.string :as s]))

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

(declare do-declaration-modifiers)
(declare do-declaration)
(declare do-body)

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
        (let [[remaining _] (do-declaration items)]
          remaining)))))

(defn do-declaration-name [[name & others]]
  [others {:name name}])

(defn do-declaration-assign [[assignment & others]]
  (let [remaining (case assignment
                    "{" (do-body others)
                    "(" (do-method others)
                    "=" (do-assignment others)
                    ";" others)]
    [remaining {}]))



(defn append-modifier [[remaining info] modifer]
  (let [modifiers (:modifiers info)]
    [remaining (assoc info
                      :modifiers
                      (cons modifer modifiers))]))

(defn do-declaration-modifiers [items]
  (let [[fst & others] items
        modifier (case fst
                   ("public" "private"
                    "protected" "internal"
                    "static" "final") fst
                   nil)]
    (if modifier
      (-> (do-declaration-modifiers others)
          (append-modifier modifier))
      [items nil])))

(defn merge-results [results0
                     results1]
  (let
    [[_          info0] results0
     [remaining1 info1] results1]
    (if results1
      [remaining1 (merge info0 info1)]
    results0)))

(defn chain-merge [items & fns]
  (let [[fn & fns] fns]
    (if fn
      (let [results (fn items)
           [remaining _] results]
        (merge-results results
                       (apply chain-merge
                              remaining
                              fns)))
      [items nil])))



(defn is-next-declaration [items]
  (loop [[fst & rest] items
         count 0]
    (case fst
      ("this" "super") false
      "=" (> count 1)
      ("(" "{") true
      (recur rest (inc count)))))

(defn do-declaration [items]
  (chain-merge items
               do-declaration-modifiers
               ;do-declaration-type
               do-declaration-name
               do-method))

(defn do-root [all-items]
  (let [mapped (map-indexed (fn [index item] [index item]) all-items)]
    (loop [[items info] [all-items {}]]

      (println (for [[index value] mapped
                      :when (identical? value (:name info))]
                      [index value]))

      (let [[next & others] items]
        (if next
          (recur
            (case next
              "package" (do-package others)
              "import"  (do-import others)
              (do-declaration-modifiers items)))
          (println "done"))))))

(defn process-symbols [symbols] (do-root symbols))

(defn -main [] (process-symbols (get-symbol-stream)))