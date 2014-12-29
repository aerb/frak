(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.java)
  (:require [clojure.string :as s]))

(defn ignore-to [[first & others] stop-at]
  {:pre [(not (nil? first))]}
  (if (= first stop-at) others
    (recur others stop-at)))

(defn nested-ignore-to [open close items]
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
  (->> (ignore-to items "{")
       (nested-ignore-to "{" "}")))

(defn do-assignment [items] (ignore-to-eol items))

(declare do-declaration)

(defn do-body [items]
  (let [[next & others] items]
    (case next
      "}" others
      (recur
        (let [[remaining _] (do-declaration items)]
          remaining)))))

(defn do-declaration-name [[name assignment & others]]
  (let [info {:name name}
        remaining (case assignment
                    "{" (do-body others)
                    "(" (do-method others)
                    "=" (do-assignment others)
                    ";" others)]
    [remaining info]))

(defn do-generic-type [items]
  (loop [items items
         depth 1
         generic "<"]
    (let [[first & others] items]
      (if-not (= depth 0)
        (recur
          others
          (+ depth
             (case first
               "<"  1
               ">" -1
               0))
          (str generic first))
        [items generic]))))

(defn do-declaration-type [items]
  (let [[type generic-declaration & others] items
        [remaining generic] (case generic-declaration
                              "<" (do-generic-type others)
                              nil)
        has-generic (not (nil? generic))
        remaining (if has-generic
                    remaining
                    (cons generic-declaration others))
        type {:type (str type generic)}

        [remaining info] (do-declaration-name remaining)]
    [remaining (merge info type)]))

(defn log [[remaining info]]
  (println info)
  [remaining info])

(defn do-declaration [items]
  (let [[first & others] items
        declaration (case first
                      ("public" "private" "protected" "internal") [first (do-declaration-type others)]
                      ["private" (do-declaration-type items)])
        [visability [remaining info]] declaration]

    (log [remaining (merge info {:visability visability})])))

(defn do-root [items]
  (loop [[items _] [items {}]]
    (let [[next & others] items]
      (recur
        (case next
          "package" (do-package others)
          "import"  (do-import others)
          (do-declaration items))))))

(defn process-symbols [symbols]
  (do-root symbols))

(defn -main [] (process-symbols (get-symbol-stream)))