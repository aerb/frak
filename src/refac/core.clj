(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.java))

(defn ignore-to-eol [[first & others]]
  (case first
    ";" others
    (recur others)))

(defn do-package [items] [(ignore-to-eol items) {}])
(defn do-import  [items] [(ignore-to-eol items) {}])

(defn do-open-body [[first & others]]
  (if-not (= first "{")
    (throw (IllegalStateException. (str "Expected {, got " first)))
    others))

(defn do-body [items]
  (loop [[first & others] (do-open-body items)
         indent 1]
    (println first)
    (when-not (= indent 0)
      (recur
        others
         (+ indent
            (case first
              "{"  1
              "}" -1
              0))
        ))))

(defn do-type [[first second & others]]
  (let [info {:type first :name second}
        remaining (case first
                     "class" (do-body others))]
    [remaining info]))

(defn do-declaration [items]
  (let [[first & others] items
        [visability [remaining info]]
          (case first
            ("public" "private" "protected" "internal") [first (do-type others)]
            ["private" (do-type items)])]
    [remaining (assoc info :visability visability)]))

(defn do-root [items]
  (loop [[items _] [items {}]]
    (let [[first & others] items]
      (println first)
      (recur
        (case first
          "package" (do-package others)
          "import"  (do-import others)
          (do-declaration items))))))

(defn process-symbols [symbols]
  (do-root symbols))


(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



