(ns refac.syntax)

(defn is-valid-name [[fst & rest]]
  (if (or fst rest)
    (if (or (= \_ fst)
            (Character/isLetterOrDigit fst))
      (if rest
        (recur rest)
        true)
      false)
    false))

(defn get-name [items]
  (letfn [(get-seperator [[fst & rest]]
            (case fst
              "." (let [next-ns (get-ns rest)]
                    (if next-ns
                      (cons fst next-ns)
                      nil))
              nil))
          (get-ns [[fst & rest]]
             (if (is-valid-name fst)
               (cons fst (get-seperator rest))
               nil))]
    (let [name (get-ns items)
          size (count name)]
      (if name
        [(drop size items) (reduce str name)]))))

(declare get-type)

(defn get-generic-type [items]
  (letfn [(get-open [[fst & rest]]
            (case fst
              "<" (get-close rest nil)
              nil))
          (get-close [[fst & rest :as all] types]
            (case fst
              ">" [rest types]
              (get-inner-type all types)))
          (get-inner-type [[fst & rest :as all] types]
            (let [[remaining type] (get-type all)]
              (get-seperator remaining (cons type types))))
          (get-seperator [[fst & rest] types]
              (case fst
                "," (get-inner-type rest types)
                ">" [rest types]
                nil))]
    (get-open items)))

(defn get-type [items]
  (let [[remaining type-name] (get-name items)]
    (if type-name
      (let [[generic-remaining generic] (get-generic-type remaining)]
        (if generic
          [generic-remaining [type-name generic]]
          [remaining type-name])))))

(defn is-declaration [items]
  (loop [[fst & rest :as all] items
         count 0]
    (let [name (get-name all)]
      (if name
        (recur (first name) (inc count))
        (and (= fst "=") (= count 1))))))









