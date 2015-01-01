(ns refac.syntax
  (use refac.comp))

(defn get-declaration-modifiers [items]
  (letfn [(next-modifier [[fst & rest]]
           (let [modifier (case fst
                            ("public" "private" "protected" "internal"
                             "static" "final") fst
                            nil)]
             (if modifier
               (cons modifier (next-modifier rest))
               )))]
    (let [modifiers (next-modifier items)]
      (if modifiers
        [(drop (count modifiers) items) modifiers]))))

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
              "<" (get-close rest '())
              nil))
          (get-close [[fst & rest :as all] types]
            (case fst
              ">" [rest types]
              (get-inner-type all types)))
          (get-inner-type [all types]
            (let [[remaining type :as valid-type] (get-type all)]
              (if valid-type
                (get-seperator remaining (cons type types)))))
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
    (let [[remaining _ :as is-name ] (get-name all)]
      (if is-name
        (recur remaining (inc count))
        (and (= fst "=") (= count 1))))))

(defn get-parameters [items]
  (letfn [(get-open [[fst & rest]]
            (case fst
              "(" (get-close rest)
              nil))
          (get-close [[fst & rest :as all]]
             (case fst
               ")" [rest '()]
               (get-inner-decl all '())))
          (get-inner-decl [items declarations]
              (let [[remaining decl :as found]
                    (chain-merge items
                                 get-type :type
                                 get-name :name)]
                (if found
                  (get-seperator remaining (cons decl declarations)))))
          (get-seperator [[fst & rest] declarations]
             (case fst
               "," (get-inner-decl rest declarations)
               ")" [rest declarations]
               nil))]
    (get-open items)))







