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

(defn is-valid-name [items]
  (letfn [(is-letter-or-special
            [char]
            (or (= \_ char)
                (Character/isLetter char)))
          (is-letter-digit-or-special
            [char]
            (or (is-letter-or-special char)
                (Character/isDigit char)))
          (check-char
            [[fst & rest] validator if-valid]
            (if (or fst rest)
              (if (validator fst)
                (if rest
                  (if-valid rest)
                  true))))
          (check-remaining
            [items]
            (check-char items
                        is-letter-digit-or-special
                        check-remaining))
          (check-first
            [items]
            (check-char items
                        is-letter-or-special
                        check-remaining))]
    (not (nil? (check-first items)))))

(defn get-full-name [items]
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
        [(drop size items) name]))))

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
  (let [[remaining type-name] (get-full-name items)]
    (if type-name
      (let [[generic-remaining generic] (get-generic-type remaining)]
        (if generic
          [generic-remaining [type-name generic]]
          [remaining type-name])))))

(defn is-declaration [items]
  (loop [[fst & rest :as all] items
         count 0]
    (let [[remaining _ :as is-name ] (get-full-name all)]
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
                                 get-full-name :name)]
                (if found
                  (get-seperator remaining (cons decl declarations)))))
          (get-seperator [[fst & rest] declarations]
             (case fst
               "," (get-inner-decl rest declarations)
               ")" [rest declarations]
               nil))]
    (get-open items)))

(defn log [v] (prn "log" v) v)

(defn get-value
  [items]
  (letfn
    [(seperator
       [[fst & rest]]
       (case fst
         "," (inner-value rest)
         ")" [rest nil nil]
         nil))
     (inner-value
       [items]
       (when-let [[remaining value] (get-value items)]
         (cons-into-thrd value
                         (seperator remaining))))
     (inner-values
       [[fst & rest :as all]]
       (case fst
         ")" [rest nil]
         (log
           (inner-value all)
           )
         ))
     (method
       [all]
       (if-let [[[fst & rest] name] (get-full-name all)]
         (case fst
           "(" (log (cons-into-scnd name
                                    (log (inner-values rest))))
           nil)))
     (get-var
       [items]
       (if-let [name (get-full-name items)]
         name))]
    (if-let [method (method items)]
      method
      (get-var items))))





