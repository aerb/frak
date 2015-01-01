(ns refac.comp
  (use refac.comp))

(defn merge-into-vec [m0 [items m1]]
  [items (merge m0 m1)])

(defn chain-merge [items & [fn key & rest]]
  (if fn
    (let [[remaining artifact :as success] (fn items)]
      (if success
        (merge-into-vec {key artifact}
                        (apply chain-merge remaining rest))))
    [items {}]))

(defn optional-chain-merge [items & [fn key & rest]]
  (if fn
    (let [[remaining artifact :as success] (fn items)]
      (if success
        (merge-into-vec {key artifact} (apply optional-chain-merge remaining rest))
        (apply optional-chain-merge items rest)))
    [items {}]))
