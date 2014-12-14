(ns refac.core
  (:gen-class))

(require '[clojure.string :as str])
(use 'clojure.java.io)

(def visibility-keywords
  #{"private"
    "public"
    "protected"
    "internal"})

(def punctuation
  #{
    "{" "}"
    "(" ")"
    "[" "]"
    "<" ">"
    ";"
    "."
    "="
    ","
  })


(defn process-package [symbols]
  (println "package")
  (loop [symbols symbols
         namespace ()]
    (if (not (nil? symbols))
      (let [symbol (first symbols)]
        (cond
          (= symbol ".") (recur (next symbols)
                                namespace)
          (= symbol ";") (do
                           (println (reverse namespace))
                           symbols)
          :else (recur (next symbols)
                       (cons symbol namespace))
        )
      )
    )
  )
)

(defn process-import [symbols]
  (println "import")
  symbols)

(defn process-symbols [symbols]
  (loop [symbols symbols]
    (if (not (nil? symbols))
      (let [symbol (first symbols)]
        (recur
          (cond
            (= symbol "package") (process-package (next symbols))
            (= symbol "import")  (process-import (next symbols))
            :else (next symbols)))))))

(defn read-java [] (slurp "test.java"))

(defn pad-symbol [orig symbol]
  (str/replace orig symbol (str/join [" " symbol " "]))
)

(defn pad-all-symbols
  [str]
  (loop [p (vec punctuation)
         s str]
    (if (empty? p)
      s
      (recur
        (rest p)
        (pad-symbol s (first p)))
    )
  ) 
)

(defn get-symbol-stream
  []
  (filter
    (fn [x] (not (empty? x)))
    (str/split
      (pad-all-symbols
        (read-java))
      #"\s")
    )
  )

(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



