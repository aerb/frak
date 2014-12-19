(ns refac.core
  (:gen-class))

(require '[clojure.string :as str])
(use 'clojure.java.io)

(def visibility-keywords
  #{"private"
    "public"
    "protected"
    "internal"})

(def special-characters
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
        )))))

(defn process-class [symbols]
  (let [symbol (first symbols)])
  )

(defn process-symbols [symbols]
  (if (not (nil? symbols))
    (let [symbol (first symbols)]
      (recur
        (case symbol
          "package" (process-package (next symbols))
          "import"  (process-package (next symbols))
          "class"   (process-class (next symbols))
          (next symbols))))))

(defn read-java [] (slurp "test.java"))

(defn pad-symbol [orig symbol]
  (str/replace orig symbol (str/join [" " symbol " "]))
)

(defn pad-all-symbols
  [str]
  (reduce
    (fn [acc item] (pad-symbol acc item))
    str
    special-characters))

(defn get-symbol-stream
  []
  (filter
    (fn [x] (not (empty? x)))
    (str/split
      (pad-all-symbols
        (read-java))
      #"\s")))

(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



