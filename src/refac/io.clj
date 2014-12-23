(ns refac.io
  (:require [clojure.string :as str]))

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


(defn read-java [] (slurp "test.java"))

(defn pad-symbol [orig symbol]
  (str/replace orig symbol (str/join [" " symbol " "])))

(defn pad-all-symbols
  [java-str]
  (reduce
    (fn [acc item] (pad-symbol acc item))
    java-str
    special-characters))

(defn get-symbol-stream
  []
  (filter
    (fn [x] (not (empty? x)))
    (str/split
      (pad-all-symbols
        (read-java))
      #"\s")))
