(ns refac.core
  (:gen-class)
  (use clojure.java.io)
  (use refac.io))

(def visibility-keywords
  #{"private"
    "public"
    "protected"
    "internal"})

(defn handle-namespace [namespace symbol]
  (case symbol
    "." [false namespace]
    ";" [true  namespace]
        [false (if (nil? symbol)
                 (list symbol)
                 (cons symbol namespace))]
        ))

(defn handle-package [context symbol]
  (let [[complete namespace] (handle-namespace (context :namespace) symbol)]
    (if complete
      (assoc context :mode "init"
                     :namespace namespace)
      (assoc context :namespace namespace))))

(defn handle-import [context symbol]
  (if (= ";" symbol)
    (assoc context :mode "init"))
  context)

(defn next-handler [context symbol]
  (case symbol
    "package" (assoc context :mode "package")
    "import"  (assoc context :mode "import")))

(defn handle-symbol [context symbol]
  (let [mode (context :mode)]
    (case mode
      "init"    (next-handler   context symbol)
      "package" (handle-package context symbol)
      "import"  (handle-import  context symbol)
      context)))

(defn process-symbols [symbols]
  (reduce handle-symbol (hash-map :mode "init") symbols))

(defn -main
  [& args]
  (println (process-symbols
    (get-symbol-stream))))



