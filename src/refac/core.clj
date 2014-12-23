(ns refac.core
  (:gen-class)
  (use clojure.java.io)
  (use refac.io)
  (:require [clojure.string :as str]))

(defn goto-state
  ([context state] (assoc-in context [:state :current] state)))

(defn goto-state-with
  ([context state arg]
    (-> context
        (assoc-in [:state :current] state)
        (assoc-in [:state :arg] arg))))

(defn state-arg [context]
  (let [arg (context :state :arg)]
    [arg (assoc context [:state :arg] nil)]))

(defn handle-namespace [namespace symbol]
  (case symbol
    "." [false namespace]
    ";" [true  namespace]
        [false (if (nil? symbol)
                 (list symbol)
                 (cons symbol namespace))]
    ))

(defn handle-package [context symbol]
  (let [[complete namespace] (handle-namespace (get-in context [:class :namespace]) symbol)]
    (if complete
      (-> context
          (goto-state "base")
          (assoc-in [:class :namespace] namespace))
    (assoc-in context [:class :namespace] namespace))))

(defn handle-import [context symbol]
  (if (= ";" symbol)
    (goto-state context "base")
    context))

(defn handle-class [context symbol]
  (case symbol
    "{" (-> context
            (assoc-in [:class :body] {})
            (goto-state "class-body"))
    (-> context
        (assoc-in [:class :name] symbol)
        (assoc-in [:class :visability] (get-in context [:state :arg]))
        (assoc-in [:state :arg] nil))))

(defn handle-declaration [context symbol]
    (case symbol
      "class" (-> context (goto-state "class"))
      (-> context (goto-state "type"))))

(defn handle-class-body [context symbol]
  (case symbol
    "}" (-> context (goto-state "base"))
    (-> context (goto-state-with "declaration" symbol))))

(defn handle-type [context symbol] context)

(defn handle-base [context symbol]
  (case symbol
    ("class" "import" "package")                (goto-state context symbol)
    ("private" "public" "protected" "internal") (goto-state-with context "declaration" symbol)
    (throw IllegalStateException)))

(def states
  {"declaration"  handle-declaration
   "base"         handle-base
   "package"      handle-package
   "import"       handle-import
   "class"        handle-class
   "type"         handle-type
   "class-body"   handle-class-body})

(defn state-for [arg]
  (if (contains? states arg)
    (states arg)
    (throw (Exception. arg))))

(defn handle-symbol [context symbol]
  (let [state   (context :state)
        current (state :current)
        handler (state-for current)
        next    (handler context symbol)]
    (println next)
    next))

(defn initial-context [] {:state {:current "base" :arg nil}
                          :class {:fields {}}})

(defn process-symbols [symbols]
  (reduce handle-symbol (initial-context) symbols))

(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



