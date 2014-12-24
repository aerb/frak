(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.java))

(def states
  {"declaration"  handle-declaration
   "base"         handle-base
   "package"      handle-package
   "import"       handle-import
   "class"        handle-class
   "type"         handle-type
   "field"        handle-field-name
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
    next))

(defn initial-context [] {:state {:current "base" :arg nil}
                          :class {:fields {}}})

(defn process-symbols [symbols]
  (reduce handle-symbol (initial-context) symbols))

(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



