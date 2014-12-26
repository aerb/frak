(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.java))

(defn state-for [arg]
  (if (contains? handlers arg)
    (handlers arg)
    (throw (Exception. arg))))

(defn handle-symbol [context symbol]
  (let [state   (:state context)
        current (:current state)
        handler (state-for current)
        next    (handler context symbol)]
    next))

(defn initial-context [] {:state {:current "base" :arg nil}
                          :class {:fields {}}})

(defn fsm-reduce [reducer init items]
  (loop [reduction init
         items     items]
    (if (not (empty? items))
             (recur (reducer reduction
                             (first items))
                    (rest items)))))

(defn process-symbols [symbols]
  (fsm-reduce handle-symbol (initial-context) symbols))


(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



