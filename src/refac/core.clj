(ns refac.core
  (:gen-class)
  (use refac.io)
  (use refac.java))

(defn handler-for [arg]
  (if (contains? handlers arg)
    (handlers arg)
    (do
      (println arg)
      (throw (IllegalStateException. arg)))))

(defn handle-symbol [context current]
  (let [context   (assoc context :current current)

        log       (println "context -> " context)

        current   (:state context)
        handler   (handler-for current)

        response  (handler context)

        remember  (:remember response)
        next      (:next response)
        pop       (:pop-state response)
        push      (:push-state response)
        context   (cond
                    next
                    (assoc (if push
                             (assoc context
                                    :pushed
                                    current)
                             context)
                           :state next)
                    pop
                    (assoc context :state (:pushed context))
                    :else
                    context)
        context   (if remember (assoc context
                                      :remember
                                      remember)
                               context)]

    (println "response -> " response)
    context
))

(defn fsm-reduce [reducer init items]
  (loop [reduction init
         items     items]
    (if (not (empty? items))
             (recur (reducer reduction
                             (first items))
                    (rest items)))))

(defn process-symbols [symbols]
  (fsm-reduce handle-symbol {:state "base"} symbols))


(defn -main
  [& args]
  (process-symbols
    (get-symbol-stream)))



