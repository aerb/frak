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


(defn assoc-if [context key value]
  (if value
    (assoc context key value)
    context))

(defn handle-response [context response]
  (let [{:keys [next
                remember
                pop-state
                push-state
                push-arg]} response

        context   (if pop-state  (assoc context :state (:pushed-state context)) context)
        context   (if push-state (assoc context :pushed-state (:state context)) context)
        context   (if push-arg   (assoc context :args (cons push-arg (:args context))) context)
        context   (assoc-if context :state next)
        context   (assoc-if context :remember remember)]
    context))

(defn handle-symbol [context symbol]
  (println "context -> " context " -- " symbol)
  ;(println symbol)
  (println (context :remember))
  (let [handler       (handler-for (:state context))
        response      (handler symbol (:args context))
        next-context  (handle-response context response)]
    ;(println "response -> " response)
    next-context))

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



