(ns refac.fsm)

(defn goto-state
  ([context state]
    (println "Goto -> " state " " (get-in context [:state :arg]))
    (assoc-in context [:state :current] state)))

(defn goto-state-with
  ([context state arg]
    (let [context (assoc-in context [:state :current] state)
          current (get-in context [:state :arg])
          next    (assoc-in context [:state :arg]
                    (if
                      (and (instance? clojure.lang.IFn arg)
                           (not (instance? clojure.lang.IPersistentMap arg)))
                      (arg current)
                      arg))]
      (println "Goto -> " state " " (get-in context [:state :arg]))
      next)))


(defn state-arg [context]
  (let [arg (context :state :arg)]
    [arg (assoc context [:state :arg] nil)]))
