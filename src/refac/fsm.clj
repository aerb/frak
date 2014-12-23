(ns refac.fsm)

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
