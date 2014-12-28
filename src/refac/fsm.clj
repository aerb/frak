(ns refac.fsm)

(defn stay [] {})
(defn goto [state] {:next state})
(defn pop-state [] {:pop-state true})
(defn push-state [return] (assoc return :push-state true))
(defn with [return value] (assoc return :push-arg value))

(defn curr [context] (context :current))

(defn remember [response key value] (assoc-in response (cons :remember key) value))

(defmacro ccase [e & clauses] `(case (:current ~e) ~@clauses))