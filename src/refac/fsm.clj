(ns refac.fsm)

(defn goto [state] {:next state})
(defn stay [] {})
(defn pop-state [] {:pop-state true})
(defn push-state [return] (assoc return :push-state true))

(defn curr [context] (context :current))

(defn remember [response key value] (assoc-in response (cons :remember key) value))

(defmacro ccase [e & clauses] `(case (:current ~e) ~@clauses))
