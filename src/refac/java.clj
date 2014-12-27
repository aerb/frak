(ns refac.java
  (:require [refac.fsm :as fsm]))

(def handlers {
   "ignore-to-eol"
   (fn [context]
     (fsm/ccase context
                ";" (fsm/pop-state)
                (fsm/stay)))
   "package"
   (fn [context] (fsm/goto "ignore-to-eol"))
   "import"
   (fn [context] (fsm/goto "ignore-to-eol"))
   "class"
   (fn [context]
     (fsm/ccase context
       "{" (fsm/goto "class-body")
       (-> (fsm/stay)
           (fsm/remember [:class :name] (fsm/curr context)))))
   "declaration"
   (fn [context]
     (fsm/ccase context
       "class" (fsm/goto "class")
       (fsm/goto "type")))
   "class-body"
   (fn [context]
     (fsm/ccase context
       "}" (fsm/pop-state)
       (-> (fsm/goto "declaration")
           (fsm/push-state))))
   "field"
   (fn [context]
     (-> (fsm/goto "ignore-to-eol")))
   "type"
   (fn [context]
     (-> (fsm/goto "field")))
   "base"
   (fn [context]
     (let [cur (fsm/curr context)]
       (case cur
             ("class" "import" "package") (-> (fsm/goto (fsm/curr context))
                                              (fsm/push-state))
             ("private" "public" "protected" "internal") (fsm/goto "declaration")
             (throw IllegalStateException)))
     )
   })