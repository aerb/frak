(ns refac.java
  (:require [refac.fsm :as fsm]))

(def handlers {
   "ignore-to-eol"
   (fn [symbol args]
     (case symbol
           ";" (fsm/pop-state)
           (fsm/stay)))
   "package"
   (fn [symbol args] (fsm/goto "ignore-to-eol"))
   "import"
   (fn [symbol args] (fsm/goto "ignore-to-eol"))
   "class"
   (fn [symbol [privacy]]
     (case symbol
       "{" (fsm/goto "class-body")
       (-> (fsm/stay)
           (fsm/remember [:class :name] symbol)
           (fsm/remember [:class :privacy] privacy))))
   "declaration"
   (fn [symbol args]
     (case symbol
       "class" (fsm/goto "class")
     (->(fsm/goto "type")
        (fsm/with symbol))))
   "class-body"
   (fn [symbol args]
     (case symbol
       "}" (fsm/pop-state)
       ("private" "public" "internal" "protected") (-> (fsm/goto "declaration")
                                                       (fsm/with symbol)
                                                       (fsm/push-state))

       (-> (fsm/goto "declaration")
           (fsm/with "private")
           (fsm/push-state))))
   "field"
   (fn [assignment [name type]]
     (-> (case assignment
           "=" (fsm/goto "ignore-to-eol")
           ";" (fsm/pop-state))
         (fsm/remember [:class (keyword name)] {})
         (fsm/remember [:class (keyword name) :type] type)))
   "generic"
   (fn [symbol _]
     (case symbol
       ">" (fsm/pop-state)
       (fsm/stay)))
   "type"
   (fn [name _]
     (case name
       "<" (->(fsm/goto "generic")
              (fsm/push-state))
       (-> (fsm/goto "field")
           (fsm/with name))))
   "base"
   (fn [symbol args]
       (case symbol
           "class" (-> (fsm/goto "class")
                       (fsm/with "private")
                       (fsm/push-state))

           ("import" "package") (-> (fsm/goto symbol)
                                    (fsm/push-state))

           ("private" "public" "internal" "protected") (-> (fsm/goto "declaration")
                                                           (fsm/with symbol)
                                                           (fsm/push-state))))
   })