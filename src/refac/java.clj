(ns refac.java
  (use refac.fsm))

(defn handle-namespace [namespace symbol]
  (case symbol
    "." [false namespace]
    ";" [true  namespace]
    [false (if (nil? symbol)
             (list symbol)
             (cons symbol namespace))]))

(def handlers {
   "package"
    (fn [context symbol]
      (let [[complete namespace] (handle-namespace (get-in context [:class :namespace]) symbol)]
        (if complete
          (-> context
              (goto-state "base")
              (assoc-in [:class :namespace] namespace))
          (assoc-in context [:class :namespace] namespace))))
   "import"
   (fn [context symbol]
     (if (= ";" symbol)
       (goto-state context "base")
       context))
   "class"
   (fn [context symbol]
     (case symbol
       "{" (-> context
               (goto-state "class-body"))
       (-> context
           (assoc-in [:class :name] symbol)
           (assoc-in [:class :visability] (get-in context [:state :arg])))))
   "declaration"
   (fn [context symbol]
     (case symbol
       "class" (-> context (goto-state "class"))
       (-> context
           (goto-state-with "type" #(assoc % :type symbol)))))
   "class-body"
   (fn [context symbol]
     (case symbol
       "}" (-> context (goto-state "base"))
       (-> context (goto-state-with "declaration" {:visability symbol}))))
   "field"
   (fn [context symbol]
     (-> context
         (goto-state "field")))
   "type"
   (fn [context symbol]
     (-> context
         (goto-state-with "field" #(assoc % :name symbol))))
   "base"
   (fn [context symbol]
     (case symbol
       ("class" "import" "package")                (goto-state context symbol)
       ("private" "public" "protected" "internal") (goto-state-with context "declaration" symbol)
       (throw IllegalStateException)))
   })