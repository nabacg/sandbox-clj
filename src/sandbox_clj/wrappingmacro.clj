(ns sandbox-clj.wrappingmacro
   [:use clojure.pprint])

(defn extract-binding [[_ name & rest]]
  [(symbol name) (first  rest)])

;todo handle funcs with comment string!
(defn extract-fn-binding [[_ name args & body]]
  [(symbol name) `(fn [~@args] ~(first body))])

(defmacro wrap-namespace-in-lambda [exprs]
  (let [def-exprs (filter #(-> % first (= 'def)) exprs)
        fn-exprs (filter #(-> % first (= 'defn)) exprs)
        main-fn  (filter #(-> % second (= '-main)) exprs)
        fn-exprs-less-main (remove #(-> %  second (= '-main)) fn-exprs)
        def-bindings (mapcat extract-binding def-exprs)
        fn-bindings (mapcat extract-fn-binding fn-exprs-less-main)
         main-fn-value (-> main-fn
                          first
                          extract-fn-binding
                          second)]
    `(fn [board#]
       (let [~@def-bindings
             ~@fn-bindings
             -main# ~main-fn-value]
         (-main# board#)))))


(comment



  (macroexpand
   '(wrap-namespace-in-lambda
     ((def a 11)
      (defn b [c d] (+ c d))
      (defn -main [board] (+ board a (b 1 2))))))


( (wrap-namespace-in-lambda
                              ((def a 11)
                               (defn b [c d] (+ c d))
                               (defn -main [board] (+ board a (b 1 2)))))  10)

                                        ;>> 24

( (wrap-namespace-in-lambda
                              ((def a 10)
                               (defn b [c d] (* c d))
                               (defn -main [board] (+ a (b 10 board)))))  100)

; 1010
  )
