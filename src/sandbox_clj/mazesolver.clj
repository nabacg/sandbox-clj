(ns sandbox-clj.mazesolver
  (:require [clojure.string :as string]))

(def sample-input
"+--+--+--+--+--+--+--+--+--+--+
   |           |              |
+  +  +--+--+  +--+--+--+--+  +
|     |     |     |  |     |  |
+--+--+--+  +--+  +  +  +  +  +
|           |     |  |  |     |
+  +--+--+  +  +--+  +  +--+--+
|  |     |  |     |  |  |     |
+  +  +  +  +--+  +  +  +  +  +
|  |  |     |     |        |  |
+  +  +  +--+--+  +  +  +--+--+
|  |  |  |  |     |  |  |     |
+  +  +  +  +  +--+  +  +  +  +
|  |  |  |     |     |     |  |
+--+  +  +--+  +  +--+--+--+  +
|     |     |  |  |     |     |
+  +--+--+  +  +  +  +--+  +--+
|  |     |     |  |     |     |
+  +  +  +--+--+  +  +  +--+  +
|     |           |  |       **
+--+--+--+--+--+--+--+--+--+--")


(def adj-matrix (->> (string/split  sample-input #"\n")
         (map (comp (partial into []) #(string/split %  #"")))
         (into []))
    )

(defn get-value [graph [x y]]
  (-> graph
      (get y)
      (get x)))

(defn set-value [graph [x y] value]
  (assoc graph y  (-> (get graph y)
              (assoc x value)
              )))



(get-value adj-matrix [3 1]) ; "|"
(get-value adj-matrix [(dec  (count adj-matrix))
                       (dec  (count (get adj-matrix 19))) ])
(get-value adj-matrix [29 19])

(defn path [[x1 y1] [x2 y2]]
  (for [i (range x1 (inc x2))
        j (range y1 (inc y2))]
    [i j]))

(map (partial get-value adj-matrix) (path [1 0] [3 0]))

(defn has-edge [graph p1 p2]
  (->> (path p1 p2)
       (map (partial get-value graph))
      (every? string/blank?)))

(has-edge adj-matrix [1 1] [3 1]) ; false
(has-edge adj-matrix [1 1] [1 3])  ;true so first vertical, second horizontal..

(defn directions [step]
  [[(* step -1) 0]
   [0 (* step -1)]
   [step 0]
   [0 step]])


(directions 3)

(defn moves [[x y]]
  (map (fn [[v w]]
         [ (+ x v)
           (+ y w)])
       (directions 1)))

(moves [1 1])

(defn children [graph node]
  (->> (moves node)
       (filter (fn [[x y]] (and (>= x 0) (>= y 0))))
       (filter (partial has-edge graph node))))

(children adj-matrix [1 1])
(children adj-matrix [0 1])


(defn dfs-2 [graph success-fn acc head]
  (if (success-fn head)
    (conj acc head)
    (map (partial dfs-2 graph success-fn (conj acc head))
         (->> (children head)
              (filter #(not (contains? acc %)))))))

(defn dfs-3 [graph success-fn head]
  (loop [stack (into [] (children graph head)) explored #{head} ]
    (let [n (peek stack)]
      (if (or  (success-fn n)
               (empty? stack))
        explored
        (recur
         (into  (pop stack) (remove #(contains? explored % )
                                    (children graph n)))
         (conj explored n)
         )))))

(defn dfs [graph success-fn start]
  (let [dfs-fn (fn [visited to-visit path]
                 (let [w (first to-visit)]
                   (conj visited w)
                   (cond
                    (success-fn w) (conj path w) ;;eventually start will be either end of maze (success!)
                    (empty? to-visit) nil        ;; or dead end with no edges
                    :else
                    ;;TODO start here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    ;; need to loop over all to-visit call them recursively and find the one with solution..
                    (remove
                     nil?
                     (map
                      (dfs-fn
                       visited
                       (remove #(contains? visited %) (children graph %))
                       (conj path %))
                      (rest to-visit))))
                   ))]
    (dfs-fn #{start} (children graph start) [])))


(dfs adj-matrix #(= (get-value adj-matrix %) "*") [1 1])


(defn graph->str [graph]
  (string/join "\n"
   (map string/join graph)))

(println (graph->str adj-matrix))

(println (-> (set-value adj-matrix [1 1] "#")
             (graph->str)))

(defn print-path [graph path]
  (if (seq path)
    (print-path (set-value graph (first path) "#")
                (rest path))
    (graph->str graph)))

(println (print-path adj-matrix '([1 1] [2 1] [2 2] [2 3] [3 3])))

(defn solve [graph dfs-fn]
  (->> (dfs-fn graph #(= (get-value graph %) "*") [1 1])
       (print-path graph)
       println))

(solve adj-matrix  dfs)


(comment
  (->> (dfs adj-matrix #(= (get-value adj-matrix %) "*") [1 1])
       (print-path adj-matrix)
       println))
