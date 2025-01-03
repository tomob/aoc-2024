(ns tomob.day10
  (:require [clojure.string :as string]))

(defn parse-map [data]
  (->> data
       slurp
       string/split-lines
       (map #(map (fn [x] (Character/digit x 10)) %))
       to-array-2d))

(defn find-all [topo-map val max-x max-y]
  (for [y (range max-y)
        x (range max-x)
       :when (= val (aget topo-map y x))]
   [x y]))

(defn reachable? [topo-map start end max-x max-y]
  (let [visited (atom #{})
        valid-move? (fn [[x y]]
                     (and (>= x 0) (< x max-x)
                          (>= y 0) (< y max-y)))
        neighbors (fn [[x y]]
                   (filter valid-move?
                          [[(inc x) y] [(dec x) y]
                           [x (inc y)] [x (dec y)]]))]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)]
      (cond
        (empty? queue) false
        (= (peek queue) end) true
        :else
        (let [current (peek queue)
              [cx cy] current
              current-val (aget topo-map cy cx)
              next-positions (->> (neighbors current)
                                  (filter #(not (@visited %)))
                                  (filter (fn [[x y]]
                                          (= (aget topo-map y x)
                                             (inc current-val)))))]
          (swap! visited conj current)
          (recur (into (pop queue) next-positions)))))))

(defn step1 [data]
  (let [topo-map (parse-map data)
        max-x (alength (aget topo-map 0))
        max-y (alength topo-map)]
    (count
      (for [zero (find-all topo-map 0 max-x max-y)
            nine (find-all topo-map 9 max-x max-y)
            :when (reachable? topo-map zero nine max-x max-y)]
        1))))

(defn count-trailhead-rating [topo-map start end max-x max-y]
  (let [memo (atom {})
        valid-move? (fn [[x y]]
                     (and (>= x 0) (< x max-x)
                          (>= y 0) (< y max-y)))
        neighbors (fn [[x y]]
                   (filter valid-move?
                          [[(inc x) y] [(dec x) y]
                           [x (inc y)] [x (dec y)]]))]
    (letfn [(count-paths [current]
              (if-let [cached (@memo current)]
                cached
                (let [[cx cy] current
                      result (cond
                              (= current end) 1
                              :else
                              (let [current-val (aget topo-map cy cx)
                                    next-positions (->> (neighbors current)
                                                      (filter (fn [[x y]]
                                                              (= (aget topo-map y x)
                                                                 (inc current-val)))))]
                                (reduce + 0 (map count-paths next-positions))))]
                  (swap! memo assoc current result)
                  result)))]
      (count-paths start))))

(defn step2 [data]
  (let [topo-map (parse-map data)
        max-x (alength (aget topo-map 0))
        max-y (alength topo-map)]
    (apply +
      (for [zero (find-all topo-map 0 max-x max-y)
            nine (find-all topo-map 9 max-x max-y)]
        (count-trailhead-rating topo-map zero nine max-x max-y)))))
