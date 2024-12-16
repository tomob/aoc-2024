(ns tomob.day16
  (:require [clojure.string :as string]))

(defn find-pos [maze ch]
  (first 
    (for [y (range (alength maze))
          x (range (alength (aget maze 0)))
          :when (= ch (aget maze y x))]
      [x y])))

(defn get-next-positions [maze [x y] dir]
  (let [moves {:north [[x (dec y)] :north]
               :south [[x (inc y)] :south]
               :east  [[(inc x) y] :east]
               :west  [[(dec x) y] :west]}
        possible-dirs (case dir
                       :north [:north :east :west]
                       :south [:south :east :west]
                       :east  [:east :north :south]
                       :west  [:west :north :south])
        valid? (fn [[[nx ny] _]]
                 (and (>= nx 0) (>= ny 0)
                      (< ny (alength maze))
                      (< nx (alength (aget maze 0)))
                      (not= \# (aget maze ny nx))))]
    (->> possible-dirs
         (map moves)
         (filter valid?))))

(defn step-cost [curr-dir next-dir]
  (if (= curr-dir next-dir) 1 1001))

(defn dijkstra [maze start start-dir end]
  (loop [q (sorted-set [0 start start-dir])
         dist {[start start-dir] 0}]
    (if (empty? q) dist
      (let [[d current-pos curr-dir] (first q)
            [nq ndist] (reduce
                         (fn [[q d] [next-pos next-dir alt]]
                             [(conj q [alt next-pos next-dir]) (assoc d [next-pos next-dir] alt)])
                         [(disj q (first q)) dist]
                         (for [[next-pos next-dir cost] (get-next-positions maze current-pos curr-dir)
                               :let [alt (+ (get dist [current-pos curr-dir] Integer/MAX_VALUE) (step-cost curr-dir next-dir))]
                               :when (< alt (get dist [next-pos next-dir] Integer/MAX_VALUE))]
                           [next-pos next-dir alt]
                         ))]
        (recur nq ndist)))))

(defn print-maze [maze path]
  (doseq [[[x y] dir] path]
    (case dir
      :north (aset maze y x \^)
      :south (aset maze y x \v)
      :east (aset maze y x \>)
      :west (aset maze y x \<)))
  (dotimes [y (alength maze)]
    (dotimes [x (alength (aget maze y))]
        (print (aget maze y x)))
    (println)))

(defn step1 [data]
  (let [maze (-> data slurp string/split-lines to-array-2d)
        start (find-pos maze \S)
        end (find-pos maze \E)
        dist (dijkstra maze start :east end)]
    (->> dist
         (filter (fn [[[pos dir] _]] (= pos end)))
         (map (fn [[k v]] v))
         sort
         first)))

(defn step2 [data]
  :not-implemented)
