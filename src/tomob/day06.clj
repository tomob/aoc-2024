(ns tomob.day06
  (:require [clojure.string :as string]))

(defn turn [dir]
  (case dir
    :down :left
    :left :up
    :up :right
    :right :down))

(defn find-guard [lab-map]
  (first 
    (for [y (range (alength lab-map))
          x (range (alength (aget lab-map 0)))
          :let [cell (aget lab-map y x)]
          :when (#{\v \< \> \^} cell)]
      [[x y] (case cell
             \v :down
             \< :left
             \> :right
             \^ :up)])))

(defn parse [data]
  (-> data slurp string/split-lines to-array-2d))

(defn outside? [next-p max-x max-y]
  (or (< (first next-p) 0)
      (< (second next-p) 0)
      (>= (first next-p) max-x)
      (>= (second next-p) max-y)))

(defn next-pos [[x y] direction]
  (case direction
    :up    [x (dec y)]
    :down  [x (inc y)]
    :left  [(dec x) y]
    :right [(inc x) y]))

(defn get-path [lab-map [[x y] direction]]
  (let [max-y (alength lab-map)
        max-x (alength (aget lab-map 0))]
  (loop [pos [x y]
         dir direction
         path #{[pos direction]}]
    (let [[x y :as next-p] (next-pos pos dir)]
      (cond
        (outside? next-p max-x max-y) path ;; outside the map - finish
        (= \# (aget lab-map y x)) (recur pos (turn dir) path) ;; found '#', turn right
        :else (recur next-p dir (conj path [next-p dir]))))))) ;; add loc to path and continue

(defn step1 [data]
  (let [lab-map (parse data)
        guard (find-guard lab-map)]
    (->> (get-path lab-map guard)
         (map first)
         (apply hash-set) ;; Build a set to remove duplicates
         count)))

(defn array-to-map [arr]
  (let [max-y (alength arr)
        max-x (alength (aget arr 0))
        base-map (into {} 
                      (for [y (range max-y)
                            x (range max-x)]
                        [[x y] (aget arr y x)]))
        outside-right (into {} (for [y (range max-y)]
                                [[max-x y] :outside]))
        outside-bottom (into {} (for [x (range max-x)]
                                [[x max-y] :outside]))
        outside-left (into {} (for [y (range max-y)]
                               [[-1 y] :outside]))
        outside-top (into {} (for [x (range max-x)]
                              [[x -1] :outside]))]
    (merge base-map 
           outside-right 
           outside-bottom 
           outside-left 
           outside-top
           {[max-x max-y] :outside}
           {[-1 -1] :outside}
           {[max-x -1] :outside}
           {[-1 max-y] :outside})))

(defn looped? [lab-map [[x y] direction]]
  (loop [pos [x y]
         dir direction
         path #{[pos direction]}]
    (let [[x y :as next-p] (next-pos pos dir)
          ch (get lab-map [x y])]
      (cond
        (= :outside ch) false ;; left the board, not a loop
        (contains? path [next-p dir]) true ;; in the loop - finish
        (= \# ch) (recur pos (turn dir) path) ;; found '#', turn right
        :else (recur next-p dir (conj path [next-p dir])))))) ;; add loc to path and continue

(defn get-looping-points [lab-map [[x y] direction :as start-pos]]
  (let [path (get-path lab-map [[x y] direction])
        lab-map (array-to-map lab-map)]
    (reduce (fn [result [[x y] _]]
              (if (looped? (merge lab-map {[x y] \#}) start-pos)
                (conj result [x y])
                result))
            #{}
            path)))

(defn step2 [data]
  (let [lab-map (parse data)
        guard (find-guard lab-map)]
    (count (get-looping-points lab-map guard))))
