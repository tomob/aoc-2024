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
         path [pos]]
    (let [[x y :as next-p] (next-pos pos dir)]
      (cond
        (outside? next-p max-x max-y) path ;; outside the map - finish
        (= \# (aget lab-map y x)) (recur pos (turn dir) path) ;; found '#', turn right
        :else (recur next-p dir (conj path next-p))))))) ;; add loc to path and continue

(defn step1 [data]
  (let [lab-map (parse data)
        guard (find-guard lab-map)]
    (->> (get-path lab-map guard)
         (apply hash-set) ;; Build a set to remove duplicates
         count)))

(defn step2 [data]
  :not-implemented)
