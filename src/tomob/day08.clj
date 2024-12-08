(ns tomob.day08
  (:require [clojure.string :as string]))

(defn antenna-pairs [ch arr max-x max-y]
  (let [positions (for [y (range max-y)
                        x (range max-x)
                       :when (= ch (aget arr y x))]
                   [x y])]
    (for [pos1 positions
          pos2 positions
          :when (not= pos1 pos2)]
      [pos1 pos2])))

(defn get-antenna-types [arr max-x max-y]
  (set (for [y (range max-y)
             x (range max-x)
             :let [ch (aget arr y x)]
             :when (not= ch \.)]
         ch)))

(defn get-antinode [[[x1 y1] [x2 y2]]]
  [(+ x2 (- x2 x1)) (+ y2 (- y2 y1))])

(defn in-map? [[x y] max-x max-y]
  (and (>= x 0) (>= y 0) (< x max-x) (< y max-y)))

(defn step1 [data]
  (let [city-map (-> data slurp string/split-lines to-array-2d)
        max-x (alength (aget city-map 0))
        max-y (alength city-map)
        antenna-types (get-antenna-types city-map max-x max-y)]
    (->>
      (for [a antenna-types
            pair (antenna-pairs a city-map max-x max-y)
            :let [antinode (get-antinode pair)]
            :when (in-map? antinode max-x max-y)]
        antinode)
      (into #{})
      count)))

(defn step2 [data]
  :not-implemented)
