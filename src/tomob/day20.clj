(ns tomob.day20
  (:require [clojure.string :as string]))

(defn array->map [^objects a]
  (into {}
        (for [y (range (alength a))
              x (range (alength ^objects (aget a 0)))]
          [[x y] (aget a y x)])))

(defn find-char [^objects m ch]
  (first 
    (for [y (range (alength m))
          x (range (alength ^objects (aget m 0)))
          :when (= ch (aget m y x))]
      [x y])))

(defn parse-racetrack [data]
  (let [^objects m (->> data string/split-lines to-array-2d)
        start (find-char m \S)
        end (find-char m \E)
        racetrack (array->map m)
        racetrack (assoc racetrack start \. end \.)]
    [racetrack start end [(alength ^objects (aget m 0)) (alength m)]]))

(def possible-moves
  [[-1 0] [1 0] [0 -1] [0 1]])

(defn get-neighbors [[x y] racetrack]
  (->> possible-moves
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       (filter (fn [[nx ny]]
                 (= \. (racetrack [nx ny]))))))

(defn find-path [racetrack start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start [[start 0]]])
         visited #{start}]
    (if (empty? queue)
      nil
      (let [[current path] (peek queue)
            current-length (count path)
            queue (pop queue)]
        (cond
          (= current end) path
          :else (let [neighbors (get-neighbors current racetrack)
                      unvisited (remove visited neighbors)]
                  (recur
                    (into queue (map #(vector % (conj path [% current-length])) unvisited))
                    (into visited unvisited))))))))

(defn manhattan [[^int x1 ^int y1] [^int x2 ^int y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn neighbors-within [path pos mn mx]
  (for [[coord _ :as point] path
        :when (>= mx (manhattan pos coord) mn)]
    point))

(defn find-cheats [racetrack start end [mn mx] at-least]
  (let [path (find-path racetrack start end)]
    (for [[pos length] path
          [neighbor n-length] (neighbors-within path pos mn mx)
          :when (> n-length length)
          :let [saved (- n-length length (manhattan pos neighbor))]
          :when (>= saved at-least)]
      [pos neighbor saved])))

(defn step1 [data]
  (let [[racetrack start end maxs] (parse-racetrack (slurp data))]
    (->> (find-cheats racetrack start end [2 2] 100)
         (group-by (fn [[_ _ l]] l))
         (map (fn [[k v]] (count v)))
         (reduce +))))

(defn step2 [data]
  (let [[racetrack start end maxs] (parse-racetrack (slurp data))]
    (->> (find-cheats racetrack start end [2 20] 100)
         (group-by (fn [[_ _ l]] l))
         (map (fn [[k v]] (count v)))
         (reduce +))))
