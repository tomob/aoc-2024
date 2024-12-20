(ns tomob.day20
  (:require [clojure.string :as string]))

(defn array->map [a]
  (into {}
        (for [y (range (alength a))
              x (range (alength (aget a 0)))]
          [[x y] (aget a y x)])))

(defn find-char [m ch]
  (first 
    (for [y (range (alength m))
          x (range (alength (aget m 0)))
          :when (= ch (aget m y x))]
      [x y])))

(defn parse-racetrack [data]
  (let [m (->> data string/split-lines to-array-2d)]
    [(array->map m) (find-char m \S) (find-char m \E) [(alength (aget m 0)) (alength m)]]))

(defn get-neighbors [[x y] racetrack]
  (let [possible-moves [[-1 0] [1 0] [0 -1] [0 1]]]
    (->> possible-moves
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [[nx ny]]
                  (not= \# (racetrack [nx ny])))))))

(defn count-path
  ([racetrack start end] (count-path racetrack start end Integer/MAX_VALUE))
  ([racetrack start end max-length]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
           visited #{start}]
      (if (empty? queue)
        nil
        (let [[current steps] (peek queue)
              queue (pop queue)]
          (cond
            (= current end) steps
            (> steps max-length) nil
            :else (let [neighbors (get-neighbors current racetrack)
                  unvisited (remove visited neighbors)]
              (recur
                (into queue (map #(vector % (inc steps)) unvisited))
                (into visited unvisited)))))))))

(defn is-cheat? [racetrack [x y :as pos]]
  (and (= \# (racetrack pos))
       (or (and (not= \# (racetrack [(inc x) y]))
                (not= \# (racetrack [(dec x) y])))
           (and (not= \# (racetrack [x (inc y)]))
                (not= \# (racetrack [x (dec y)]))))))

(defn find-cheats [racetrack start end [max-x max-y] at-least]
  (let [longest (count-path racetrack start end)
        max-length (- longest at-least)]
    (for [x (range 1 (dec max-x))
          y (range 1 (dec max-y))
          :when (is-cheat? racetrack [x y])
          :let [l (count-path (assoc racetrack [x y] \.) start end max-length)]
          :when (not (nil? l))
          :when (>= (- longest l) at-least)]
      [[x y] (- longest l)])))

(defn step1 [data]
  (let [[racetrack start end maxs] (parse-racetrack (slurp data))]
    (->> (find-cheats racetrack start end maxs 100)
         count)))

(defn step2 [data]
  :not-implemented)
