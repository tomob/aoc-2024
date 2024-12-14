(ns tomob.day14
  (:require [clojure.string :as string]))

(defn parse-numbers [s ch]
  (->> (string/split s ch)
       (mapv #(Integer/parseInt %))))

(defn parse-robot-line [line]
  (let [[p v] (string/split line #" ")
        [px py] (parse-numbers (subs p 2) #",")
        [vx vy] (parse-numbers (subs v 2) #",")]
    {:px px :py py :vx vx :vy vy}))

(defn parse-robots [lines]
  (mapv parse-robot-line lines))

(defn move-robot [times [dimx dimy] {:keys [px py vx vy]}]
  (letfn [(move [x v dim]
            (mod (+ x (* times v)) dim))]
    {:px (move px vx dimx) :py (move py vy dimy) :vx vx :vy vy}))

(defn split-into-quadrants [[dimx dimy] points]
  (let [center-x (quot dimx 2)
        center-y (quot dimy 2)
        in-quadrant? (fn [px py]
                      (cond
                        (= px center-x) nil
                        (= py center-y) nil
                        (and (< px center-x) (< py center-y)) 0
                        (and (> px center-x) (< py center-y)) 1
                        (and (< px center-x) (> py center-y)) 2
                        (and (> px center-x) (> py center-y)) 3
                        :else nil))]
    (->> points
         (group-by (fn [{:keys [px py]}] 
                    (in-quadrant? px py)))
         (filter (fn [[k v]] (not (nil? k))))
         (mapv second))))

(defn step1 [data]
  (let [lines (->> data slurp string/split-lines)
        [dimx dimy] (parse-numbers (first lines) #"x")
        robots (map parse-robot-line (rest lines))]
    (->> robots
         (map #(move-robot 100 [dimx dimy] %))
         (split-into-quadrants [dimx dimy])
         (map count)
         (apply *))))

(defn step2 [data]
  :not-implemented)
