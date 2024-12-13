(ns tomob.day12
  (:require [clojure.string :as string]
            [clojure.set :as st]))

(defprotocol GardenMap
  (char-at [garden [x y]] "Get char at [x y]")
  (char-valid? [this ch [x y]] "Is this char at this position?")
  (valid-position? [garden [x y]] "Is this position within bounds")
  (min-x [garden] "Lowest x index in the garden")
  (min-y [garden] "Lowest y index in the garden")
  (max-x [garden] "Highest x index in the garden")
  (max-y [garden] "Highest y index in the garden")
  (char-equal? [garden chr [x2 y2]] "Char chr equals char in position [x2 y2]"))

(defn neighbors [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn flood-fill [garden start-pos chr visited]
  (loop [to-visit #{start-pos}
         visited visited
         plot #{}]
    (if (empty? to-visit)
      plot
      (let [current (first to-visit)
            valid-neighbors (->> (neighbors current)
                               (filter #(and (valid-position? garden %)
                                             (not (visited %))
                                             (char-equal? garden chr %)))
                               set)]
        (recur (st/union (disj to-visit current) valid-neighbors)
               (conj visited current)
               (conj plot current))))))


(defn garden-plots [garden]
  (let [max-x (max-x garden)
        max-y (max-y garden)]
    (loop [x (min-x garden)
           y (min-y garden)
           visited #{}
           plots []]
      (cond
        (>= x max-x) plots
        (>= y max-y) (recur (inc x) (min-y garden) visited plots)
        (visited [x y]) (recur x (inc y) visited plots)
        :else
        (let [plot (flood-fill garden [x y] (char-at garden [x y]) visited)]
          (recur x y (st/union visited plot) (conj plots plot)))))))

(defn count-neighbors [plot pos]
  (count (filter #(contains? plot %) (neighbors pos))))

(defn area-peri [plot]
  {:area (count plot)
   :perimeter (->> plot
                   (map #(- 4 (count-neighbors plot %)))
                   (reduce +))})

(defn mark-edges [plot]
  (apply merge-with (comp flatten conj)
        (for [[x y :as pos] plot
              missing-neighbor
                    (for [[dx dy dir] [[-1 0 :left]
                                       [1 0 :right]
                                       [0 -1 :up]
                                       [0 1 :down]]
                          :let [new-x (+ x dx)
                                new-y (+ y dy)]
                          :when (not (contains? plot [new-x new-y]))]
                      {[new-x new-y] [dir]})]
          missing-neighbor)))

(defn array-garden [array]
  (let [amax-x (alength array)
        amax-y (alength (aget array 0))]
    (reify GardenMap
      (char-at [this [x y]] (aget array x y))
      (char-equal? [this chr [x2 y2]] (= chr (aget array x2 y2)))
      (char-valid? [this ch [x y]] true)
      (valid-position? [this [x y]] (and (>= x 0) (>= y 0)
                                         (< x amax-x) (< y amax-y)))
      (min-x [this] 0)
      (min-y [this] 0)
      (max-x [this] amax-x)
      (max-y [this] amax-y))))

(defn step1 [data]
  (let [garden (->> data slurp string/split-lines to-array-2d)]
    (->> garden
         array-garden
         garden-plots
         (map area-peri)
         (map (fn [{area :area, peri :perimeter}] (* area peri)))
         (apply +)
         )))

(defn map-garden [a-map]
  (let [amax-x (apply max (map (fn [[[x y] _]] x) a-map))
        amax-y (apply max (map (fn [[[x y] _]] y) a-map))
        amin-x (apply min (map (fn [[[x y] _]] x) a-map))
        amin-y (apply min (map (fn [[[x y] _]] y) a-map))]
    (reify GardenMap
      (char-at [this [x y]] (first (a-map [x y])))
      (char-equal? [this chr [x2 y2]]
        (some #(= chr %) (a-map [x2 y2])))
      (char-valid? [this ch [x y]] (char-equal? this ch [x y]))
      (valid-position? [this [x y]] (contains? a-map [x y]))
      (min-x [this] amin-x)
      (min-y [this] amin-y)
      (max-x [this] amax-x)
      (max-y [this] amax-y))))

(defn garden-plots2 [ch garden]
  (let [max-x (max-x garden)
        max-y (max-y garden)]
    (loop [x (min-x garden)
           y (min-y garden)
           visited #{}
           plots []]
      (cond
        (> x max-x) plots
        (> y max-y) (recur (inc x) (min-y garden) visited plots)
        (visited [x y]) (recur x (inc y) visited plots)
        (not (char-valid? garden ch [x y])) (recur x (inc y) visited plots)
        :else
        (let [plot (flood-fill garden [x y] ch visited)]
          (recur x y (st/union visited plot) (conj plots plot)))))))

(defn map-over-directions [garden]
  (flatten (map #(garden-plots2 % garden) [:left :right :up :down])))

(defn sides [plot]
  (->> plot
       mark-edges
       map-garden
       map-over-directions
       count))

(defn area-sides [plot]
  {:area (count plot)
   :sides (sides plot)})

(defn step2 [data]
  (let [garden (->> data slurp string/split-lines to-array-2d)]
    (->> garden
         array-garden
         garden-plots
         (map area-sides)
         (map (fn [{area :area, sides :sides}] (* area sides)))
         (apply +))))
