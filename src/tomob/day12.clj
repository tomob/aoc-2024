(ns tomob.day12
  (:require [clojure.string :as string]
            [clojure.set :as st]))

(defn neighbors [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn valid-position? [garden [x y] max-x max-y]
  (and (>= x 0)
       (>= y 0)
       (< x max-x)
       (< y max-y)))

(defn get-char [garden [x y]]
  (aget garden x y))

(defn flood-fill [garden start-pos visited max-x max-y]
  (loop [to-visit #{start-pos}
         visited visited
         plot #{}]
    (if (empty? to-visit)
      plot
      (let [current (first to-visit)
            current-char (get-char garden current)
            valid-neighbors (->> (neighbors current)
                               (filter #(and (valid-position? garden % max-x max-y)
                                             (not (visited %))
                                             (= (get-char garden %) current-char)))
                               set)]
        (recur (st/union (disj to-visit current) valid-neighbors)
               (conj visited current)
               (conj plot current))))))

(defn garden-plots [garden]
  (let [max-x (alength garden)
        max-y (alength (get garden 0))]
    (loop [x 0
           y 0
           visited #{}
           plots []]
      (cond
        (>= x (alength garden)) plots
        (>= y (alength (aget garden 0))) (recur (inc x) 0 visited plots)
        (visited [x y]) (recur x (inc y) visited plots)
        :else
        (let [plot (flood-fill garden [x y] visited max-x max-y)]
          (recur x (inc y) (st/union visited plot) (conj plots plot)))))))

(defn count-neighbors [plot pos]
  (count (filter #(contains? plot %) (neighbors pos))))

(defn area-peri [plot]
  {:area (count plot)
   :perimeter (->> plot
                   (map #(- 4 (count-neighbors plot %)))
                   (reduce +))})

(defn step1 [data]
  (let [garden (->> data slurp string/split-lines to-array-2d)]
    (->> garden
         garden-plots
         (map area-peri)
         (map (fn [{area :area, peri :perimeter}] (* area peri)))
         (apply +)
         )))

(defn step2 [data]
  :not-implemented)
