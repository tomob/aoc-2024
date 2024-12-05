(ns tomob.day05
  (:require [clojure.string :as string]))

(defn parse-ordering [input]
  (->> (string/split-lines input)
       (map #(string/split % #"\|"))
       (map (fn [[k v]] [(Integer/parseInt k) (Integer/parseInt v)]))
       (reduce (fn [acc [k v]]
                 (update acc k #(conj (or % []) v)))
               {})))

(defn parse-updates [input]
  (->> (string/split-lines input)
       (map #(string/split % #","))
       (map #(map (fn [x] (Integer/parseInt x)) %))))

(defn get-middle [sequence]
  (nth sequence (quot (count sequence) 2)))

(defn sort-by-ordering [ordering x y]
  (let [xo (get ordering x)
        yo (get ordering y)]
    (cond
      (and (nil? xo) (nil? yo)) 0
      (and (not (nil? xo))
           (.contains xo y)) -1
      (and (not (nil? yo))
           (.contains yo x)) 1
      :else 0)))

(defn sorted? [ordering seq]
  (= seq (sort #(sort-by-ordering ordering %1 %2) seq)))

(defn step1 [data]
  (let [data (string/split (slurp data) #"\n\n")
        ordering (parse-ordering (first data))
        updates (parse-updates (second data))]
    (->> updates
         (filter #(sorted? ordering %))
         (map get-middle)
         (apply +))))

(defn step2 [data]
  (let [data (string/split (slurp data) #"\n\n")
        ordering (parse-ordering (first data))
        updates (parse-updates (second data))]
    (->> updates
         (filter #(not (sorted? ordering %)))
         (map #(sort (fn [x y] (sort-by-ordering ordering x y)) %))
         (map get-middle)
         (apply +))))
