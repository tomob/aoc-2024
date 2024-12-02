(ns tomob.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn safe-mono? [report diff-fn]
  (loop [previous (first report)
         r (rest report)]
    (cond
      (empty? r) true
      (contains? #{1 2 3} (diff-fn previous (first r))) (recur (first r) (rest r))
      :else false)))

(defn safe? [report]
  (cond
    (empty? report) true
    (== 1 (count report)) true
    (> (first report) (second report)) (safe-mono? report #(- %1 %2))
    (< (first report) (second report)) (safe-mono? report #(- %2 %1))
    :else false))

(defn common [filter-fn]
  (->> (io/resource "day02/step1-data.txt")
       slurp
       string/split-lines
       (map #(string/split % #" +"))
       (map #(map (fn [x] (Integer. x)) %))
       (filter filter-fn)
       count))

(defn step1 []
  (common safe?))

(defn remove-one [coll]
  (map-indexed (fn [idx _]
                 (concat (take idx coll) (drop (inc idx) coll)))
               coll))

(defn safe-but-one? [report]
  (if (safe? report) true
    (some safe? (remove-one report))))

(defn step2 []
  (common safe-but-one?))
