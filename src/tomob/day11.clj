(ns tomob.day11
  (:require [clojure.string :as string]))

(defn blink [stones]
  (flatten
    (for [stone stones
          :let [stone-str (str stone)
                stone-len (count stone-str)]]
      (cond
        (zero? stone) 1
        (even? stone-len)
        (let [half (/ stone-len 2)]
          (map #(-> % string/join Long.) (split-at half stone-str)))
        :else (* 2024 stone)))))

(defn blink2 [stone-map]
  (apply
    merge-with +
    (for [[stone cnt] stone-map]
      (cond
        (= stone "0") {"1" cnt}

        (even? (count stone))
        (apply
          merge-with + 
          (map (fn [half] {(-> half string/join Long. str) cnt})
               (split-at (/ (count stone) 2) stone)))

        :else {(str (* 2024 (Long. stone))) cnt}))))

(defn step1 [data]
  (let [init (map #(Long. %) (string/split (slurp data) #"\s+"))]
    (->> init
      (iterate blink)
      (drop 25)
      first
      count)))

(defn step2 [data]
  (let [init (into {} (map (fn [x] [x 1]) (string/split (slurp data) #"\s+")))]
    (->> init
      (iterate blink2)
      (drop 75)
      first
      vals
      (apply +))))
