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

(defn step1 [data]
  (let [init (map #(Long. %) (string/split (slurp data) #"\s+"))]
    (->> init
      (iterate blink)
      (drop 1) ;; First result of iterate is the initial value
      (take 25)
      last
      count)))

(defn step2 [data]
  :not-implemented)
