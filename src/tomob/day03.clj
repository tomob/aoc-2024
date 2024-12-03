(ns tomob.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def mul-regex #"mul\((\d+),(\d+)\)")
(def mul-do-dont-regex #"(mul\((\d+),(\d+)\)|do\(\)|don't\(\))")

(defn step1 [data]
  (->> 
    data
    slurp
    (re-seq mul-regex)
    (map #(* (Integer. (second %)) (Integer. (nth % 2))))
    (apply +)))

(defn filter-disabled [enabled instructions]
  (if (empty? instructions) '()
    (let [[instruction _ a b] (first instructions)]
      (cond
        (= instruction "do()") (filter-disabled true (rest instructions))
        (= instruction "don't()") (filter-disabled false (rest instructions))
        (not enabled) (filter-disabled enabled (rest instructions))
        :else (cons (* (Integer. a) (Integer. b)) (filter-disabled enabled (rest instructions)))))))

(defn step2 [data]
  (->> 
    data
    slurp
    (re-seq mul-do-dont-regex)
    (filter-disabled true)
    (apply +)
    ))
