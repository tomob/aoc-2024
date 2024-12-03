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

(defn filter-disabled [enabled result instructions]
  (if (empty? instructions) result
    (let [[instruction _ a b] (first instructions)]
      (cond
        (= instruction "do()") (recur true result (rest instructions))
        (= instruction "don't()") (recur false result (rest instructions))
        (not enabled) (recur enabled result (rest instructions))
        :else (recur enabled (conj result (* (Integer. a) (Integer. b))) (rest instructions))))))

(defn step2 [data]
  (->> 
    data
    slurp
    (re-seq mul-do-dont-regex)
    (filter-disabled true [])
    (apply +)))
