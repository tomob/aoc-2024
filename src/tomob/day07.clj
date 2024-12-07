(ns tomob.day07
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (let [[key nums] (string/split line #": ")
        key (Long/parseLong key)
        nums (map Long/parseLong (string/split nums #"\s+"))]
    [key nums]))

(defn parse-input [input]
  (->> input
       slurp
       string/split-lines
       (map parse-line)))

(defn || [x y]
  (Long/parseLong (str x y)))

(defn calculable?
  ([[result numbers]]
   (calculable? result (first numbers) (rest numbers) false))

  ([result calculation numbers add-concat]
   (cond
     (empty? numbers) (= result calculation)
     (> calculation result) false ;; early abort, numbers can't decrease
     :else 
     (or (calculable? result (+ calculation (first numbers)) (rest numbers) add-concat)
         (calculable? result (* calculation (first numbers)) (rest numbers) add-concat)
         (and add-concat (calculable? result (|| calculation (first numbers)) (rest numbers) add-concat))))))

(defn concat-calculable? [[result numbers]]
  (calculable? result (first numbers) (rest numbers) true))

(defn step1 [data]
  (->> (parse-input data)
       (filter calculable?)
       (map first)
       (apply +)))

(defn step2 [data]
  (->> (parse-input data)
       (filter concat-calculable?)
       (map first)
       (apply +)))
