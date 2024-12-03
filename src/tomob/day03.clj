(ns tomob.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def mul-regex #"mul\((\d+),(\d+)\)")

(defn step1 [data]
  (->> 
    data
    slurp
    (re-seq mul-regex)
    (map #(* (Integer. (second %)) (Integer. (nth % 2))))
    (apply +)))

(defn step2 [data]
  :not-implemented)
