(ns tomob.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn unzip 
  ([list-of-pairs] (unzip list-of-pairs [] []))
  ([list-of-pairs left right]
   (if (empty? list-of-pairs)
     [left right]
     (unzip (rest list-of-pairs) 
            (conj left (first (first list-of-pairs)))
            (conj right (second (first list-of-pairs)))))))

(defn zip [left right]
  (map vector left right))

(defn parse-input [input]
  (unzip (for [line (doall input)]
    (map #(Integer. %) (string/split line #" +")))))

(defn step1 []
  (let [[left right]
          (with-open [data (io/reader (io/resource "day01/step1-data.txt"))]
            (parse-input (line-seq data)))
        s-left (sort left)
        s-right (sort right)
        zipped (zip s-left s-right)]
    (->> zipped
         (map (fn [[a b]] (abs (- a b))))
         (apply +))))

(defn step2 []
  'not-implemented)
