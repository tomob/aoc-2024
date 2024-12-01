(ns tomob.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn unzip 
  ([list-of-pairs] (unzip list-of-pairs [] []))
  ([list-of-pairs left right]
   (if (empty? list-of-pairs)
     [left right]
     (recur (rest list-of-pairs) 
            (conj left (first (first list-of-pairs)))
            (conj right (second (first list-of-pairs)))))))

(defn parse-input [input]
  (unzip (for [line (doall input)]
    (map #(Integer. %) (string/split line #" +")))))

(defn step1 []
  (let [[left right]
          (with-open [data (io/reader (io/resource "day01/step1-data.txt"))]
            (parse-input (line-seq data)))]
    (apply + (mapv (comp abs - ) (sort left) (sort right)))))

(defn step2 []
  (let [[left right]
          (with-open [data (io/reader (io/resource "day01/step1-data.txt"))]
            (parse-input (line-seq data)))
        occ (frequencies right)]
    (->> left
         (map #(* %1 (get occ %1 0)))
         (apply +))))
