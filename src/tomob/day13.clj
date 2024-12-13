(ns tomob.day13
  (:require [clojure.string :as string]))

(defn parse-numbers [line]
  (->> (re-seq #"[-\d]+" line)
       (mapv #(Long. %))))

(defn parse-machine [machine-description]
  (let [lines (string/split-lines machine-description)
        [a1 a2] (parse-numbers (first lines))
        [b1 b2] (parse-numbers (second lines))
        [x1 x2] (parse-numbers (last lines))]
    [[a1 b1 x1] [a2 b2 x2]]))

(defn solve-linear-system 
  [[a b c] [d e f]]
  (let [determinant (- (* a e) (* b d))]
    (if (zero? determinant)
      nil
      (let [x (/ (- (* c e) (* b f)) determinant)
            y (/ (- (* a f) (* c d)) determinant)]
        {:a x :b y}))))

(defn is-valid? [{a :a b :b :as solution}]
  (cond
    (nil? solution) false
    :else (and (integer? a) (integer? b))))

(defn less-than-100 [{a :a b :b :as solution}]
  (and (<= a 100) (<= b 100)))

(defn token-cost [{a :a b :b}]
  (+ (* 3 a) b))

(defn step1 [data]
  (let [machines-descriptions (string/split (slurp data) #"\n\n")]
    (->> machines-descriptions
         (map parse-machine)
         (map #(apply solve-linear-system %))
         (filter is-valid?)
         (filter less-than-100)
         (map token-cost)
         (apply +))))

(defn add-long [[[a b c] [d e f]]]
  [[a b (+ c 10000000000000)] [d e (+ f 10000000000000)]])

(defn step2 [data]
  (let [machines-descriptions (string/split (slurp data) #"\n\n")]
    (->> machines-descriptions
         (map parse-machine)
         (map add-long)
         (map #(apply solve-linear-system %))
         (filter is-valid?)
         (map token-cost)
         (apply +))))
