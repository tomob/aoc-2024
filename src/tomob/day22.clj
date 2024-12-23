(ns tomob.day22
  (:require [clojure.string :as string]))

(defn s1 ^long [^long secret]
  (-> secret
      (bit-shift-left 6) ;; Multiply by 64
      (bit-xor secret)
      (bit-and 16777215)))

(defn s2 ^long [^long secret]
  (-> secret
      (bit-shift-right 5) ;; Devide by 32
      (bit-xor secret)
      (bit-and 16777215)))

(defn s3 ^long [^long secret]
  (-> secret
      (bit-shift-left 11) ;; Multiply by 2048
      (bit-xor secret)
      (bit-and 16777215)))

(defn next-secret ^long [^long secret]
  (-> secret s1 s2 s3))

(defn nth-secret ^long [^long n ^long secret]
  (first
    (drop n
          (iterate next-secret secret))))

(defn secrets [^long n ^long secret]
  (take n (iterate next-secret secret)))

(defn step1 [data]
  (let [numbers (->> data slurp string/split-lines (map Long/parseLong))]
    (->> numbers
         (map #(nth-secret 2000 %))
         (reduce +))))

(defn diffs [prices]
  (map (fn [previous current] [current (- current previous)])
       prices
       (rest prices)))

(defn sequences [prices+diffs]
  (apply merge-with (fn [x _] x)
        (map (fn [v w x y]
               (let [key (vec (map second [v w x y]))]
                 {key (first y)}))
             prices+diffs
             (drop 1 prices+diffs)
             (drop 2 prices+diffs)
             (drop 3 prices+diffs))))

(defn find-best-sequence [all]
  (let [sequences (distinct (mapcat keys all))]
    (reduce 
      (fn [[s1 v1] [s2 v2]] (if (> v1 v2) [s1 v1] [s2 v2]))
      (for [s sequences]
        [s (reduce + (map #(get % s 0) all))]))))

(defn step2 [data]
  (let [numbers (->> data slurp string/split-lines (map Long/parseLong))]
    (->> numbers
         (map #(secrets 2001 %))
         (map #(map (fn [x] (mod x 10)) %))
         (map diffs)
         (map sequences)
         find-best-sequence)))
