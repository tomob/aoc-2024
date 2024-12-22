(ns tomob.day22
  (:require [clojure.string :as string]))

(defn s1 ^long [^long secret]
  (-> secret
      (bit-shift-left 6) ;; Multiply by 64
      (bit-xor secret)
      (mod 16777216)))

(defn s2 ^long [^long secret]
  (-> secret
      (bit-shift-right 5) ;; Devide by 32
      (bit-xor secret)
      (mod 16777216)))

(defn s3 ^long [^long secret]
  (-> secret
      (bit-shift-left 11) ;; Multiply by 2048
      (bit-xor secret)
      (mod 16777216)))

(defn next-secret ^long [^long secret]
  (-> secret s1 s2 s3))

(defn nth-secret ^long [^long n ^long secret]
  (first
    (drop n
          (iterate next-secret secret))))

(defn step1 [data]
  (let [numbers (->> data slurp string/split-lines (map Long/parseLong))]
    (->> numbers
         (map #(nth-secret 2000 %))
         (reduce +))))

(defn step2 [data]
  :not-implemented)
