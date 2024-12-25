(ns tomob.day25
  (:require [clojure.string :as string]))

(defn count-pins [pattern]
  (let [width (alength (aget pattern 0))]
    (mapv
     (fn [col]
       (dec (count (filter #(= \# (aget % col)) pattern))))
     (range width))))

(defn lock? [pattern]
  (= \# (aget pattern 0 0)))

(defn parse-locks-and-keys [data]
  (let [locks (atom [])
        keys (atom [])]
    (doseq [pattern (string/split data #"\n\n")
            :let [pattern (to-array-2d (string/split-lines pattern))]]
      (if (lock? pattern)
        (swap! locks conj (count-pins pattern))
        (swap! keys conj (count-pins pattern))))
    [@locks @keys]))

(defn matches? [lock key]
  (every? (fn [s] (<= s 5))
          (map + lock key)))

(defn count-matching-locks-and-keys [locks keys]
  (loop [l locks
         k keys
         sum 0]
    (cond
      (empty? l) sum
      (empty? k) (recur (rest l) keys sum)
      (matches? (first l) (first k)) (recur l (rest k) (inc sum))
      :else (recur l (rest k) sum))))

(defn step1 [data]
  (let [[locks keys] (parse-locks-and-keys (slurp data))]
    (count-matching-locks-and-keys locks keys)))

(defn step2 [data]
  :not-needed)
