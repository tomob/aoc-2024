(ns tomob.day19
  (:require [clojure.string :as string]))

(defn parse-towels [data]
  (let [[t p] (string/split data #"\n\n")
        towels (string/split t #", ")
        patterns (string/split-lines p)]
    [towels patterns]))

(defn build-regex [towels]
  (re-pattern (str "(" (string/join "|" towels) ")+")))

(defn step1 [data]
  (let [[towels patterns] (parse-towels (slurp data))
        regex (build-regex towels)]
    (->> patterns
         (filter #(re-matches regex %))
         count)))

(def arrangements 
  (memoize 
    (fn [towels pattern]
      (if (empty? pattern) 1
        (->> towels
             (map
               (fn [towel]
                 (if-not (.startsWith pattern towel) 0
                   (arrangements towels (subs pattern (count towel))))))
             (reduce +))))))

(defn step2 [data]
  (let [[towels patterns] (parse-towels (slurp data))
        regex (build-regex towels)]
    (->> patterns
         (filter #(re-matches regex %))
         (map #(arrangements towels %))
         (reduce +))))
