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

(defn memo-arrangements [towels num pattern]
  (let [cache (atom {})]
    (letfn [(memo-fn [p]
              (if-let [[_ v] (find @cache p)] v
                (let [result (if (empty? p) 1
                                (->> towels
                                     (map
                                       (fn [towel]
                                         (if-not (.startsWith p towel) 0
                                           (memo-fn (subs p (count towel))))))
                                     (reduce +)))]
                  (swap! cache assoc p result)
                  result)))]
      (memo-fn pattern))))

(defn step2 [data]
  (let [[towels patterns] (parse-towels (slurp data))
        regex (build-regex towels)]
    (->> patterns
         (filter #(re-matches regex %))
         (map-indexed #(memo-arrangements towels %1 %2))
         (apply +))))
