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

(defn step2 [data]
  :not-implemented)
