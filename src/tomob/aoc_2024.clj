(ns tomob.aoc-2024
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn step-symbol [day step]
  (symbol (str "tomob." day "/step" step)))

(defn select-file [day & files]
  (->> files
       (map #(io/resource (str day "/" %)))
       (drop-while #(nil? (io/as-file %)))
       first))

(defn get-data-file [day step data]
  (case data
    "data" (select-file day "data.txt" (str "step" step "-data.txt"))
    "example" (select-file day "example.txt" (str "step" step "-example.txt"))
    (throw (RuntimeException. "Invalid data set"))))

(defn run-day [day data]
  (let [step1 (requiring-resolve (step-symbol day "1"))
        step2 (requiring-resolve (step-symbol day "2"))
        data1 (get-data-file day 1 data)
        data2 (get-data-file day 2 data)]
    (println "Step 1: " (time (step1 data1)))
    (println "Step 2: " (time (step2 data2)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& [day data]]
  (run-day day (or data "data")))
