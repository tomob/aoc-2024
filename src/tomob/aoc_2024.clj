(ns tomob.aoc-2024
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn step-symbol [day step]
  (symbol (str "tomob." day "/step" step)))

(defn get-data-file [day data]
  (case data
    "data" (io/resource (str day "/data.txt"))
    "example" (io/resource (str day "/example.txt"))
    "step1-data" (io/resource (str day "/step1-data.txt"))
    "step1-example" (io/resource (str day "/step1-example.txt"))
    "step2-data" (io/resource (str day "/step2-data.txt"))
    "step2-example" (io/resource (str day "/step2-example.txt"))
    (throw (RuntimeException. "Invalid data set"))))

(defn run-day [day data]
  (let [step1 (requiring-resolve (step-symbol day "1"))
        step2 (requiring-resolve (step-symbol day "2"))
        data (get-data-file day data)]
    (println "Step 1: " (time (step1 data)))
    (println "Step 2: " (time (step2 data)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& [day data]]
  (run-day day (or data "data")))
