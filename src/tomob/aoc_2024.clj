(ns tomob.aoc-2024
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:gen-class))

(defn step-symbol [day step]
  (symbol (str "tomob." day "/step" step)))

(defn select-file [day & files]
  (->> files
       (map #(io/resource (str day "/" %)))
       (drop-while #(nil? (io/as-file %)))
       first))

(defn get-data-file [day step data]
  (cond
    (= data "data") (select-file day "data.txt" (str "step" step "-data.txt"))
    (= data "example") (select-file day "example.txt" (str "step" step "-example.txt"))
    (string/starts-with? data "example") (select-file day (str data ".txt")) ;; If more examples are needed
    :else (throw (RuntimeException. "Invalid data set"))))

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

(defn create-day [{number :number :as all}]
  (let [number (if (nil? number) (Integer. all) number)
        resource-dir (io/as-file (format "resources/day%02d" number))
        example-file (io/as-file (format "resources/day%02d/example.txt" number))
        data-file (io/as-file (format "resources/day%02d/data.txt" number))
        code-file (io/as-file (format "src/tomob/day%02d.clj" number))]
    (.mkdir resource-dir)
    (.createNewFile example-file)
    (.createNewFile data-file)
    (.createNewFile code-file)
    (spit code-file (format
                      "(ns tomob.day%02d
  (:require [clojure.string :as string]))

(defn step1 [data]
  :not-implemented)

(defn step2 [data]
  :not-implemented)"
                      number))))
