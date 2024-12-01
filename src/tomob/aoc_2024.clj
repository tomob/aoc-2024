(ns tomob.aoc-2024
  (:gen-class))

(defn step-symbol [day step]
  (symbol (str "tomob." day "/step" step)))

(defn run-day [day]
  (let [step1 (requiring-resolve (step-symbol day "1"))
        step2 (requiring-resolve (step-symbol day "2"))]
    (println "Step 1: " (step1))
    (println "Step 2: " (step2))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run-day (first args)))
