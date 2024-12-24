(ns tomob.day24
  (:require [clojure.string :as string]))

(defn parse-device [data]
  (let [[xs gates] (string/split data #"\n\n")
        xs (string/split-lines xs)
        gates (string/split-lines gates)
        pattern #"(\w+) (AND|XOR|OR) (\w+) -> (\w+)"]
      (into {}
            (concat 
            (for [x xs
                  :let [[x val] (string/split x #": ")]]
              [x (Integer. val)])
            (for [gate gates
                  :let [[_ g1 op g2 result] (re-matches pattern gate)]]
              [result [(string/lower-case op) g1 g2]])))))

(defn build-expression [device elem]
  (let [formula (get device elem)]
    (if (integer? formula)
      formula
      (let [[op g1 g2] formula]
        (list (symbol (str "bit-" op)) 
          (build-expression device g1)
          (build-expression device g2))))))

(defn only-z [[k _]]
  (.startsWith k "z"))

(defn calculate [device]
  (->> device
       (filter only-z)
       (map (fn [[k _]] [k (build-expression device k)]))
       (map (fn [[k ex]] [(Integer. (subs k 1)) (eval ex)]))))

(defn build-number [zs]
  (->> zs
       (sort-by first >)
       (map second)
       (apply str "2r")
       read-string))

(defn step1 [data]
  (let [device (parse-device (slurp data))]
    (->> device
         calculate
         build-number)))

(defn step2 [data]
  :not-implemented)
