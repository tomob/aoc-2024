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
       (sort-by first)
       reverse
       (map second)
       (apply str "2r")
       read-string))

(defn step1 [data]
  (let [device (parse-device (slurp data))]
    (->> device
         calculate
         build-number)))

(defn find-wrong [device]
  (let [wrong (atom #{})]
    (doseq [[out rule] device
            :when (not (integer? rule))
            :let [[op l1 l2] rule]]
      (cond
        (and (.startsWith out "z") (not= "xor" op) (not= out "z45"))
        (swap! wrong conj out)

        (and (= op "xor")
             (every? #(not (.startsWith out %)) ["x" "y" "z"])
             (every? #(not (.startsWith l1 %)) ["x" "y" "z"])
             (every? #(not (.startsWith l2 %)) ["x" "y" "z"]))
        (swap! wrong conj out)

        (and (= "and" op) (not= "x00" l1) (not= "x00" l2))
        (doseq [[subout subrule] device
                :when (not (integer? subrule))
                :let [[subop subl1 subl2] subrule]
                :when (and (or (= out subl1) (= out subl2))
                           (not= subop "or"))]
          (swap! wrong conj out))

        (= "xor" op)
        (doseq [[subout subrule] device
                :when (not (integer? subrule))
                :let [[subop subl1 subl2] subrule]
                :when (and (or (= out subl1) (= out subl2))
                           (= subop "or"))]
          (swap! wrong conj out))))
    @wrong))

(defn step2 [data]
  (let [device (parse-device (slurp data))]
    (->> device
         find-wrong
         sort
         (string/join ","))))
