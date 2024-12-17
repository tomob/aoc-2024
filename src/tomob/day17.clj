(ns tomob.day17
  (:require [clojure.string :as string]
            [clojure.math :as math]))

(defn parse-machine [input]
  (let [lines (string/split-lines input)
        get-register-value (fn [line]
                            (-> line
                                (string/split #":\s+")
                                second
                                Integer/parseInt))
        program-str (-> (nth lines 4)
                       (string/split #":\s+")
                       second)
        program-array (->> (string/split program-str #",\s*")
                          (mapv #(Integer/parseInt %))
                          into-array)]
    {:a (get-register-value (nth lines 0))
     :b (get-register-value (nth lines 1))
     :c (get-register-value (nth lines 2))
     :ip 0
     :program program-array
     :program-length (alength program-array)}))

(defn combo [op machine]
  (case op
    (0 1 2 3) op
    4 (:a machine)
    5 (:b machine)
    6 (:c machine)))

(defn step
  ([machine] (assoc machine :ip (+ 2 (:ip machine))))
  ([machine k v] (step (assoc machine k v))))

(defn adv [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :a (int (quot (:a machine) (math/pow 2 operand)))) nil]))

(defn bxl [operand machine]
  [(step machine :b (int (bit-xor (:b machine) operand))) nil])

(defn bst [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :b (int (mod operand 8))) nil]))

(defn jnz [operand machine]
  (if (zero? (:a machine))
    [(step machine) nil]
    [(assoc machine :ip operand) nil]))

(defn bxc [operand machine]
  [(step machine :b (int (bit-xor (:b machine) (:c machine)))) nil])

(defn out [operand machine]
  (let [operand (combo operand machine)]
    [(step machine) (mod operand 8)]))

(defn bdv [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :b (int (quot (:a machine) (math/pow 2 operand)))) nil]))

(defn cdv [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :c (int (quot (:a machine) (math/pow 2 operand)))) nil]))

(def ops
  {0 adv
   1 bxl
   2 bst
   3 jnz
   4 bxc
   5 out
   6 bdv
   7 cdv})

(defn run-instruction [{ip :ip program :program :as machine}]
  ((ops (aget program ip)) (aget program (inc ip)) machine))

(defn run-program [{program :program length :program-length :as machine}]
  (loop [machine machine
         output []]
    (if (>= (:ip machine) length) output
      (let [[new-machine new-output] (run-instruction machine)]
        (recur new-machine (if (nil? new-output) output (conj output (int new-output))))))))

(defn step1 [data]
  (let [machine (parse-machine (slurp data))
        output (run-program machine)]
    (string/join "," output)))

(defn step2 [data]
  :not-implemented)
