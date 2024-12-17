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
     :program-string program-str
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
    [(step machine :a (long (quot (:a machine) (math/pow 2 operand)))) nil]))

(defn bxl [operand machine]
  [(step machine :b (long (bit-xor (:b machine) operand))) nil])

(defn bst [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :b (long (mod operand 8))) nil]))

(defn jnz [operand machine]
  (if (zero? (:a machine))
    [(step machine) nil]
    [(assoc machine :ip operand) nil]))

(defn bxc [operand machine]
  [(step machine :b (long (bit-xor (:b machine) (:c machine)))) nil])

(defn out [operand machine]
  (let [operand (combo operand machine)]
    [(step machine) (mod operand 8)]))

(defn bdv [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :b (long (quot (:a machine) (math/pow 2 operand)))) nil]))

(defn cdv [operand machine]
  (let [operand (combo operand machine)]
    [(step machine :c (long (quot (:a machine) (math/pow 2 operand)))) nil]))

(def ops
  {0 adv
   1 bxl
   2 bst
   3 jnz
   4 bxc
   5 out
   6 bdv
   7 cdv})

(def op->string {0 "adv" 1 "bxl" 2 "bst" 3 "jnz" 4 "bxc" 5 "out" 6 "bdv" 7 "cdv"})
(def operand->string {0 "0  " 1 "1  " 2 "2  " 3 "3  " 4 "4|A" 5 "5|B" 6 "6|C" 7 "7  "})

(defn run-instruction [{ip :ip program :program :as machine}]
  ((ops (aget program ip)) (aget program (inc ip)) machine))

(defn print-op[{:keys [:ip :program]}]
  (print
    (op->string (aget program ip))
    (operand->string (aget program (inc ip)))
    "|| "))

(defn print-registers [{:keys [:a :b :c]}]
  (println "A" a "B" b "C" c))

(defn disassemble [program]
  (println (:program-string program))
  (for [i (range 0 (:program-length program) 2)
        :let [opcode (aget (:program program) i)
              operand (aget (:program program) (inc i))]]
    (println opcode operand ":" (op->string opcode) (operand->string operand))))

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

(defn find-input [machine x output]
  (let [start (* 8 x)]
    (for [n (range start (+ 8 start))
          :let [o (first (run-program (assoc machine :a n)))]
          :when (= output o)]
      n)))

(defn step2 [data]
  (let [machine (parse-machine (slurp data))
        program (:program-string machine)
        n (reverse (map #(Integer. %) (string/split program #",")))]
    (loop [vals (sorted-map 0 n)]
      (let [[val nv] (first vals)]
        (if (empty? nv) val
          (let [digit (first nv)
                new-vals (find-input machine val digit)]
            (if (empty? new-vals)
              (recur (dissoc vals val))
              (recur (into (dissoc vals val) (map (fn [x] [x (rest nv)]) new-vals))))))))))

