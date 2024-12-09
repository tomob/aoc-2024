(ns tomob.day09
  (:require [clojure.string :as string]))

(defn expand [numbers]
  (loop [nums numbers
         block-num 0
         data true
         result []]
    (cond
      (empty? nums) (flatten (filter #(not (empty? %)) result))
      data (let [block (repeat (first nums) block-num)]
             (recur (rest nums) (inc block-num) false (conj result block)))
      :else (let [space (repeat (first nums) -1)]
              (recur (rest nums) block-num true (conj result space))))))

(defn compact [numbers]
  (let [numbers (long-array numbers)]
    (loop [destination 0
           source (dec (alength numbers))
           result []]
      (let [current (aget numbers destination)]
        (cond
          (> destination source) result
          (= (aget numbers source) -1) (recur destination (dec source) result) ;; '.' on the right side
          (= current -1) (recur (inc destination) (dec source) (conj result (aget numbers source)))
          :else (recur (inc destination) source (conj result current)))))))

(defn checksum [numbers]
  (->> numbers
       (map-indexed *)
       (apply +)))

(defn step1 [data]
  (->> (slurp data)
       string/trimr
       (mapv #(Character/digit % 10))
       expand
       compact
       checksum))

(defn step2 [data]
  :not-implemented)
