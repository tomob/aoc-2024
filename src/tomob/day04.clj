(ns tomob.day04
  (:require [clojure.string :as string]))

(defn get-sequences [x y max-x max-y]
  (let [directions [[1 0]
                    [-1 0]
                    [0 1]
                    [0 -1]
                    [1 1]
                    [1 -1]
                    [-1 1]
                    [-1 -1]]
        valid-coord? (fn [[x y]]
                      (and (>= x 0) (>= y 0)
                           (< x max-x) (< y max-y)))
        make-sequence (fn [[dx dy]]
                       (filter valid-coord?
                               (for [i (range 4)]
                                 [(+ x (* i dx))
                                  (+ y (* i dy))])))]
    (filter #(= 4 (count %)) (map make-sequence directions))))

(defn step1 [data]
  (let [d (-> data slurp string/split-lines to-array-2d)
        max-x (alength (aget d 0))
        max-y (alength d)]
    (count 
      (for [y (range (alength d))
            x (range (alength (aget d 0)))
            :when (= \X (aget d x y))
            potential (get-sequences x y max-x max-y)
            :let [word (apply str (map (fn [[x y]] (aget d x y)) potential))]
            :when (= "XMAS" word)]
        :found))))


(defn step2 [data]
  :not-implemented)
