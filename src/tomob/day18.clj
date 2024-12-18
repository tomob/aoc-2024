(ns tomob.day18
  (:require [clojure.string :as string]))

(defn create-map [n]
  (to-array-2d (vec (repeat n (vec (repeat n \.))))))

(defn parse-bytes [data]
  (->> data
       string/split-lines
       (map #(string/split % #","))
       (mapv (fn [[x y]] [(Integer. x) (Integer. y)]))))

(defn fall-bytes [mem-map bytes]
  (doseq [[x y] bytes]
    (aset mem-map y x \#))
  mem-map)

(defn get-neighbors [[x y] mem-map size]
  (let [possible-moves [[-1 0] [1 0] [0 -1] [0 1]]]
    (->> possible-moves
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [[nx ny]]
                  (and (>= nx 0) (< nx size)
                       (>= ny 0) (< ny size)
                       (not= \# (aget mem-map ny nx))))))))

(defn find-path [mem-map start end]
  (let [size (alength mem-map)]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
           visited #{start}]
      (if (empty? queue)
        nil
        (let [[current steps] (peek queue)
              queue (pop queue)]
          (if (= current end)
            steps
            (let [neighbors (get-neighbors current mem-map size)
                  unvisited (remove visited neighbors)]
              (recur
                (into queue (map #(vector % (inc steps)) unvisited))
                (into visited unvisited)))))))))

(defn step1 [data]
  (let [mem-map (create-map 71)
        bytes (parse-bytes (slurp data))]
    (-> (fall-bytes mem-map (take 1024 bytes))
        (find-path [0 0] [70 70]))))

(defn copy-2d-array [array]
  (let [rows (alength array)
        cols (alength (aget array 0))
        new-array (make-array Character/TYPE rows cols)]
    (dotimes [i rows]
      (dotimes [j cols]
        (aset new-array i j (aget array i j))))
    new-array))

(defn test-path-blocked? [mem-map bytes n end-idx]
  (let [test-map (copy-2d-array mem-map)]
    (fall-bytes test-map (take end-idx bytes))
    (nil? (find-path test-map [0 0] [n n]))))

(defn find-first-blocking [mem-map bytes n]
  (let [bytes-vec (vec bytes)]
    (loop [left 0
           right (count bytes-vec)]
      (if (>= left right)
        (get bytes-vec left)
        (let [mid (quot (+ left right) 2)]
          (if (test-path-blocked? mem-map bytes-vec n (inc mid))
            (recur left mid)
            (recur (inc mid) right)))))))

(defn step2 [data]
  (let [mem-map (create-map 71)
        bytes (parse-bytes (slurp data))]
    (fall-bytes mem-map (take 1024 bytes))
    (->> (find-first-blocking mem-map (drop 1024 bytes) 70)
         (string/join ","))))
