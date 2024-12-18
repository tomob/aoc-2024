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

(defn find-first-blocking [mem-map bytes n]
  (first
    (for [byte bytes
          :let [mem-map (fall-bytes mem-map [byte])
                path (find-path mem-map [0 0] [n n])]
          :when (nil? path)]
      byte)))

(defn step2 [data]
  (let [mem-map (create-map 71)
        bytes (parse-bytes (slurp data))]
    (fall-bytes mem-map (take 1024 bytes))
    (->> (find-first-blocking mem-map (drop 1024 bytes) 70)
         (string/join ","))))
