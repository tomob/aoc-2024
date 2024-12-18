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

(defn get-neighbors [[x y] mem-map]
  (let [height (alength mem-map)
        width (alength (aget mem-map 0))
        possible-moves [[-1 0] [1 0] [0 -1] [0 1]]]
    (->> possible-moves
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [[nx ny]]
                  (and (>= nx 0) (< nx width)
                       (>= ny 0) (< ny height)
                       (not= \# (aget mem-map ny nx))))))))

(defn find-path [mem-map start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{start}
         parent {}]
    (if (empty? queue)
      nil ; No path
      (let [current (peek queue)
            queue (pop queue)]
        (if (= current end)
          (loop [pos end
                 path []]
            (if (nil? pos)
              (reverse path)
              (recur (get parent pos) (conj path pos))))

          (let [neighbors (get-neighbors current mem-map)
                unvisited (remove visited neighbors)]
            (recur
              (into queue unvisited)
              (into visited unvisited)
              (into parent (map #(vector % current) unvisited)))))))))

(defn step1 [data]
  (let [mem-map (create-map 71)
        bytes (parse-bytes (slurp data))]
    (-> (fall-bytes mem-map (take 1024 bytes))
        (find-path [0 0] [70 70])
        count
        dec)))

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
