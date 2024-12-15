(ns tomob.day15
  (:require [clojure.string :as string]))

(defn get-cell [warehouse [x y]]
  (aget warehouse y x))

(defn set-cell! [warehouse [x y] value]
  (aset warehouse y x value))

(defn find-robot [warehouse]
  (first 
    (for [y (range (alength warehouse))
          x (range (alength (aget warehouse 0)))
          :when (= \@ (aget warehouse y x))]
      [x y])))

(defn parse-map [data]
  (let [warehouse (to-array-2d (string/split-lines data))
        [x y :as robot] (find-robot warehouse)]
    (set-cell! warehouse [x y] \.)
    {:warehouse warehouse
     :robot robot}))

(defn parse-commands [data]
  (let [dirs {\< :left
              \> :right
              \^ :up
              \v :down}]
    (for [ch (string/trim data)
          :when (not (= \newline ch))]
      (get dirs ch))))

(defn get-next-position [[x y] direction]
  (case direction
    :up    [x (dec y)]
    :down  [x (inc y)]
    :left  [(dec x) y]
    :right [(inc x) y]))

(defn count-pushable-boxes [warehouse pos direction]
  (loop [current-pos pos
         boxes 0]
    (let [cell (get-cell warehouse current-pos)]
      (cond
        (= cell \#) nil  ; ściana
        (= cell \O) (recur (get-next-position current-pos direction) (inc boxes))
        (= cell \.) boxes))))

(defn move-boxes! [warehouse pos direction box-count]
  (loop [current-pos pos
         remaining box-count]
    (when (pos? remaining)
      (set-cell! warehouse current-pos \O)
      (recur (get-next-position current-pos direction) (dec remaining))))
  warehouse)

(defn move-robot [warehouse robot direction]
  (let [next-pos (get-next-position robot direction)]
    (case (get-cell warehouse next-pos)
      \# [warehouse robot]
      \. [warehouse next-pos]
      \O (if-let [box-count (count-pushable-boxes warehouse next-pos direction)]
           (do
             (move-boxes! warehouse 
                         (get-next-position next-pos direction) 
                         direction
                         box-count)
             (set-cell! warehouse next-pos \.)
             [warehouse next-pos])
           [warehouse robot]))))

(defn print-warehouse [warehouse robot]
  (dotimes [y (alength warehouse)]
    (dotimes [x (alength (aget warehouse y))]
      (if (= [x y] robot)
        (print \@)
        (print (aget warehouse y x))))
    (println)))

(defn calc-gps [warehouse]
  (apply +
         (for [y (range (alength warehouse))
               x (range (alength (aget warehouse 0)))
               :when (= \O (get-cell warehouse [x y]))]
           (+ x (* 100 y)))))

(defn run-commands [w r c]
  (when-not (empty? c)
    (let [[nw nr] (move-robot w r (first c))]
      (recur nw nr (rest c)))))

(defn step1 [data]
  (let [[map-data commands-data] (string/split (slurp data) #"\n\n")
        {warehouse :warehouse robot :robot} (parse-map map-data)
        commands (parse-commands commands-data)]
    (run-commands warehouse robot commands)
    (calc-gps warehouse)))

(defn step2 [data]
  :not-implemented)
