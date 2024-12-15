(ns tomob.day15
  (:require [clojure.string :as string]
            [clojure.set :as st]))

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
        (= cell \#) nil
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

(defn calc-gps [warehouse ch]
  (apply +
         (for [y (range (alength warehouse))
               x (range (alength (aget warehouse 0)))
               :when (= ch (get-cell warehouse [x y]))]
           (+ x (* 100 y)))))

(defn run-commands [fun w r c]
  (when-not (empty? c)
    (let [[nw nr] (fun w r (first c))]
      (recur fun nw nr (rest c)))))

(defn step1 [data]
  (let [[map-data commands-data] (string/split (slurp data) #"\n\n")
        {warehouse :warehouse robot :robot} (parse-map map-data)
        commands (parse-commands commands-data)]
    (run-commands move-robot warehouse robot commands)
    (calc-gps warehouse \O)))

(defn enwiden [warehouse]
  (let [height (alength warehouse)
        width (alength (aget warehouse 0))
        new-warehouse (make-array Character/TYPE height (* 2 width))]
    (dotimes [y height]
      (dotimes [x width]
        (let [char (aget warehouse y x)
              [c1 c2] (case char
                        \# [\# \#]
                        \O [\[ \]]
                        \. [\. \.]
                        \@ [\@ \.])]
          (aset new-warehouse y (* 2 x) c1)
          (aset new-warehouse y (inc (* 2 x)) c2))))
    new-warehouse))

(defn sort-distinct [boxes direction]
  (sort (fn [[x1 y1] [x2 y2]] (case direction
                                :up (compare y1 y2)
                                :down (compare y2 y1)))
        (distinct boxes)))

(defn find-pushable-wide-boxes [warehouse pos direction]
  (loop [positions [pos]
         boxes []
         visited #{}]
    (let [current-pos (first positions)]
      (case direction
        (:left :right)  ;; This works just like in part 1, just scanning for other characters
        (let [cell (get-cell warehouse current-pos)]
          (cond
            (= cell \#) nil
            (#{\[ \]} cell) (recur #{(get-next-position current-pos direction)} (conj boxes current-pos) nil)
            (= cell \.) (reverse boxes)))

        (:up :down) ;; Up and down needs add more positions to the list of positions to accomodate for wider boxes
        (let [cell (get-cell warehouse current-pos)]
          (cond
            (= cell \#) nil ;; If any cell is blocked by a wall, all are blocked

            (#{\[ \]} cell)
            (let [other-cell-dir (if (= \[ (get-cell warehouse current-pos)) :right :left)
                  another-pos (get-next-position current-pos other-cell-dir)
                  new-positions (if (visited another-pos) (rest positions)
                                  (conj (rest positions) another-pos))]
              (recur (conj new-positions (get-next-position current-pos direction))
                     (conj boxes current-pos)
                     (conj visited current-pos)))

            (= cell \.) (if (empty? (rest positions))
                          (sort-distinct boxes direction)
                          (recur (rest positions) boxes (conj visited current-pos)))))))))

(defn move-wide-boxes! [warehouse boxes direction]
  (doseq [pos boxes]
    (let [next-pos (get-next-position pos direction)
          ch (get-cell warehouse pos)]
      (set-cell! warehouse pos \.)
      (set-cell! warehouse next-pos ch)))
  warehouse)

(defn move-wide-robot [warehouse robot direction]
  (let [next-pos (get-next-position robot direction)]
    (case (get-cell warehouse next-pos)
      \# [warehouse robot]
      \. [warehouse next-pos]
      (\[ \]) (if-let [boxes (find-pushable-wide-boxes warehouse next-pos direction)]
                (do
                  (move-wide-boxes! warehouse boxes direction)
                  [warehouse next-pos])
                [warehouse robot]))))

(defn step2 [data]
  (let [[map-data commands-data] (string/split (slurp data) #"\n\n")
        {warehouse :warehouse robot :robot} (parse-map map-data)
        warehouse (enwiden warehouse)
        robot [(* 2 (first robot)) (second robot)]
        commands (parse-commands commands-data)]
    (run-commands move-wide-robot warehouse robot commands)
    (calc-gps warehouse \[)))
