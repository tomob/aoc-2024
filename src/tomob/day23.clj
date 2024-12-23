(ns tomob.day23
  (:require [clojure.string :as string]))

(defn parse-network [data]
  (apply
    merge-with 
    concat
    (for [line (string/split-lines data)
          :let [[x y] (string/split line #"-")]]
      {x [y] y [x]})))

(defn find-cycle-from-node
  [graph start visited path cycles]
  (let [current (last path)]
    (doseq [neighbor (get graph current)]
      (cond
        ;; Cycle found - check if it contains start node
        (and (contains? visited neighbor)
             (= neighbor start)
             (> (count path) 2))
        (swap! cycles conj path)

        ;; Ignore cycles longer than 3
        (> (count path) 2)
        nil

        (not (contains? visited neighbor))
        (find-cycle-from-node
          graph
          start
          (conj visited neighbor)
          (conj path neighbor)
          cycles))))
  cycles)

(defn find-cycles-dfs [graph]
  (let [cycles (atom #{})]
    (doseq [start (keys graph)]
      (find-cycle-from-node
        graph
        start
        #{start}
        [start]
        cycles))
    ;; Remove duplicates and rotations
    (->> @cycles
         (map set)
         set)))

(defn step1 [data]
  (let [network (parse-network (slurp data))]
    (->> network
         find-cycles-dfs
         (filter #(some (fn [elem] (.startsWith elem "t")) %))
         count)))

(defn step2 [data]
  :not-implemented)
