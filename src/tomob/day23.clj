(ns tomob.day23
  (:require [clojure.string :as string]
            [clojure.set :as st]))

(defn parse-network [data]
  (apply
    merge-with 
    st/union
    (for [line (string/split-lines data)
          :let [[x y] (string/split line #"-")]]
      {x #{y} y #{x}})))

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

(defn find-cliques
  "Find cliques using Bron-Kerbosh algorithm"
  ([graph] 
   (let [nodes (set (keys graph))]
     (find-cliques graph #{} nodes #{})))
  ([graph r p x]
   ;; r - current clique
   ;; p - candidates to add to clique
   ;; x - excluded vertices
   (if (and (empty? p) (empty? x))
     #{r}
     (let [pivot (first (or (seq p) (seq x)))
           neighbors (get graph pivot #{})
           candidates (remove #(contains? neighbors %) p)]
       (->> candidates
            (mapcat (fn [v]
                     (let [v-neighbors (get graph v #{})]
                       (find-cliques
                         graph
                         (conj r v)
                         (set (filter #(contains? v-neighbors %) p))
                         (set (filter #(contains? v-neighbors %) x))))))
            set)))))

(defn find-maximal-cliques [graph]
  (->> (find-cliques graph)
       (remove empty?)
       (sort-by count >)))

(defn sort-and-join [s]
  (->> s sort (string/join ",")))

(defn step2 [data]
  (let [network (parse-network (slurp data))]
    (->> network
         find-maximal-cliques
         first
         sort-and-join)))
