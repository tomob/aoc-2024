(ns tomob.day16
  (:require [clojure.string :as string]))

(defn find-pos [maze ch]
  (first 
    (for [y (range (alength maze))
          x (range (alength (aget maze 0)))
          :when (= ch (aget maze y x))]
      [x y])))

(def next-moves
  {:north [[[0 -1] :north]
           [[1 0] :east]
           [[-1 0] :west]]
   :south [[[0 1] :south]
           [[1 0] :east]
           [[-1 0] :west]]
   :east  [[[1 0] :east]
           [[0 -1] :north]
           [[0 1] :south]]
   :west  [[[-1 0] :west]
           [[0 -1] :north]
           [[0 1] :south]]})

(defn get-next-positions [maze [x y] dir]
  (for [[[dx dy] next-dir] (next-moves dir)
        :let [nx (+ x dx)
              ny (+ y dy)]
        :when (not= \# (aget maze ny nx))]
    [[nx ny] next-dir]))

(defn step-cost [curr-dir next-dir]
  (if (= curr-dir next-dir) 1 1001))

(defn dijkstra [maze start start-dir end]
  (loop [q (sorted-set [0 start start-dir])
         dist {[start start-dir] 0}]
    (if (empty? q) dist
      (let [[d current-pos curr-dir] (first q)
            [nq ndist] (reduce
                         (fn [[q d] [next-pos next-dir alt]]
                             [(conj q [alt next-pos next-dir]) (assoc d [next-pos next-dir] alt)])
                         [(disj q (first q)) dist]
                         (for [[next-pos next-dir cost] (get-next-positions maze current-pos curr-dir)
                               :let [alt (+ (get dist [current-pos curr-dir] Integer/MAX_VALUE) (step-cost curr-dir next-dir))]
                               :when (< alt (get dist [next-pos next-dir] Integer/MAX_VALUE))]
                           [next-pos next-dir alt]
                         ))]
        (recur nq ndist)))))

(defn dijkstra-all [maze start start-dir end]
  (loop [q (sorted-set [0 start start-dir])
         dist {[start start-dir] 0}
         paths {[start start-dir] #{[[start start-dir]]}}]
    (if (empty? q) 
      {:dist dist :paths paths}
      (let [[d current-pos curr-dir] (first q)
            current-key [current-pos curr-dir]
            current-paths (get paths current-key #{})
            [nq ndist npaths] (reduce
                               (fn [[q d p] [next-pos next-dir alt]]
                                 (let [next-key [next-pos next-dir]
                                       current-alt (get d next-key Integer/MAX_VALUE)]
                                   (cond
                                     (< alt current-alt)
                                     [(conj q [alt next-pos next-dir])
                                      (assoc d next-key alt)
                                      (assoc p next-key 
                                        (set (map #(conj % next-key) current-paths)))]

                                     (= alt current-alt)
                                     [q d
                                      (update p next-key into
                                        (map #(conj % next-key) current-paths))]

                                     :else
                                     [q d p])))
                               [(disj q (first q)) dist paths]
                               (for [[next-pos next-dir cost] (get-next-positions maze current-pos curr-dir)
                                     :let [alt (+ (get dist current-key Integer/MAX_VALUE)
                                                (step-cost curr-dir next-dir))]]
                                 [next-pos next-dir alt]))]
        (recur nq ndist npaths)))))

(defn print-maze [maze path]
  (doseq [[[x y] dir] path]
    (case dir
      :north (aset maze y x \^)
      :south (aset maze y x \v)
      :east (aset maze y x \>)
      :west (aset maze y x \<)))
  (dotimes [y (alength maze)]
    (dotimes [x (alength (aget maze y))]
        (print (aget maze y x)))
    (println)))

(defn find-best [dist end]
  (->> dist
         (filter (fn [[[pos dir] _]] (= pos end)))
         (map (fn [[k v]] v))
         sort
         first))

(defn step1 [data]
  (let [maze (-> data slurp string/split-lines to-array-2d)
        start (find-pos maze \S)
        end (find-pos maze \E)
        dist (dijkstra maze start :east end)]
    (find-best dist end)))

(defn find-all-best [{:keys [dist paths]} end]
  (let [best-dist (->> dist
                      (filter (fn [[[pos _] _]] (= pos end)))
                      (map (fn [[k v]] v))
                      sort
                      first)
        best-paths (->> paths
                       (filter (fn [[[pos _] _]] (= pos end)))
                       (filter (fn [[k _]] (= (dist k) best-dist)))
                       (mapcat (fn [[_ paths]] paths)))]
    {:distance best-dist
     :paths best-paths}))

(defn get-points [path]
  (map (fn [[pos dir]] pos) path))

(defn step2 [data]
  (let [maze (-> data slurp string/split-lines to-array-2d)
        start (find-pos maze \S)
        end (find-pos maze \E)
        dist (dijkstra-all maze start :east end)
        {distance :distance paths :paths} (find-all-best dist end)]
    (->> paths
         (map get-points)
         (mapcat identity)
         (into #{})
         count)))
