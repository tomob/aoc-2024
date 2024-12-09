(ns tomob.day09
  (:require [clojure.string :as string]))

(defn expand
  ([numbers] (expand true numbers))
  ([flatten? numbers]
    (loop [nums numbers
           block-num 0
           data true
           result []]
      (cond
        (empty? nums) (if flatten?
                        (vec (flatten (filter #(not (empty? %)) result)))
                        (vec (filter #(not (empty? %)) result)))
        data (let [block (vec (repeat (first nums) block-num))]
               (recur (rest nums) (inc block-num) false (conj result block)))
        :else (let [space (vec (repeat (first nums) -1))]
                (recur (rest nums) block-num true (conj result space)))))))

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
       (map-indexed (fn [i v] (if (= v -1) 0 (* i v))))
       (apply +)))

(defn step1 [data]
  (->> (slurp data)
       string/trimr
       (mapv #(Character/digit % 10))
       expand
       compact
       checksum))

(defn blocks-info [blocks]
  (loop [remaining-blocks blocks
         current-index 0
         result {}]
    (if (empty? remaining-blocks)
      result
      (let [current-block (first remaining-blocks)
            block-length (count current-block)]
        (recur 
         (rest remaining-blocks)
         (+ current-index block-length)
         (assoc result 
                current-block 
                {:index current-index 
                 :length block-length}))))))

(defn build-free-blocks [blocks]
  (loop [remaining-blocks blocks
         current-index 0
         result {}]
    (if (empty? remaining-blocks)
      result
      (let [current-block (first remaining-blocks)
            block-length (count current-block)]
        (recur
          (rest remaining-blocks)
          (+ current-index block-length)
          (if (= -1 (first current-block))
            (update result 
                   block-length 
                   (fnil #(sort (conj % current-index)) []))
            result))))))

(defn find-free-block [free-blocks len]
  (let [suitable-lengths (filter #(>= % len) (keys free-blocks))
        all-suitable-blocks (for [l suitable-lengths
                                  index (get free-blocks l)]
                              {:length l :index index})
        min-index-block (when (seq all-suitable-blocks)
                          (apply min-key :index all-suitable-blocks))]
    (when min-index-block
      (let [{:keys [length index]} min-index-block
            blocks-list (get free-blocks length)
            new-blocks (vec (remove #{index} blocks-list))
            new-map (if (seq new-blocks)
                     (assoc free-blocks length new-blocks)
                     (dissoc free-blocks length))
            final-map (if (> length len)
                       (update new-map
                              (- length len)
                              (fn [existing]
                                (vec (sort (conj (or existing []) 
                                               (+ index len))))))
                       new-map)]
        [index final-map]))))

(defn compact-by-block [numbers]
  (let [anumbers (long-array (flatten numbers))
        info (blocks-info numbers)]
    (loop [free-blocks (build-free-blocks numbers)
           block-number (dec (count numbers))]
      (let [block (numbers block-number)]
        (cond
          (zero? block-number) (vec anumbers)
          (= -1 (first block)) (recur free-blocks (dec block-number))
          :else
          (let [place (find-free-block free-blocks (count block))
                block-info (info block)]
            (if (and (not (nil? place))
                     (< (first place) (:index block-info))) ;; Don't move farther right than its original position
              (do
                (dotimes [x (count block)]
                  (aset anumbers (+ (first place) x) (first block)))
                (let [start-index (:index block-info)]
                  (dotimes [x (:length block-info)]
                    (aset anumbers (+ x start-index) -1)))
                (recur (second place) (dec block-number)))
              (recur free-blocks (dec block-number)))))))))

(defn step2 [data]
  (->> (slurp data)
       string/trimr
       (mapv #(Character/digit % 10))
       (expand false)
       compact-by-block
       checksum))
