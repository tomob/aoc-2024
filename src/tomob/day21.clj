(ns tomob.day21
  (:require [clojure.string :as string]))

;; Used dijkstra to find best paths, manually selected ones that prefer < and v first
(def keypad-paths
  {[\1 \0] ">vA"    [\9 \7] "<<A"  [\9 \8] "<A"     [\7 \2] "vv>A"  [\2 \0] "vA"    [\7 \0] ">vvvA" [\1 \7] "^^A"
   [\2 \5] "^A"     [\3 \A] "vA"   [\7 \1] "vvA"    [\7 \7] "A"     [\3 \6] "^A"    [\1 \3] ">>A"   [\7 \5] "v>A"
   [\4 \1] "vA"     [\2 \9] "^^>A" [\2 \3] ">A"     [\A \8] "<^^^A" [\8 \6] "v>A"   [\7 \4] "vA"    [\A \4] "^^<<A"
   [\4 \9] ">>^A"   [\6 \0] "<vvA" [\2 \7] "<^^A"   [\0 \0] "A"     [\5 \7] "<^A"   [\5 \9] ">^A"   [\0 \9] "^^^>A"
   [\9 \1] "<<vvA"  [\0 \5] "^^A"  [\7 \8] ">A"     [\6 \2] "<vA"   [\0 \3] "^>A"   [\3 \0] "<vA"   [\9 \6] "vA"
   [\3 \2] "<A"     [\4 \6] ">>A"  [\0 \6] "^^>A"   [\A \6] "^^A"   [\8 \A] "vvv>A" [\5 \2] "vA"    [\A \3] "^A"
   [\6 \6] "A"      [\4 \5] ">A"   [\A \0] "<A"     [\3 \4] "<<^A"  [\1 \1] "A"     [\8 \1] "<vvA"  [\3 \9] "^^A"
   [\5 \3] "v>A"    [\6 \8] "^<A"  [\8 \4] "<vA"    [\0 \8] "^^^A"  [\5 \6] ">A"    [\2 \6] "^>A"   [\1 \6] ">>^A"
   [\3 \8] "<^^A"   [\3 \1] "<<A"  [\1 \2] ">A"     [\A \1] "^<<A"  [\5 \0] "vvA"   [\8 \0] "vvvA"  [\5 \4] "<A"
   [\8 \3] "vv>A"   [\8 \8] "A"    [\6 \3] "vA"     [\7 \9] ">>A"   [\6 \7] "<<^A"  [\1 \9] "^^>>A" [\9 \2] "<vvA"
   [\6 \9] "^A"     [\8 \2] "vvA"  [\9 \A] "vvvA"   [\6 \5] "<A"    [\0 \1] "^<A"   [\2 \A] "v>A"   [\3 \7] "<<^^A"
   [\5 \5] "A"      [\6 \1] "<<vA" [\1 \8] "^^>A"   [\9 \5] "<vA"   [\7 \6] ">>vA"  [\4 \7] "^A"    [\5 \8] "^A"
   [\8 \5] "vA"     [\4 \2] ">vA"  [\5 \A] "vv>A"   [\8 \9] ">A"    [\2 \4] "<^A"   [\0 \7] "^^^<A" [\5 \1] "<vA"
   [\9 \9] "A"      [\0 \2] "^A"   [\2 \8] "^^A"    [\8 \7] "<A"    [\3 \3] "A"     [\6 \A] "vvA"   [\4 \4] "A"
   [\6 \4] "<<A"    [\1 \4] "^A"   [\7 \A] ">>vvvA" [\9 \3] "vvA"   [\9 \0] "<vvvA" [\7 \3] "vv>>A" [\A \A] "A"
   [\2 \2] "A"      [\A \9] "^^^A" [\4 \A] "vv>>A"  [\1 \A] ">>vA"  [\A \2] "<^A"   [\1 \5] ">^A"   [\4 \0] ">vvA"
   [\A \7] "^^^<<A" [\A \5] "<^^A" [\9 \4] "<<vA"   [\4 \8] ">^A"   [\3 \5] "<^A"   [\2 \1] "<A"    [\0 \4] "^^<A"
   [\0 \A] ">A"     [\4 \3] "v>>A"})

(def dirpad-paths
  {[\> \A] "^A"   [\^ \v] "vA"  [\A \>] "vA"  [\> \v] "<A"
   [\< \A] ">>^A" [\v \v] "A"   [\> \^] "<^A" [\A \^] "<A"
   [\v \>] ">A"   [\^ \<] "<vA" [\< \v] ">A"  [\^ \^] "A"
   [\> \>] "A"    [\^ \>] "v>A" [\< \^] ">^A" [\A \<] "v<<A"
   [\v \A] "^>A"  [\> \<] "<<A" [\< \<] "A"   [\^ \A] ">A"
   [\A \A] "A"    [\A \v] "<vA" [\v \^] "^A"  [\< \>] ">>A"
   [\v \<] "<A"})

(defn get-pairs [code]
  (map (fn [a b] [a b]) (str "A" code) code))

(defn drive-robot [paths code]
  (apply str
         (for [pair (get-pairs code)]
           (paths pair))))

(defn drive-all-robots [code]
  (->> code
       (drive-robot keypad-paths)
       (drive-robot dirpad-paths)
       (drive-robot dirpad-paths)))

(defn code->number [code]
  (->> code
       drop-last
       (apply str)
       (Integer. )))

(defn complexity [[code instructions]]
  (* code (count instructions)))

(defn step1 [data]
  (let [codes (->> data slurp string/split-lines)]
    (->> codes
         (map (fn [code] [(code->number code) (drive-all-robots code)]))
         (map complexity)
         (reduce +))))

(defn step2 [data]
  :not-implemented)
