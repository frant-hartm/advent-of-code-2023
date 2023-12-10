(ns advent-of-code.day5
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day5-input"))

(def sample-raw "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(def sample (str/split-lines sample-raw))

(defn read-as-list [string]
  (read-string (str "(" string ")")))

(defn parse-seeds [input]
  (read-as-list (second (str/split (first input) #":"))))

(defn read-mappings
  ([input]
   (read-mappings input [] []))

  ([input acc current]
   (let [value (first input)]
     (if (empty? input)
       (conj acc current)
       (if (.isEmpty value)
         (if (empty? current)
           (read-mappings (rest input) acc current)
           (read-mappings (rest input) (conj acc current) [])
           )
         (if (.endsWith value ":")
           (read-mappings (rest input) acc current)
           (read-mappings (rest input) acc (conj current value))
           )
         ))
     )
   )
  )

(defn to-range [in]
  {:from (second in) :to (+ (second in) (nth in 2)) :adjustment (- (first in) (second in))}
  )

(defn parse-mappings [input]
  (map (fn [coll] (map (comp to-range read-as-list) coll)) (read-mappings (rest input))))

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn find-range [mapping n]
  (find-first #(and (<= (% :from) n) (< n (% :to))) mapping))

(defn range-to-fn [range]
  (fn [x]
    (if (nil? range)
      x
      (+ x (range :adjustment))))
  )

(defn map-source-target [mapping source]
  (map (fn [x] (((comp range-to-fn (partial find-range mapping)) x) x)) source)
  )

(defn functions [input]
  (map #(partial map-source-target %) (parse-mappings input)))

(defn find-lowest [mappings seeds]
  (apply min ((apply comp (reverse (map #(partial map-source-target %) mappings))) seeds)))

(def solution-sample (find-lowest (parse-mappings sample) (parse-seeds sample)))
(println solution-sample)
(assert (= solution-sample 35))

(def solution (find-lowest (parse-mappings input) (parse-seeds input)))
(println solution)
(assert (= solution 323142486))

(defn list-to-pairs [coll]
  (if (empty? coll)
    coll
    (conj (list-to-pairs (rest (rest coll))) (list (first coll) (second coll)))
    )
  )

(defn expand-seeds [seeds]
  (map #(range (first %) (+ (first %) (second %))) (list-to-pairs seeds)))

(def solution-sample-part-2 (apply min (pmap #(find-lowest (parse-mappings sample) %) (expand-seeds (parse-seeds sample)))))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 46))

; Takes several minutes
;(def solution-part-2 (apply min (pmap #(find-lowest (parse-mappings input) %) (expand-seeds (parse-seeds input))) ))
;(println solution-part-2)
;(assert (= solution-part-2 79874951))
