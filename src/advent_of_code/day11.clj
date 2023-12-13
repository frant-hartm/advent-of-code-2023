(ns advent-of-code.day11
  (:require [clojure.string :as str]))

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day11-input"))

(def sample-raw "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(def sample (str/split-lines sample-raw))

(defn parse-universe [input]
  (vec (map #(vec (map (comp keyword str) (seq %))) input)))

(defn find-empty-columns [universe]
  (set
    (let [width (range 0 (count (first universe)))]
      (filter (fn [column] (every? #(= :. (nth % column)) universe)) width)
      )))

(defn expand-columns-in-row [columns row]
  (vec (flatten
         (map-indexed
           (fn [i x] (if (contains? columns i)
                       [x x]
                       x))
           row))))

(defn expand-columns [universe]
  (map #(expand-columns-in-row (find-empty-columns universe) %) universe))

(defn expand-rows [universe]
  (if (empty? universe)
    ()
    (let [row (first universe)]
      (if (every? (partial = :.) row)
        (conj (conj (expand-rows (rest universe)) row) row)
        (conj (expand-rows (rest universe)) row)))))

(defn expand-universe [universe]
  (vec (expand-rows (expand-columns universe))))

(defn is-galaxy [x y universe]
  (= :# (nth (nth universe y) x)))

(defn find-galaxies [universe]
  (for [y (range 0 (count universe))
        x (range 0 (count (first universe)))
        :when (is-galaxy x y universe)]
    (vector x y)))

(defn distance [g1 g2]
  (apply + (mapv (comp abs -) g1 g2)))

(defn sum-all-paths [galaxies]
  (if (empty? galaxies)
    0
    (+
      (apply + (map #(distance (first galaxies) %) (rest galaxies)))
      (sum-all-paths (rest galaxies)))))

(def solution-sample (sum-all-paths (find-galaxies (expand-universe (parse-universe sample)))))
(println solution-sample)
(assert (= solution-sample 374))

(def solution (sum-all-paths (find-galaxies (expand-universe (parse-universe input)))))
(println solution)
(assert (= solution 9274989))

(defn find-empty-rows [universe]
  (set
    (filter some? (map-indexed
      (fn [i row]
        (if (every? (partial = :.) row)
          i
          nil)) universe))))

(defn count-in-interval [coll interval]
  ;(println coll interval)
  (count (filter #(< (first interval) % (second interval)) coll)))

(defn distanceX [g1 g2 empty-rows empty-columns multiplier]
  (let [x1 (first g1)
        y1 (second g1)
        x2 (first g2)
        y2 (second g2)
        int-x [(min x1 x2) (max x1 x2)]
        int-y [(min y1 y2) (max y1 y2)]
        ]
    (+
      (apply + (mapv (comp abs -) g1 g2))
      (let [columns (* (- multiplier 1) (count-in-interval empty-columns int-x))
            rows (* (- multiplier 1) (count-in-interval empty-rows int-y))]
        ;(println columns rows)
        (+ columns rows)))))

(defn sum-all-paths-X [galaxies empty-rows empty-columns multiplier]
  (if (empty? galaxies)
    0
    (+
      (apply + (map #(distanceX (first galaxies) % empty-rows empty-columns multiplier) (rest galaxies)))
      (sum-all-paths-X (rest galaxies) empty-rows empty-columns multiplier))))

(def solution-sample-part2
  (let [universe (parse-universe sample)]
                             (sum-all-paths-X (find-galaxies universe) (find-empty-rows universe) (find-empty-columns universe) 10)))

(println solution-sample-part2)
(assert (= solution-sample-part2 1030))

(def solution-part2
  (let [universe (parse-universe input)]
                             (sum-all-paths-X (find-galaxies universe) (find-empty-rows universe) (find-empty-columns universe) 1000000)))

(println solution-part2)
(assert (= solution-part2 357134560737))
