(ns advent-of-code.day11_tidy
  (:require [clojure.string :as str]))

(defn read-file [input] (slurp input))
(defn read-lines [raw-input] (str/split-lines raw-input))

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

(def puzzle (read-lines (read-file "src/advent_of_code/day11-input")))
(def sample (read-lines sample-raw))

(defn parse-universe [input]
  (vec (map #(vec (map (comp keyword str) (seq %))) input)))

(defn find-empty-columns [universe]
  (set
    (let [width (range 0 (count (first universe)))]
      (filter (fn [column] (every? #(= :. (nth % column)) universe)) width)
      )))

(defn is-galaxy [x y universe]
  (= :# (nth (nth universe y) x)))

(defn find-galaxies [universe]
  (for [y (range 0 (count universe))
        x (range 0 (count (first universe)))
        :when (is-galaxy x y universe)]
    (vector x y)))

(defn count-in-interval [coll interval]
  ;(println coll interval)
  (count (filter #(< (first interval) % (second interval)) coll)))

(defn distance [g1 g2 empty-rows empty-columns multiplier]
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

(defn sum-all-paths [galaxies empty-rows empty-columns multiplier]
  (if (empty? galaxies)
    0
    (+
      (apply + (map #(distance (first galaxies) % empty-rows empty-columns multiplier) (rest galaxies)))
      (sum-all-paths (rest galaxies) empty-rows empty-columns multiplier))))

(defn find-empty-rows [universe]
  (set
    (filter some? (map-indexed
                    (fn [i row]
                      (if (every? (partial = :.) row)
                        i
                        nil)) universe))))

(defn solve-puzzle [input multiplier]
  (let [universe (parse-universe input)]
    (sum-all-paths (find-galaxies universe) (find-empty-rows universe) (find-empty-columns universe) multiplier)))

(def solution-sample (solve-puzzle sample 2))
(println solution-sample)
(assert (= solution-sample 374))

(def solution (solve-puzzle puzzle 2))
(println solution)
(assert (= solution 9274989))

(def solution-sample-part2 (solve-puzzle sample 10))
(println solution-sample-part2)
(assert (= solution-sample-part2 1030))

(def solution-part2 (solve-puzzle puzzle 1000000))
(println solution-part2)
(assert (= solution-part2 357134560737))
