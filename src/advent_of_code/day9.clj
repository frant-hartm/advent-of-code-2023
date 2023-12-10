(ns advent-of-code.day9
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day9-input"))

(def sample-raw "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(def sample (str/split-lines sample-raw))

(defn read-as-list [string]
  (read-string (str "(" string ")")))

(defn parse-input [input]
  (map read-as-list input))

(defn compute-differences
  ([x y]
   (list (- y x)))

  ([x y & more]
   (conj (apply compute-differences y more) (- y x)))
  )

(defn recurse-compute-differences [input]
  (if (every? (partial = 0) input)
    (list input)
    (let [differences (apply compute-differences input)]
      (conj (recurse-compute-differences differences) input))
    )
  )

(defn sum-last [differences]
  (apply + (map last differences))
  )

(defn sum-all-predictions [all-differences]
  (apply + (map sum-last all-differences)))

(def solution-sample (sum-all-predictions (map recurse-compute-differences (parse-input sample))))
(println solution-sample)
(assert (= solution-sample 114))

(def solution (sum-all-predictions (map recurse-compute-differences (parse-input input))))
(println solution)
(assert (= solution 1819125966))

(defn subtract-last
  ([x]
   x)

  ([x & more]
   (- x (apply subtract-last more)))
  )

(defn sum-all-previous-predictions [all-differences]
  (apply + (map #(apply subtract-last %) (map (partial map first) all-differences)))
  )

(def solution-sample-part-2 (sum-all-previous-predictions (map recurse-compute-differences (parse-input sample))))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 2))

(def solution-part-2 (sum-all-previous-predictions (map recurse-compute-differences (parse-input input))))
(println solution-part-2)
(assert (= solution-part-2 1140))
