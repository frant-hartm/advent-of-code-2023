(ns advent-of-code.day4
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.math :as math])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day4-input"))

(def sample-raw
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def sample (str/split-lines sample-raw))

(defn parse-card [card]
  (let [split (str/split card #"[:\|]")]
    (list
      (set (read-string (str "(" (second split) ")")))
      (set (read-string (str "(" (nth split 2) ")")))
      )
    )
  )

(defn matching-numbers [card]
  (count (apply set/intersection card)))

(defn count-winning-numbers [card]
  (math/floor (math/pow 2 (- (matching-numbers card) 1)))
  )

(defn sum-winning-numbers [cards]
  (int (apply + (map (comp count-winning-numbers parse-card) cards)))
  )

(def solution-sample (sum-winning-numbers sample))
(println solution-sample)
(assert (= solution-sample 13))

(def solution (sum-winning-numbers input))
(println solution)
(assert (= solution 20117))

(defn increment [list from to by]
  (map-indexed (fn [i n] (if (and (<= from i) (< i to)) (+ n by) n)) list))

(defn count-cards
  ([deck]
   (count-cards deck (repeat (count deck) 1) 0))

  ([deck counts n]
   (if (empty? deck)
     (apply + counts)
     (count-cards (rest deck) (increment counts (+ n 1) (+ 1 n ((comp matching-numbers parse-card) (first deck))) (nth counts n)) (+ n 1))
     )
   )
  )

(def solution-sample-part-2 (count-cards sample))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 30))

(def solution-part-2 (count-cards input))
(println solution-part-2)
(assert (= solution-part-2 13768818))
