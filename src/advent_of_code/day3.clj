(ns advent-of-code.day3
  (:require [clojure.string :as str]))

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))

(def sample-raw
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def sample (str/split-lines sample-raw))

;; "467..114.." 0 -> ([467 0 0 ] [114 0 5 ])
(defn parse-numbers
  ([line n]
   ;(map (fn [x] {:s x :row n :col (.indexOf line (str "(\\A\\.)" x "(\\z\\.)"))}) (filter not-empty (str/split line #"\D")))
   (parse-numbers line n () -1 0)
   )
  ([line n acc start current]
   (if (= current (.length line))
     (if (< start 0)
       acc
       (conj acc {:s (.substring line start current) :row n :col start}))
     (if (= start -1)
       (if (Character/isDigit (.charAt line current))
         (parse-numbers line n acc current (+ 1 current))
         (parse-numbers line n acc -1 (+ 1 current))
         )
       (if (Character/isDigit (.charAt line current))
         (parse-numbers line n acc start (+ 1 current))
         (parse-numbers line n (conj acc {:s (.substring line start current) :row n :col start}) -1 (+ 1 current))
         )
       )
     )
   )
  )

(defn is-symbol [plan row column]
  (let [character (.charAt (nth plan row) column)]
    (not (or (Character/isDigit character) (= \. character))))
  )

(defn safe-range [from to max]
  (defn fix [x] (if (< x 0) 0 (if (>= x max) max x)))
  (range (fix from) (fix to))
  )

(defn has-adjacent-symbol [plan number]
  (some identity (for [col (safe-range (- (number :col) 1) (+ (number :col) (.length (number :s)) 1) (.length (first plan)))
                       row (safe-range (- (number :row) 1) (+ (number :row) 2) (.length plan))]

                   (is-symbol plan row col)
                   )
        )
  )

(defn plan-to-numbers [plan]
  (flatten (map-indexed #(parse-numbers %2 %1) plan))
  )

(defn sum-numbers [plan]
  (apply + (map #(Integer/parseInt (% :s)) (filter (fn [number] (has-adjacent-symbol plan number)) (plan-to-numbers plan))))
  )

(def input (read-lines "src/advent_of_code/day3-input"))

(def solution-sample (sum-numbers sample))
(println solution-sample)
(assert (= solution-sample 4361))

(def solution (sum-numbers input))
(println solution)
(assert (= solution 519444))

(defn find-stars-in-line
  ([line n]
   (find-stars-in-line line n 0)
   )
  ([line n index]
   (let [pos (.indexOf line "*" index)]
     (if (= -1 pos)
       ()
       (conj (find-stars-in-line line n (+ 1 pos)) {:row n :col pos})
       )
     )
   )
  )

(defn find-stars [plan]
  (flatten (map-indexed #(find-stars-in-line %2 %1) plan))
  )

(defn star-adjacent [star number]
  (and
    (<= (abs (- (star :row) (number :row))) 1)              ;adjacent row
    (or
      (<= (abs (- (star :col) (number :col))) 1)            ;adjacent start of the number
      (<= (abs (- (star :col) (+ (number :col) (.length (number :s)) -1))) 1) ;adjacent end of the number
      )
    )
  )

(defn find-adjacent-numbers [star numbers]
  (filter #(star-adjacent star %) numbers)
  )

(defn find-number-pairs [numbers stars]
  (filter #(= (count %) 2) (map #(find-adjacent-numbers % numbers) stars))
  )

(defn multiply-sum-pairs [plan]
  (apply + (map #(* (Integer/valueOf ((first %) :s)) (Integer/valueOf ((second %) :s))) (find-number-pairs (plan-to-numbers plan) (find-stars plan))))
  )

(def solution-sample-part-2 (multiply-sum-pairs sample))
(println solution-sample-part-2)
(assert (= 467835 solution-sample-part-2))

(def solution-part-2 (multiply-sum-pairs input))
(println solution-part-2)
(assert (= 74528807 solution-part-2))
