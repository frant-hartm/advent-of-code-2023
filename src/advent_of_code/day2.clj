(ns advent-of-code.day2
  (:require [clojure.string :as str]))

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))

(def sample-raw
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def sample (str/split-lines sample-raw))

(def max-red 12)
(def max-green 13)
(def max-blue 14)

; parse '3 blue' into map {:blue 3}
(defn parse-color [color-part]
  (let [value (str/split color-part #" ")] {(keyword (second value)) (Integer/valueOf (first value))})
  )

; parse single draw:
; 3 blue, 4 red -> {:blue 3, :red 4}
(defn parse-draw [draw-part]
  (let [color-parts (map str/trim (str/split draw-part #","))]
    (reduce conj (map parse-color color-parts))
    )
  )

; parse all draws for one game:
; 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green -> ({:blue 3, :red 4}, {:red 1, :green 2, :blue 6} {:green 2})
(defn parse-sets [sets-part] (map parse-draw (str/split sets-part #";")))

;; parse game id
;; Game 11 -> 11
(defn parse-game-id [game-part] (Integer/valueOf (second (str/split game-part #" "))))

(defn parse-game [line]
  (let [split (str/split line #":")]
    {
     :game (parse-game-id (first split)),
     :sets (parse-sets (second split))
     }
    )
  )

(defn is-valid-set [set]
  (and (<= (set :red 0) max-red) (<= (set :blue 0) max-blue) (<= (set :green 0) max-green)))

(defn filter-game [game]
  (every? is-valid-set (game :sets))
  )

(defn count-games [lines]
  (reduce + (map #(% :game) (filter filter-game (map parse-game lines)))))

(def sample-solution (count-games sample))
(println sample-solution)
(assert (= sample-solution 8))

(def part1-solution (count-games (read-lines "src/advent_of_code/day2-input")))
(println part1-solution)
(assert (= part1-solution 2061))

(defn power [game]
  (apply * (map (fn [key] (apply max (map #(get % key 0) (game :sets)))) '(:red :blue :green)))
  )

(defn sum-power [games]
  (apply + (map power games)))

(def part2-sample-solution (sum-power (map parse-game sample)))
(println part2-sample-solution)
(assert (= part2-sample-solution 2286))

(def part2-solution (sum-power (map parse-game (read-lines "src/advent_of_code/day2-input"))))
(println part2-solution)
(assert (= part2-solution 72596))
