(ns advent-of-code.day6
  (:require [clojure.string :as str]))

(def input-raw
  "Time:        62     64     91     90
Distance:   553   1010   1473   1074")

(def sample-raw
  "Time:      7  15   30
Distance:  9  40  200")

(def input (str/split-lines input-raw))
(def sample (str/split-lines sample-raw))

(defn read-as-list [string]
  (read-string (str "(" string ")")))


(defn races [input]
  (let [parts (map (comp read-as-list #(second (str/split % #":"))) input)]
    (map vector (first parts) (second parts)))
  )

(defn race-wins [time distance]
  (count (filter (partial < distance) (map #(* % (- time %)) (range 1 time))))
  )

(defn number-of-wins [races]
  (apply * (map #(race-wins (first %) (second %)) races))
  )

(def solution-sample (number-of-wins (races sample)))
(println solution-sample)
(assert (= solution-sample 288))

(def solution (number-of-wins (races input)))
(println solution)
(assert (= solution 840336))

(defn races2 [input]
  (list (vec (map (comp #(Long/valueOf %) #(.replace % " " "") #(second (str/split % #":"))) input)))
  )

(def solution-sample-part-2 (number-of-wins (races2 sample)))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 71503))

(def solution-part-2 (number-of-wins (races2 input)))
(println solution-part-2)
(assert (= solution-part-2 41382569))
