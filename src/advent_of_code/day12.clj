(ns advent-of-code.day12
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day10-input"))

(def sample-raw "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1 ")

(def sample (str/split-lines sample-raw))

(defn parse-line [line]
  (let [split (str/split line #" ")]
    {
     :row (vec (map (comp keyword str) (first split)))
     :groups (vec (map #(Integer/parseInt %) (str/split (second split) #",")))
     }
    )
  )

(defn parse-input [input]
  (vec (map parse-line input))
  )

(defn solve [row groups n s]
  (case (nth row n)
    :? (+
         (solve (assoc row n :#) groups (+ 1 n))
         (solve (assoc row n :.) groups (+ 1 n)))

    :. ()                                                   ; finishes group, s must match group size

    :#                                                      ;; either starts or continues a group

    )


    )


  )