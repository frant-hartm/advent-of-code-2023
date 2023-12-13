(ns advent-of-code.day12
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day12-input"))

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
     :row    (vec (map (comp keyword str) (first split)))
     :groups (vec (map #(Integer/parseInt %) (str/split (second split) #",")))
     }
    )
  )

(defn parse-input [input]
  (vec (map parse-line input))
  )

(defn is-operational [row]
  (every? #(or (= % :.) (= % :?)) row))

(def solve
  (memoize
    (fn [row groups s]
      ;(println row groups s)
      (if (empty? row)
        ; Reached end of row
        (if (or (empty? groups)
                (and (some? s) (= (count groups) 1) (= (first groups) s)))
          (do
            ;(println "GOOD: Reached EOL with matching group")
            1)
          (do
            ;(println "FAIL: Reach EOL with too many groups")
            0))

        (if (and (some? s) (< (first groups) s))
          (do
            ;(println "FAIL: Current size over expected group size")
            0)

          (if (empty? groups)
            ; Consumed all groups, rest of the row must be operational (either . or ?, which all represent .)
            (if (is-operational row)
              (do
                ;(println "GOOD: All groups applied, rest of row operational")
                1)
              (do
                ;(println "FAIL: All groups applied, rest of row NOT operational")
                0))

            (let [head (first row)
                  tail (rest row)]

              (case head
                :? (+
                     (solve (conj tail :#) groups s)
                     (solve (conj tail :.) groups s))

                :. (if (nil? s)
                     ; Another consecutive '.'
                     (solve tail groups nil)

                     ; Dot after group of '#'
                     (if (= (first groups) s)
                       ; if group is of expected size, then recurse
                       (solve tail (rest groups) nil)

                       ; if not this solution is not valid
                       (do
                         ;(println "FAIL: Group does not match size")
                         0)))

                :# (if (nil? s)
                     (solve tail groups 1)
                     (solve tail groups (+ 1 s)))))))))))

(defn sum-records [records]
  (apply + (map #(solve (% :row) (% :groups) nil) records)))

(def solution-sample (sum-records (parse-input sample)))
(println solution-sample)
(assert (= solution-sample 21))

(def solution (sum-records (parse-input input)))
(println solution)
(assert (= solution 7163))

; Part 2
(defn unfold [records]
  (map (fn [record]
         {
          :row    (reduce #(concat %1 [:?] %2) (repeat 5 (record :row)))
          :groups (apply concat (repeat 5 (record :groups)))
          }) records))

(def solution-sample-part-2 (sum-records (unfold (parse-input sample))))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 525152))

(def solution-part-2 (time (sum-records (unfold (parse-input input)))))
(println solution-part-2)
(assert (= solution-part-2 17788038834112))
