(ns advent-of-code.day1
  (:require [clojure.string :as str]))
(require ['clojure.string :as 'str])

(def sample "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defn read-file [input] (slurp input))

(defn decodeLine [line]
  (def digits (filter #(Character/isDigit %) (seq line)))
  (Integer/parseInt (apply str [(first digits) (last digits)]))
  )

(defn readAndDecode [input] (reduce + (map decodeLine (str/split-lines input))))

(def sample-solution (readAndDecode sample))
(println sample-solution)
(assert (= sample-solution 142))

(def part1-solution (readAndDecode (read-file "src/advent_of_code/day1-input")))
(println part1-solution)
(assert (= part1-solution) 55002)

; Part 2

(defn read-lines [input] (str/split-lines (read-file input)))

(def sample2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn decode-line [digits]
  (Integer/parseInt (apply str [(first digits) (last digits)]))
  )

(def pairs '{"1"     1,
             "2"     2,
             "3"     3,
             "4"     4,
             "5"     5,
             "6"     6,
             "7"     7,
             "8"     8,
             "9"     9,
             "one"   1,
             "two"   2,
             "three" 3,
             "four"  4,
             "five"  5,
             "six"   6,
             "seven" 7,
             "eight" 8,
             "nine"  9}
  )

(defn find-all-occurrences
  ([line pattern]
   (find-all-occurrences line pattern 0))

  ([line pattern fromIndex]
   (let [index (str/index-of line pattern fromIndex)]
     (if (some? index)
       (conj (find-all-occurrences line pattern (+ 1 index)) index)
       '()
       )
     )
   )
  )

(defn read-digits [line]
  (map
    first
    (sort-by
      second
      (apply concat
             (map
               (fn [key] (map
                           (fn [item] [(get pairs key) item])
                           (find-all-occurrences line key)
                           )
                 )
               (keys pairs)))))
  )

(defn read-decode [input] (reduce + (map (comp decode-line read-digits) input)))

(def sample2-solution (read-decode (str/split-lines sample2)))
(println sample-solution)
(assert (= sample2-solution 281))

(def part2-solution (read-decode (read-lines "src/advent_of_code/day1-input")))
(println part2-solution)
(assert (= part2-solution 55093))