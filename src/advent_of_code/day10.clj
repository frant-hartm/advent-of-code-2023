(ns advent-of-code.day10
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day10-input"))

(def sample-raw "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def sample-raw2 "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ ")

(def sample (str/split-lines sample-raw))
(def sample2 (str/split-lines sample-raw2))

(defn parse-line [line]
  (vec (map (comp keyword str) (seq line))))

(defn parse-input [input]
  (vec (map parse-line input))
  )

(defn pipe-at [position plan]
  (nth (nth plan (second position)) (first position))
  )

(defn find-start [plan]
  (first (filter #(not= -1 (first %)) (map-indexed #(vector (.indexOf %2 :S) %1) plan)))
  )

(def next-steps-table
  {
   :| '([0 -1] [0 1])
   :- '([-1 0] [1 0])
   :L '([0 -1] [1 0])
   :J '([-1 0] [0 -1])
   :7 '([-1 0] [0 1])
   :F '([1 0] [0 1])
   :. '()
   :S '()
   }
  )

(defn next-steps [position symbol]
  (map (partial mapv + position) (next-steps-table symbol))
  )

(defn filter-out-of-bounds [positions]
  (filter #(and (<= 0 (first %)) (<= 0 (second %))) positions)
  )

(defn adjacent [position]
  (filter-out-of-bounds
    (for [x (range -1 2)
          y (range -1 2)
          :when (not= [x y] [0 0])]
      (mapv + position [x y])
      )
    )
  )

(defn first-steps [start plan]
  (filter #(some (partial = start) (next-steps (first %) (second %))) (map #(vector % (pipe-at % plan)) (adjacent start)))
  )

(defn next-step [position previous plan]
  (first (filter (partial not= previous) (next-steps position (pipe-at position plan)))))

(defn walk [start position previous plan distance acc]
  ;(println position)
  (if (= start position)
    acc
    (recur start (next-step position previous plan) position plan (+ 1 distance) (conj acc [position distance]))
    )
  )

(defn compute-max-distance [plan]
  (let [start (find-start plan)
        steps (first-steps start plan)]
    (second (apply max-key second (map (fn [x] (apply min-key second x)) (vals (group-by first (apply concat (map #(walk start (first %) start plan 1 ()) steps)))))))
    )
  )

(def solution-sample (compute-max-distance (parse-input sample)))
(println solution-sample)
(assert (= solution-sample 4))

(def solution-sample-2 (compute-max-distance (parse-input sample2)))
(println solution-sample-2)
(assert (= solution-sample-2 8))

(def solution (compute-max-distance (parse-input input)))
(println solution)
(assert (= solution 6717))

(defn compute-path [plan]
  (let [start (find-start plan)
        first-step (first (first-steps start plan))]
    (conj (map first (walk start (first first-step) start plan 1 ())) start)
    )
  )

(defn contains [element coll]
  (boolean (some #(= element %) coll)))


(def next-is-inside-by-key-fn
  {
   :| (fn [state] (assoc state :is-inside (not (state :is-inside))))
   :- identity
   :L (fn [state] (assoc state :prev :L))
   :J (fn [state]
        (if (= (state :prev) :L)
          state
          (assoc state :is-inside (not (state :is-inside)))
          )
        )
   :7 (fn [state]
        (if (= (state :prev) :F)
          state
          (assoc state :is-inside (not (state :is-inside)))
          )
        )
   :F (fn [state] (assoc state :prev :F))
   }
  )

(defn next-is-inside [x line state]
  ((next-is-inside-by-key-fn (nth line x)) state)
  )

(defn count-in-line [x y state line path acc]
  (if (= x (count line))
    acc

    (if (contains [x y] path)
      (count-in-line (+ x 1) y (next-is-inside x line state) line path acc)

      (count-in-line (+ x 1) y state line path (+ acc (if (state :is-inside) 1 0)))
      )
    )
  )

(defn new-s-symbol [x y plan]
  (let [up (contains? (set '(:7 :| :F)) (pipe-at [x (dec y)] plan))
        down (contains? (set '(:J :| :L)) (pipe-at [x (inc y)] plan))
        left (contains? (set '(:L :- :F)) (pipe-at [(dec x) y] plan))
        right (contains? (set '(:J :- :7)) (pipe-at [(inc x) y] plan))]
    (second (first (filter first (list
                                   [(and up down) :|]
                                   [(and left right) :-]
                                   [(and left up) :J]
                                   [(and left down) :7]
                                   [(and up right) :L]
                                   [(and down right) :F]
                                   ))))
    )
  )

(defn replace-s-symbol [start plan]
  (let [new-s (new-s-symbol (first start) (second start) plan)]
    (map
      (fn [line]
        (replace {:S new-s} line)
        )
      plan
      )
    )
  )

(defn count-total [plan]
  (let [path (compute-path plan)]
    (apply + (map-indexed (fn [y line] (count-in-line 0 y {:is-inside false} line path 0)) (replace-s-symbol (find-start plan) plan)))
    )
  )

(def solution-sample-part-2 (count-total (parse-input sample)))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 1))

(def solution-part-2 (count-total (parse-input input)))
(println solution-part-2)
(assert (= solution-part-2 381))
