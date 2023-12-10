(ns advent-of-code.day8
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day8-input"))

(def sample-raw "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def sample-raw2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(def sample (str/split-lines sample-raw))
(def sample2 (str/split-lines sample-raw2))

(defn read-as-list [string]
  (read-string (str "(" string ")")))

(defn parse-path [line]
  (vec (map (comp keyword str) (seq line))))

(defn part-to-keyword [part]
  ;(println (str (keyword part)))
  (keyword part)
  )
(defn parse-desert-map [desert-map]
  (into {} (map (fn [x] (let [parts (str/split x #"=")
                              rl (map keyword (read-string (second parts)))]
                          [
                           (part-to-keyword (str/trim (first parts)))
                           {:L (first rl) :R (second rl)}
                           ]
                          ))
                desert-map))
  )

(defn parse-input [game]
  {
   :path (parse-path (first game))
   :map  (parse-desert-map (drop 2 game))
   })

(defn run [symbol position game]
  (if (= symbol :ZZZ)
    position
    (recur (((game :map) symbol) (nth (game :path) (mod position (count (game :path))))) (+ 1 position) game)
    ))

(def solution-sample (run :AAA 0 (parse-input sample)))
(println solution-sample)
(assert (= solution-sample 2))

(def solution-sample-2 (run :AAA 0 (parse-input sample2)))
(println solution-sample-2)
(assert (= solution-sample-2 6))

(def solution (run :AAA 0 (parse-input input)))
(println solution)
(assert (= solution 17621))

(def sample-raw3 "LR

llA = (llB, XXX)
llB = (XXX, llZ)
llZ = (llB, XXX)
SSA = (SSB, XXX)
SSB = (SSC, SSC)
SSC = (SSZ, SSZ)
SSZ = (SSB, SSB)
XXX = (XXX, XXX)")

(def sample3 (str/split-lines sample-raw3))

(defn ends-with-symbol [game symbol]
  (filter (comp #(.endsWith % symbol) str) (keys (game :map))))

(defn gcd [x y]
  (if (zero? y)
    x
    (gcd y (mod x y))))

(defn lcm
  ([x y]
   (/ (* x y) (gcd x y)))

  ([x y & more]
   (lcm x (apply lcm y more))
   )
  )

(defn run-2 [symbol position end-symbols game]
  (if (contains? end-symbols symbol)
    position
    (recur (((game :map) symbol) (nth (game :path) (mod position (count (game :path))))) (+ 1 position) end-symbols game)
    ))


(defn run-all [symbols end-symbols game]
  (apply lcm (map #(run-2 % 0 end-symbols game) symbols))
  )

(def solution-sample-3 (let [game (parse-input sample3)
                             starts (ends-with-symbol game "A")
                             ends (set (ends-with-symbol game "Z"))
                             ]
                         (run-all starts ends game)))
(println solution-sample-3)
(assert (= solution-sample-3 6))

(def solution-part-2 (let [game (parse-input input)
                           starts (ends-with-symbol game "A")
                           ends (set (ends-with-symbol game "Z"))
                           ]
                       (run-all starts ends game)))

(println solution-part-2)
(assert (= solution-part-2 20685524831999))
