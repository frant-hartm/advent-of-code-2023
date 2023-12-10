(ns advent-of-code.day7
  (:require [clojure.string :as str])
  )

(defn read-file [input] (slurp input))
(defn read-lines [input] (str/split-lines (read-file input)))
(def input (read-lines "src/advent_of_code/day7-input"))

(def sample-raw "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def symbol-map
  {
   :A 0xe
   :K 0xd
   :Q 0xc
   :J 0xb
   :T 0xa
   :9 0x9
   :8 0x8
   :7 0x7
   :6 0x6
   :5 0x5
   :4 0x4
   :3 0x3
   :2 0x2
   })

(def sample (str/split-lines sample-raw))

(defn parse-game [game]
  (map (comp (fn [x] {:cards (first x) :bid (Integer/valueOf (second x))}) #(str/split % #" ")) game)
  )

(defn cards-to-numbers [cards]
  (vec (map #(symbol-map (keyword (str %))) (seq cards))))


(defn contains [element coll]
  (boolean (some #(= element %) coll)))

(defn hand-fullhouse [hand]
  (if (and (contains 3 hand) (contains 2 hand))
    (conj hand 3.5)
    hand
    ))

(defn hand-twopairs [hand]
  (if (= 2 (count (filter (partial = 2) hand)))
    (conj hand 2.5)
    hand
    ))

(defn hand [cards]
  ((comp hand-fullhouse hand-twopairs) (map (comp count (partial second)) (group-by identity (cards-to-numbers cards))))
  )


(defn compute-hands [game]
    (map #(assoc % :hand (hand (% :cards)) :numbers (cards-to-numbers (% :cards))) game)
  )

(defn sort-games [game]
  (sort-by (juxt #(apply max (% :hand)) #(% :numbers)) (compute-hands game))
  )

(defn rate-game [game]
  (apply + (map-indexed #(* (+ 1 %1) (%2 :bid)) (sort-games game)))
  )

(def solution-sample (rate-game (parse-game sample)))
(println solution-sample)
(assert (= solution-sample 6440))

(def solution (rate-game (parse-game input)))
(println solution)
(assert (= solution 249204891))

(def symbol-map2 (assoc symbol-map :J 1))

(defn cards-to-numbers2 [cards]
  (vec (map #(symbol-map2 (keyword (str %))) (seq cards))))

(defn hand2 [cards]
  (let [card-numbers (cards-to-numbers2 cards)
        bonus (count (filter (partial = (symbol-map2 :J)) card-numbers))
        filtered-card-numbers (filter (partial not= (symbol-map2 :J)) card-numbers)]
    ((comp #(conj % (+ bonus (apply max (conj % 0)))) hand-fullhouse hand-twopairs) (map (comp count (partial second)) (group-by identity filtered-card-numbers))))
  )

(defn compute-hands2 [game]
  (map #(assoc % :hand (hand2 (% :cards)) :numbers (cards-to-numbers2 (% :cards))) game)
  )

(defn sort-games2 [game]
  (sort-by (juxt #(apply max (% :hand)) #(% :numbers)) (compute-hands2 game))
  )

(defn rate-game2 [game]
  (apply + (map-indexed #(* (+ 1 %1) (%2 :bid)) (sort-games2 game)))
  )

(def solution-sample-part-2 (rate-game2 (parse-game sample)))
(println solution-sample-part-2)
(assert (= solution-sample-part-2 5905))

(def solution-part-2 (rate-game2 (parse-game input)))
(println solution-part-2)
(assert (= solution-part-2 249666369))
