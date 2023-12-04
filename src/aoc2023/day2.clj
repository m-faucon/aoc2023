(ns aoc2023.day2
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

(defn parse-turn
  [turn]
  (into {}
        (map (fn [subturn]
               (let [[_ n color] (re-find #"(\d+) (\w+)" subturn)]
                 [color (parse-long n)])))
        (str/split turn #",")))

(defn parse-game
  [game]
  (let [[_ id content] (re-find #"Game (\d+): (.*)" game)
        id (parse-long id)
        turns (map parse-turn (str/split content #";"))]
    {:id id :turns turns}))

(defn possible-turn?
  [bag turn]
  (every? (fn [color] (<= (turn color) (bag color)))
          (keys turn)))

(defn possible-game?
  [bag game]
  (every? (partial possible-turn? bag) (:turns game)))

(transduce (comp (map parse-game)
                 (filter (partial possible-game? {"red" 12 "green" 13 "blue" 14}))
                 (map :id))
           + (util/input-lines))
;; part 2

(defn turn-rf
  [bag turn]
  (reduce (fn [bag color]
            (assoc bag color (max (bag color) (turn color))))
          bag
          (keys turn)))

(defn game->min-bag
  [{:keys [turns]}]
  (reduce turn-rf {"red" 0 "blue" 0 "green" 0} turns))

(transduce (comp (map parse-game)
                 (map game->min-bag)
                 (map (fn [bag] (reduce * (vals bag)))))
           + (util/input-lines))
