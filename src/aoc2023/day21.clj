(ns aoc2023.day21
  (:require
    [aoc2023.util :as util]
    [clojure.string :as str]))

(defn parse-input
  [input]
  (let [carte (->> (str/split-lines input) util/lines->2d-map)
        start-tile (some (fn [entry] (when (= \S (val entry)) (key entry)))
                         carte)]
    {:start-tile start-tile
     :carte (assoc carte start-tile \.)}))

(defn step-fn
  [carte]
  (fn [O-tiles]
    (into #{}
          (comp (mapcat util/manhattan-neighbors)
                (filter (comp #{\.} carte)))
          O-tiles)))

(let [{:keys [start-tile carte]} (parse-input (util/slurp-input))
      step (step-fn carte)]
  (count (nth (iterate step #{start-tile}) 64)))


;; part 2

;; later...
