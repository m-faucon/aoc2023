(ns aoc2023.day17
  (:require
    [aoc2023.util :as util]
    [clojure.string :as str]
    [clojure.data.priority-map :as priority-map]))

(def other-dir
  ;; in C, would use a boolean, but this is a high level language so we want to waste ram and cpu
  {:vertical :horizontal
   :horizontal :vertical})

(def dir->dtiles1
  {:vertical (mapv (fn [dy] [0 dy]) [-3 -2 -1 1 2 3])
   :horizontal (mapv (fn [dx] [dx 0]) [-3 -2 -1 1 2 3])})

(def dirs (keys dir->dtiles1))

(defn init
  [{:keys [L H]}]
  (let [h-start {:tile [0 0] :dir :horizontal}
        v-start {:tile [0 0] :dir :vertical}]
    {:queue (priority-map/priority-map h-start 0 v-start 0)
     :mins (assoc (into {} (for [x (range L) y (range H) dir dirs] [{:tile [x y] :dir dir} Integer/MAX_VALUE]))
                  h-start 0
                  v-start 0)}))

(defn candidates-fn
  [carte dir->dtiles]
  (let [inside? (comp (partial util/inside-tile? carte) :tile)]
    (fn [{:keys [dir tile]}]
      (let [new-dir (other-dir dir)]
        (->> (map (fn [dtile] {:dir new-dir
                               :tile (util/tile-add tile dtile)})
                  (dir->dtiles new-dir))
             (filter inside?))))))

(defn in-between-tiles
  [old new]
  ;; oh no we are recomputing stuff many times for all the candidate tiles
  ;; who cares
  (let [[dx dy] (util/tile-diff old new)
        dtiles
        (if (zero? dx)
          (if (pos? dy)
            (for [y (range dy)] [0 (inc y)])
            (for [y (range (- dy))] [0 (- (inc y))]))
          (if (pos? dx)
            (for [x (range dx)] [(inc x) 0])
            (for [y (range (- dx))] [(- (inc y)) 0]))) ]
    (map (partial util/tile-add old) dtiles)))

(defn step-fn
  [{:keys [L H] :as carte} dir->dtiles]
  (let [candidates (candidates-fn carte dir->dtiles)]
    (fn [{:keys [queue mins]}]
      (let [[{:keys [tile] :as old} old-loss] (peek queue)
            [to-queue mins]
            (reduce (fn [[to-queue mins] {new-tile :tile :as new}]
                      (let [loss (reduce +
                                         old-loss
                                         (map carte (in-between-tiles tile new-tile)))]
                        (if (< loss (mins new))
                          [(conj to-queue [new loss]) (assoc mins new loss)]
                          [to-queue mins])))
                    [[] mins]
                    (candidates old))]
        {:queue (into (pop queue) to-queue)
         :mins mins
         :done (when (= tile [(dec L) (dec H)]) old-loss)}))))

(let [carte (-> (util/input-map)
                (util/update-tiles-vals util/parse-digit))
      step (step-fn carte dir->dtiles1)]
  (->> (init carte)
       (iterate step)
       (keep :done)
       first))

;; part 2

(def dir->dtiles2
  (let [r (into [] (concat (range -10 -3) (range 4 11)))]
    {:vertical (mapv (fn [dy] [0 dy]) r)
     :horizontal (mapv (fn [dx] [dx 0]) r)}))

(let [carte (-> (util/input-map)
                (util/update-tiles-vals util/parse-digit))
      step (step-fn carte dir->dtiles2)]
  (->> (init carte)
       (iterate step)
       (keep :done)
       first))
