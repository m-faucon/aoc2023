(ns aoc2023.day10
  (:require [aoc2023.util :as util]))

;; warning : this is a mess

(defn update-map-format
  ;; this was needed for part 2, but since
  ;; we're using fns from part 1 there,
  ;; we're going to use this format also for part 1.
  [m]
  (reduce (fn [m k]
            (assoc m k {:kind (m k)}))
          m
          (util/tiles m)))

(defn starting-tile
  [m]
  (->> (filter (comp #{\S} :kind val) m)
       first key))

(defn step-tile
  [m old-tile tile]
  (let [tile-kind (-> tile m :kind)
        tile-diff (util/tile-diff old-tile tile)
        newtile-diff
        (condp = [tile-kind tile-diff] ; can't use case with symbols...
          [\| util/down] util/down
          [\| util/up] util/up
          [\- util/left] util/left
          [\- util/right] util/right
          [\L util/down] util/right
          [\L util/left] util/up
          [\J util/down] util/left
          [\J util/right] util/up
          [\7 util/right] util/down
          [\7 util/up] util/left
          [\F util/left] util/down
          [\F util/up] util/right
          ;; else, including \. tile and out-of-map tiles
          :stop)]
    (or (#{:stop} newtile-diff)
        (util/tile-add tile newtile-diff))))

(defn step-path
  [m]
  (fn [{:keys [lst pen stopping] :as step}]
    (let [newtile (step-tile m pen lst)]
      (cond
        ;; this is just because take-while is annoying,
        ;; it's cumbersome to also get the last one
        stopping (assoc step :stop stopping)
        (= :stop newtile) (assoc step :stopping :stopped)
        (= \S (-> newtile m :kind)) (assoc step :lst newtile :stopping :looped)
        :else (assoc step :pen lst :lst newtile)))))

(defn starters
  [m]
  (let [s (starting-tile m)]
    (->> [(util/tile-add s util/up)
          (util/tile-add s util/down)
          (util/tile-add s util/left)
          (util/tile-add s util/right)]
         (filter (fn [t] (when-let [c (m t)] (not= \. (:kind c)))))
         (map (fn [second-tile]
                {:pen s :lst second-tile})))))

(defn build-path
  [m starter]
  (take-while (complement :stop) (iterate (step-path m) starter)))

(defn loop?
  [path]
  (-> path last :stopping (= :looped)))

(let [m (update-map-format (util/input-map))]
  (/ (->> (map (partial build-path m) (starters m))
          (filter loop?)
          first
          count)
     2))


;; part2

(defn get-loop-tiles
  [m]
  (->> (map (partial build-path m) (starters m))
       (filter loop?)
       first
       (map :lst)))

(comment
  (let [{:keys [L H] :as m} (update-map-format (util/input-map))]
    (->> (get-loop-tiles m)
         (filter (some-fn (comp #{0} first)
                          (comp #{(dec L)} first)
                          (comp #{0} second)
                          (comp #{(dec H)} second)))
         (map (juxt identity m))
         ))
  '([[139 96] {:kind \7}] [[139 97] {:kind \J}] [[91 0] {:kind \F}] [[92 0] {:kind \7}])
  ;; let's use the first two to start our loop since we know
  ;; which side (right/left) is outside
  ;; we could write some code to identify this and work
  ;; with any input, but why.
  ;; WE DECIDE TO START GOING [\7 -> \J]
  ;; SO : LEFT IS OUTSIDE, RIGHT IS INSIDE
  ;; (left and right being relative to the trajectory taken)
  ;;
  ;; I guess what we could do if we do not want to hardcode a choice
  ;; of direction, is to compute both 'left' and 'right' components
  ;; and later determine which is which (cause one has to be the unbounded cc)
  )

;; copy paste and modify the function from part 1
(defn step-tile-marking-inside
  [{:keys [lst pen start] :as m}]
  (let [tile-kind (-> lst m :kind)
        tile-diff (util/tile-diff pen lst)
        [newtile-diff inside-tile-diffs]
        (condp = [tile-kind tile-diff]
          [\| util/down] [util/down [util/left]]
          [\| util/up] [util/up [util/right]]
          [\- util/left] [util/left [util/up]]
          [\- util/right] [util/right [util/down]]
          [\L util/down] [util/right [util/left util/down]]
          [\L util/left] [util/up []]
          [\J util/down] [util/left []]
          [\J util/right] [util/up [util/down util/right]]
          [\7 util/right] [util/down []]
          [\7 util/up] [util/left [util/right util/up]]
          [\F util/left] [util/down [util/up util/left]]
          [\F util/up] [util/right []]
          ;; since we already have our loop, this shouldn't happen
          (throw (ex-info "" {:lst [lst (m lst)] :pen [pen (m pen)]})))
        newtile (util/tile-add lst newtile-diff)]
    (assert (:loop-tile (m newtile)))
    (if (= start newtile)
      (assoc m :looped true)
      (reduce (fn [m diff]
                (update m
                        (util/tile-add lst diff)
                        (fn [{:keys [loop-tile] :as tile-info}]
                          (cond-> tile-info
                            (not loop-tile)
                            (assoc :inside true)))))
              (assoc m
                     :pen lst
                     :lst newtile)
              inside-tile-diffs))))

(defn mark-loop-tiles
  [m]
  (reduce (fn [m k]
            (update m k #(assoc % :loop-tile true)))
          m
          (get-loop-tiles m)))

(defn ad-hoc-patch-S
  [m s-tile s-kind]
  (assoc-in m [s-tile :kind] s-kind))

(defn mark-inside
  [m pen lst]
  (->> (assoc m :lst lst :pen pen :start lst)
       (iterate step-tile-marking-inside)
       (drop-while (complement :looped))
       first))

(defn flood-fill-step
  [{:keys [queue] :as m}]
  (let [tile (peek queue)]
    (-> (assoc-in m [tile :inside] true)
        (assoc :queue (into (pop queue)
                            (filter (comp (complement (some-fn :loop-tile :inside)) m))
                            (util/manhattan-neighbors m tile))))))

(defn flood-fill
  [m]
  (loop [m (assoc m :queue (into [] (comp (filter (comp :inside val))
                                          (map key))
                                 m))]
    (if (-> m :queue empty?)
      m
      (recur (flood-fill-step m)))))

(->> (-> (util/input-map)
         update-map-format
         mark-loop-tiles
         (ad-hoc-patch-S [103 20] \7)
         (mark-inside [139 96] [139 97])
         flood-fill)
     (filter (comp :inside val))
     count)
