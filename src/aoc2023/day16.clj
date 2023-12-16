(ns aoc2023.day16
  (:require
   [aoc2023.util :as util]))

(def tile-kind->dir->new-dirs
  {\. vector
   \/ {util/right [util/up]
       util/up [util/right]
       util/left [util/down]
       util/down [util/left]}
   \\ {util/right [util/down]
       util/down [util/right]
       util/up [util/left]
       util/left [util/up]}
   \- {util/left [util/left]
       util/right [util/right]
       util/down [util/left util/right]
       util/up [util/left util/right]}
   \| {util/up [util/up]
       util/down [util/down]
       util/right [util/up util/down]
       util/left [util/up util/down]}})

; beam head format : [[x y] [dx dy]]

(defn advance-beam-head-fn
  [carte]
  (fn [[tile dir]]
    (keep (fn [new-dir]
            (let [new-tile (util/tile-add tile new-dir)]
              (when (carte new-tile)
                [new-tile new-dir])))
          ((tile-kind->dir->new-dirs (carte tile)) dir))))

(defn step-fn
  [carte]
  (let [advance-beam-head (advance-beam-head-fn carte)]
    (fn [{:keys [beam beam-heads] :as state}]
      (let [new-beam (into beam beam-heads)]
        (assoc state
               :beam new-beam
               :beam-heads (->> (mapcat advance-beam-head beam-heads)
                                (remove beam)))))))

(defn count-energized
  [{:keys [beam]}]
  (count (into #{} (map first) beam)))

(defn init
  [init-beam-head]
  {:beam #{}
   :beam-heads [init-beam-head]})

(defn carte->init-beam-head->count-energized
  [carte]
  (let [step (step-fn carte)]
    (fn [ibh]
      (->> (init ibh)
           (iterate step)
           (drop-while (comp seq :beam-heads))
           first
           count-energized))))

((carte->init-beam-head->count-energized (util/input-map))
 [[0 0] util/right])

;; part 2

(defn init-beam-heads
  [{:keys [L H]}]
  (concat (mapcat (fn [x] [[[x 0] util/down] [[x (dec H)] util/up]]) (range L))
          (mapcat (fn [y] [[[0 y] util/right] [[(dec L) y] util/left]]) (range H))))

(let [carte (util/input-map)]
  (transduce
   (map (carte->init-beam-head->count-energized carte))
   max 0 (init-beam-heads carte)))

;; dev

(comment
  (defn viz [{:keys [H L] :as carte} {:keys [beam]}]
    (let [vizdir {util/right \> util/left \< util/up \^ util/down \v}
          beam-map (reduce (fn [acc [tile dir]]
                             (let [cur (acc tile)]
                               (assoc acc tile
                                      (cond
                                        (not cur) (vizdir dir)
                                        (int? cur) (inc cur)
                                        :else 2))))
                           {}
                           beam)]
      (println "-----------------------------------------------------------------------")
      (println)
      (run! (fn [y] (println (apply str (for [x (range L)]
                                          (or (#{\- \| \/ \\} (carte [x y]))
                                              (beam-map [x y])
                                              (carte [x y]))))))
            (range H))
      (println)
      (println "-----------------------------------------------------------------------")
      (println))))
