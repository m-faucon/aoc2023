(ns aoc2023.day18
  (:require
    [aoc2023.util :as util]
    [clojure.edn :as edn]))

(def dirs
  {"R" util/right
   "L" util/left
   "D" util/down
   "U" util/up})

;; ah well, I thought the color would be useful later but it isn't.
;; So, I maintain a hashmap of tile->color even though it could
;; just be a set. I'm keeping it a map because of convenience :
;; I can assoc other state keys in a reduce, put a :H and :L key afterwards.

(defn parse-line
  [line]
  (let [[_ dir length color] (re-find #"([RLDU]) (\d+) (.*)" line)]
    {:dir (dirs dir)
     :length (parse-long length)
     :color color}))

(defn normalize-coords
  [m]
  (let [ks (keys m)
        x-min (apply min (map first ks))
        x-max (apply max (map first ks))
        y-min (apply min (map second ks))
        y-max (apply max (map second ks))]
    (-> (update-keys m (partial util/tile-add [(- x-min) (- y-min)]))
        (assoc :L (inc (- x-max x-min)))
        (assoc :H (inc (- y-max y-min))))))

(defn dig-rf
  ([{:keys [drill-head] :as acc} {:keys [dir color]}]
   (let [new-head (util/tile-add drill-head dir)]
     (assoc acc
            :drill-head new-head
            new-head color)))
  ;; not strictly necessary, but removes some thinking afterwards
  ([acc] (normalize-coords (dissoc acc :drill-head))))

(defn dig
  [instructions]
  (transduce
   (mapcat (fn [instruction] (repeat (:length instruction) instruction)))
   dig-rf
   {:drill-head [0 0]}
   instructions))

(defn flood-fill-step
  [{:keys [m queue]}]
  (let [tile (peek queue)]
    (reduce (fn [{:keys [m] :as acc} tile]
              (if-not (m tile)
                (-> (assoc-in acc [:m tile] :inside)
                    (update :queue conj tile))
                acc))
            {:queue (pop queue) :m m}
            (util/manhattan-neighbors m tile))))

(defn find-one-inside-square
  [{:keys [L] :as m}]
  (let [x (first (filter (fn [x] (m [x 0])) (range L)))]
    ;; has to be a Γ scenario
    [(inc x) 1]))

(defn flood-fill
  ([m init-square]
   (->> {:queue [init-square] :m (assoc m init-square :inside)}
        (iterate flood-fill-step)
        (drop-while (comp seq :queue))
        first :m))
  ([m] (flood-fill m (find-one-inside-square m))))

(defn cnt-dug
  [m]
  (count (dissoc m :H :L)))

(->> (util/input-lines)
     (map parse-line)
     dig
     flood-fill
     cnt-dug)

(comment
  (defn viz
    [{:keys [L H] :as m}]
    ;; here it is nice to have a dedicated stdout buffer
    ;; have to zoom out though
    (println) (println) (println (apply str (repeat 500 \~))) (println)
    (run! (fn [y]
            (println (apply str (map (fn [x] (if (m [x y]) \# \.))
                                     (range L)))))
          (range H))
    (println) (println) (println (apply str (repeat 500 \~))) (println)))

;; part 2

;; pff all of that for nothing, now we have to find another way

;; that would be using formulas that break the puzzle

(def dirs2
  {"0" util/right
   "1" util/down
   "2" util/left
   "3" util/up})


(defn parse-line2
  [line]
  (let [[_ hex-length dir] (re-find #"[RLDU] \d+ \(#(.....)(.)\)" line)]
    {:dir (dirs2 dir)
     :length (edn/read-string (str "0x" hex-length))}))

(defn vertices-seq
  [instructions]
  ;; pop because otherwise we have twice [0 0]
  (pop (reduce (fn [tiles {:keys [dir length]}]
             (conj tiles
                   (util/tile-add (peek tiles) (util/tile-scale length dir))))
           [[0 0]] instructions)))

(defn det
  [[ux uy] [vx vy]]
  (- (* ux vy) (* uy vx)))

(defn shoelace
  "see https://en.wikipedia.org/wiki/Shoelace_formula"
  [tiles]
  (->
   (transduce
    (map (partial apply det))
    + (partition 2 1 tiles))
   (/ 2)))

(defn num-of-interior-points
  "See https://en.wikipedia.org/wiki/Pick's_theorem"
  [A b]
  (- (inc A) (/ b 2)))

(defn num-of-boundary-points
  [tiles]
  (transduce
   (map (partial apply util/manhattan-distance))
   + (partition 2 1 (cons (last tiles) tiles))))

(defn vertices->num-of-points
  [tiles]
  (let [b (num-of-boundary-points tiles)
        A (shoelace tiles)
        i (num-of-interior-points A b)]
    (+ b i)))

(->> (util/input-lines)
     (map parse-line2)
     vertices-seq
     vertices->num-of-points)
