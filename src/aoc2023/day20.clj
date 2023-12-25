(ns aoc2023.day20
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

(defn parse-line
  [line]
  (let [[_ type id outputs] (re-find #"([&%]?)([a-z]+) -> (.*)" line)]
    [(keyword id)
     {:type type
      :outputs (mapv keyword (str/split outputs #", "))}]))

(defn send-pulse [modules pulse from queue]
  (into queue (map (partial vector pulse from)) (-> modules from :outputs)))

(defn resolve-queue
  [{:keys [modules queue pulse-count]}]
  (loop [modules modules
         queue queue
         pulse-count pulse-count]
    (let [[pulse from to] (peek queue)
          pq (pop queue)]
      (if pulse
        (let [{:keys [type flip-flop-state conjunction-memory]}
              (modules to)
              pulse-count (update pulse-count pulse inc)]
          (case type
            "%" (case pulse
                  :high (recur modules pq pulse-count)
                  :low (let [pulse (if flip-flop-state :low :high)]
                         ;; modules unupdated in send-pulse, doesn't matter since only
                         ;; the static part is used
                         (recur (update-in modules [to :flip-flop-state] not) ; to is the new from
                                (send-pulse modules pulse to pq)
                                pulse-count)))
            "&" (let [conjunction-memory (assoc conjunction-memory from pulse)
                      new-pulse (if (every? #{:high} (vals conjunction-memory))
                                  :low :high)]
                  (recur (assoc-in modules [to :conjunction-memory] conjunction-memory)
                         (send-pulse modules new-pulse to pq)
                         pulse-count))
            nil (recur modules pq pulse-count)
            :rx-output (recur (update-in modules [:rx :cnt] inc) pq pulse-count)))
        {:queue queue :modules modules :pulse-count pulse-count}))))

(defn extract-and-reset-rx
  [state]
  (-> state
      (assoc :rx-cnt (-> state :modules :rx :cnt))
      (assoc-in [:modules :rx :cnt] 0)))

(defn push-button
  [{:keys [modules], :as state}]
  (-> state
      (update-in [:pulse-count :low] inc)
      (update :queue (partial send-pulse modules :low :broadcaster))
      resolve-queue
      extract-and-reset-rx))

(defn init-conjunction-memories
  [modules]
  (reduce (fn [modules [in out]]
            (cond-> modules
              (= "&" (-> modules out :type))
              (assoc-in [out :conjunction-memory in] :low)))
          modules
          (mapcat (fn [[in {:keys [outputs]}]]
                    (map (fn [out] [in out]) outputs))
                  modules)))


(defn input->init
  [input]
  (let [modules (->> (str/split-lines input)
                     (map parse-line)
                     (into {:rx {:type :rx-output :cnt 0}})
                     init-conjunction-memories)]
    {:modules modules 
     :queue clojure.lang.PersistentQueue/EMPTY
     :pulse-count {:low 0 :high 0}}))



(defn nth* [n coll] (nth coll n))

(->> (util/slurp-input)
     input->init
     (iterate push-button)
     (nth* 1000)
     :pulse-count
     vals
     (apply *))

;; part 2

;; bruteforcing too slow...
