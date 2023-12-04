(ns aoc2023.day1
  (:require [aoc2023.util :as util]
            [clojure.string :as str]))

(->> (util/input-lines)
     (map (fn [line]
            (let [digits (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} line)
                  f (first digits)
                  l (last digits)]
              (parse-long (str f l)))))
     (reduce +))

;; part 2


(def re-first #"\d|(?:one)|(?:two)|(?:three)|(?:four)|(?:five)|(?:six)|(?:seven)|(?:eight)|(?:nine)")

(def re-last #"\d|(?:eno)|(?:owt)|(?:eerht)|(?:ruof)|(?:evif)|(?:xis)|(?:neves)|(?:thgie)|(?:enin)")

(def text->digit*
  {"one" \1
   "two" \2
   "three" \3
   "four" \4
   "five" \5
   "six" \6
   "seven" \7
   "eight" \8
   "nine" \9
   "eno" \1
   "owt" \2
   "eerht" \3
   "ruof" \4
   "evif" \5
   "xis" \6
   "neves" \7
   "thgie" \8
   "enin" \9})

(defn text->digit
  [s]
  (or (text->digit* s) s))

(->> (util/input-lines)
     (map (fn [line]
            (let [f (text->digit (re-find re-first line))
                  l (text->digit (re-find re-last (str/reverse line)))]
              (parse-long (str f l)))))
     (reduce +))
