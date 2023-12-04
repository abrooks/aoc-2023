(ns aoc-2023.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(defn read-data [filename]
  (str/split-lines (slurp (io/resource filename))))

;;; Day 1 ;;;

(defn extract-digits [line]
  (let [digits (re-seq #"\d" line)]
     (Integer/parseInt (str (first digits) (last digits)))))

(defn day-1a [f]
  (->>
   (read-data f)
   (map extract-digits)
   (apply +)))

(def digit-mapping
   ;; Day 1b has a terribly ambiguous set of requirements
   ["zerone" "01"
    "oneight" "18"
    "twone" "21"
    "threeight" "38"
    "fiveight" "58"
    "sevenine" "79"
    "eightwo" "82"
    "eighthree" "83"
    "zero" "0"
    "one" "1"
    "two" "2"
    "three" "3"
    "four" "4"
    "five" "5"
    "six" "6"
    "seven" "7"
    "eight" "8"
    "nine" "9"])

(def digit-map (apply hash-map (interleave digit-mapping)))

(def digit-pattern
  (re-pattern (str "\\d|" (str/join "|" (take-nth 2 digit-mapping)))))

(defn extract-digits-two [line]
  (let [digits (re-seq digit-pattern line)
        digits (apply str (map #(digit-map % %) digits))] 
    (Integer/parseInt (str (first digits) (last digits)))))

(defn day-1b [f]
  (->>
   (read-data f)
   (map extract-digits-two)
   (apply +)))

;;; Day 2 ;;;

(def day-2-grammar
  (insta/parser
   "<game> = <'Game '> number <': '> grabs
    <grabs> = grab (<'; '> grab)*
    <grab> = colorcount (<', '> colorcount)*
    <colorcount> = number <' '> color
    <number> = #'[0-9]+'
    <color> = 'red' | 'green' | 'blue'"))

(def day-2-limits {"red" 12 "green" 13 "blue" 14})

(defn over-limit? [[count color]]
  (let [count (Integer/parseInt count) ]
     (< (day-2-limits color) count)))

(defn parse-day-2a [lines]
  (for [line lines
        :let [[day & counts] (insta/parse day-2-grammar line)]
        :when (not (some over-limit? (partition 2 counts)))]
      (Integer/parseInt day)))

(defn day-2a [f]
  (->>
   (read-data f)
   parse-day-2a
   (apply +)))

(defn parse-day-2b [game]
  (->> game
       (insta/parse day-2-grammar)
       (drop 1)
       (partition 2) ; [[count color]...]
       (map (fn [[v k]] (hash-map k (Integer/parseInt v))))
       (apply merge-with max)
       vals
       (apply *)))

(defn day-2b [f]
  (->>
    (read-data f)
    (map parse-day-2b)
    (apply +)))

(comment
  (require 'clojure.test)
  (require 'aoc-2023.core :reload)
  (require 'aoc-2023.core-test :reload)
  (clojure.test/run-tests 'aoc-2023.core-test)
  ;; Keep kondo happy
  [day-1a day-1b day-2a day-2b])