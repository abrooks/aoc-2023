(ns aoc-2023.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-data [filename]
  (str/split-lines (slurp (io/resource filename))))

(defn extract-digits [line]
  (let [digits (re-seq #"\d" line)]
     (Integer/parseInt (str (first digits) (last digits)))))

(defn day-1a []
  (->>
   (read-data "day-1-input.txt")
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

(defn day-1b []
  (->>
   (read-data "day-1-input.txt")
   (map extract-digits-two)
   (apply +)))

(comment
  (require 'aoc-2023.core :reload)
  (require 'aoc-2023.core-test :reload)
  (clojure.test/run-tests 'aoc-2023.core-test))