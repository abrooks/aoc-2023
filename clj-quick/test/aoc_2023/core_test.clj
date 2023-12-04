(ns aoc-2023.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc-2023.core :as ac]))

;; This has been a typical pattern in previous AoC puzzles
(defn test-example-map [fn-under-test example-map]
  (doseq [[example result] (map vector (:examples example-map)
                                       (:results example-map))]
    (is (= result (fn-under-test example)))))

;;; Day 1 ;;;

(def day-1a-examples 
  {:examples ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]
   :results [12 38 15 77]})

(deftest test-day-1a
  (testing "day-1a examples"
    (test-example-map ac/extract-digits day-1a-examples))
  (testing "day-1a solution"
    (is (= 54159 (ac/day-1a "day-1-input.txt")))))

(def day-1b-examples
  {:examples ["two1nine" "eightwothree"
              "abcone2threexyz" "xtwone3four"
              "4nineeightseven2" "zoneight234"
              "7pqrstsixteen"]
   :results [29 83 13 24 42 14 76]})

(deftest test-day-1b
  (testing "day-1b examples"
    (test-example-map ac/extract-digits-two day-1b-examples))
  (testing "day-1b solution"
    (is (= 53866 (ac/day-1b "day-1-input.txt")))))