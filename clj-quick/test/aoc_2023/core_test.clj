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

;;; Day 2 ;;;

(def day-2a-example
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(deftest test-day-2a
  (testing "day-2a example"
    (is (= 8 (apply + (ac/parse-day-2a day-2a-example))))
  (testing "day-2a solution"
    (is (= 2679 (ac/day-2a "day-2-input.txt"))))))

(deftest test-day-2b
  (testing "day-2b example"
    (is (= 2286 (apply + (map ac/parse-day-2b day-2a-example))))
  (testing "day-2a solution"
    (is (= 77607 (ac/day-2b "day-2-input.txt"))))))