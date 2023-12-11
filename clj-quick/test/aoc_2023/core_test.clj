(ns aoc-2023.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc-2023.core :as ac]))

;; This has been a typical pattern in previous AoC puzzles
(defn test-example-map [fn-under-test example-map]
  (doseq [[example result] (map vector (:examples example-map)
                                       (:results example-map))]
    (is (= result (fn-under-test example)))))

(defn read-data [filename]
  (str/split-lines (slurp (io/resource filename))))

;;; Day 1 ;;;

(def day-1a-examples 
  {:examples ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]
   :results [12 38 15 77]})

(deftest test-day-1a
  (testing "day-1a examples"
    (test-example-map ac/extract-digits day-1a-examples))
  (testing "day-1a solution"
    (is (= 54159 (ac/day-1a (read-data "day-1-input.txt"))))))

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
    (is (= 53866 (ac/day-1b (read-data "day-1-input.txt"))))))

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
    (is (= 2679 (ac/day-2a (read-data "day-2-input.txt")))))))

(deftest test-day-2b
  (testing "day-2b example"
    (is (= 2286 (apply + (map ac/parse-day-2b day-2a-example))))
  (testing "day-2a solution"
    (is (= 77607 (ac/day-2b (read-data "day-2-input.txt")))))))

;;; Day 3 ;;;

(def day-3-example
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(deftest test-day-3a
  (testing "day-3a example"
    (is (= 4361 (ac/day-3a day-3-example))))
  (testing "day-3a solution"
    (is (= 522726 (ac/day-3a (read-data "day-3-input.txt"))))))

(deftest test-day-3b
  (testing "day-3b example"
    (is (= 467835 (ac/day-3b day-3-example))))
  (testing "day-3a solution"
    (is (= 81721933 (ac/day-3b (read-data "day-3-input.txt"))))))

;;; Day 4 ;;;

(def day-4-examples
  {:examples
  ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
   :results [8 2 2 1 0 0]})

(deftest test-day-4a
  (testing "day-4a example"
    (is (= 13 (ac/day-4a (:examples day-4-examples)))))
  (testing "day-4a solution"
    (is (= 25004 (ac/day-4a (read-data "day-4-input.txt"))))))

(deftest test-day-4b
  (testing "day-4b example"
    (is (= 30 (ac/day-4b (:examples day-4-examples)))))
  (testing "day-4b solution"
    (is (= 14427616 (ac/day-4b (read-data "day-4-input.txt"))))))

;;; Day 5 ;;;

(def day-5-examples
  {:example (str/join "\n"
                      ["seeds: 79 14 55 13"
                       ""
                       "seed-to-soil map:"
                       "50 98 2"
                       "52 50 48"
                       ""
                       "soil-to-fertilizer map:"
                       "0 15 37"
                       "37 52 2"
                       "39 0 15"
                       ""
                       "fertilizer-to-water map:"
                       "49 53 8"
                       "0 11 42"
                       "42 0 7"
                       "57 7 4"
                       ""
                       "water-to-light map:"
                       "88 18 7"
                       "18 25 70"
                       ""
                       "light-to-temperature map:"
                       "45 77 23"
                       "81 45 19"
                       "68 64 13"
                       ""
                       "temperature-to-humidity map:"
                       "0 69 1"
                       "1 0 69"
                       ""
                       "humidity-to-location map:"
                       "60 56 37"
                       "56 93 4"])})

(deftest test-day-5a
  (testing "day-5a example"
    (is (= 35 (ac/day-5a (:example day-5-examples)))))
  (testing "day-5a solution"
    (is (= 289863851 (ac/day-5a (str/join "\n" (read-data "day-5-input.txt")))))))

(deftest test-day-5b
  (testing "day-5b example"
    (is (= 46 (ac/day-5b (:example day-5-examples)))))
  (testing "day-5b solution"
    (is (= 60568880 (ac/day-5b (str/join "\n" (read-data "day-5-input.txt")))))))