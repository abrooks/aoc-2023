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

;;; Day 6 ;;;

(def day-6-examples
  {:example ["Time:      7  15  30"
   "Distance:  9  40  200"]
   :results [4 8 9]})

(deftest test-day-6a
  (testing "day-6a example"
    (is (= (apply * (day-6-examples :results))
           (ac/day-6a (:example day-6-examples)))))
  (testing "day-6a solution"
    (is (= 1108800 (ac/day-6a (read-data "day-6-input.txt"))))))

(deftest test-day-6b
  (testing "day-6b example"
    (is (= 71503 (ac/day-6b (:example day-6-examples)))))
  (testing "day-6b solution"
    (is (= 36919753 (ac/day-6b (read-data "day-6-input.txt"))))))

;;; Day 7 ;;;

(def day-7-examples
  {:example ["32T3K 765"
             "T55J5 684"
             "KK677 28"
             "KTJJT 220"
             "QQQJA 483"]})

(deftest test-day-7a
  (testing "day-7a example"
    (is (= 6440 (ac/day-7a (:example day-7-examples)))))
  (testing "day-7a solution"
    (is (= 253866470 (ac/day-7a (read-data "day-7-input.txt"))))))

(deftest test-day-7b
  (testing "day-7b example"
    (is (= 5905 (ac/day-7b (:example day-7-examples)))))
  (testing "day-7b solution"
    (is (= 254494947 (ac/day-7b (read-data "day-7-input.txt"))))))

;;; Day 8 ;;;

(def day-8a-examples
  {:examples [["RL"
               ""
               "AAA = (BBB, CCC)"
               "BBB = (DDD, EEE)"
               "CCC = (ZZZ, GGG)"
               "DDD = (DDD, DDD)"
               "EEE = (EEE, EEE)"
               "GGG = (GGG, GGG)"
               "ZZZ = (ZZZ, ZZZ)"]
              ["LLR"
               ""
               "AAA = (BBB, BBB)"
               "BBB = (AAA, ZZZ)"
               "ZZZ = (ZZZ, ZZZ)"]]
   :results [2 6]})

(deftest test-day-8a
  (testing "day-8a example"
    (is (= (:results day-8a-examples) (map ac/day-8a (:examples day-8a-examples)))))
  (testing "day-8a solution"
    (is (= 20659 (ac/day-8a (read-data "day-8-input.txt"))))))

(def day-8b-examples
  {:example ["LR"
             ""
             "11A = (11B, XXX)"
             "11B = (XXX, 11Z)"
             "11Z = (11B, XXX)"
             "22A = (22B, XXX)"
             "22B = (22C, 22C)"
             "22C = (22Z, 22Z)"
             "22Z = (22B, 22B)"
             "XXX = (XXX, XXX)"]
   :result 6})

(deftest test-day-8b
  (testing "day-8b example"
    (is (= (:result day-8b-examples) (ac/day-8b (:example day-8b-examples)))))
  (testing "day-8b solution"
    (is (= 15690466351717 (ac/day-8b (read-data "day-8-input.txt"))))))

;;; Day 9 ;;;

(def day-9-examples
  {:examples ["0 3 6 9 12 15"
              "1 3 6 10 15 21"
              "10 13 16 21 30 45"]
   :results-a [18 28 68]
   :results-b [-3 0 5]})

(deftest test-day-9a
  (testing "day-9a example"
    (is (= (apply + (day-9-examples :results-a))
           (ac/day-9a (:examples day-9-examples)))))
  (testing "day-9a solution"
    (is (= 2098530125 (ac/day-9a (read-data "day-9-input.txt"))))))


(deftest test-day-9b
  (testing "day-9b example"
    (is (= (apply + (day-9-examples :results-b))
           (ac/day-9b (:examples day-9-examples)))))
  
  (testing "day-9b solution"
    (is (= 1016 (ac/day-9b (read-data "day-9-input.txt"))))))

;;; Day 10 ;;;

(def day-10a-examples
  {:examples [["-L|F7"
               "7S-7|"
               "L|7||"
               "-L-J|"
               "L|-JF"]
              ["7-F7-"
               ".FJ|7"
               "SJLL7"
               "|F--J"
               "LJ.LJ"]]
   :results [4 8]})

(deftest test-day-10a
  (testing "day-10a example"
    (is (= (:results day-10a-examples)
           (map ac/day-10a (:examples day-10a-examples)))))

  (testing "day-10a solution"
    (is (= 6828 (ac/day-10a (read-data "day-10-input.txt"))))))

(def day-10b-examples
  {:examples [["..........."
               ".S-------7."
               ".|F-----7|."
               ".||.....||."
               ".||.....||."
               ".|L-7.F-J|."
               ".|..|.|..|."
               ".L--J.L--J."
               "..........."]
              [".........."
               ".S------7."
               ".|F----7|."
               ".||....||."
               ".||....||."
               ".|L-7F-J|."
               ".|..||..|."
               ".L--JL--J."
               ".........."]
              [".F----7F7F7F7F-7...."
               ".|F--7||||||||FJ...."
               ".||.FJ||||||||L7...."
               "FJL7L7LJLJ||LJ.L-7.."
               "L--J.L7...LJS7F-7L7."
               "....F-J..F7FJ|L7L7L7"
               "....L7.F7||L7|.L7L7|"
               ".....|FJLJ|FJ|F7|.LJ"
               "....FJL-7.||.||||..."
               "....L---J.LJ.LJLJ..."]
              ["FF7FSF7F7F7F7F7F---7"
               "L|LJ||||||||||||F--J"
               "FL-7LJLJ||||||LJL-77"
               "F--JF--7||LJLJ7F7FJ-"
               "L---JF-JLJ.||-FJLJJ7"
               "|F|F-JF---7F7-L7L|7|"
               "|FFJF7L7F-JF7|JL---7"
               "7-L-JL7||F7|L7F-7F7|"
               "L.L7LFJ|||||FJL7||LJ"
               "L7JLJL-JLJLJL--JLJ.L"]]
   :results [4 4 8 10]})

(deftest test-day-10b
  (testing "day-10b example"
    (is (= (:results day-10b-examples)
           (map ac/day-10b (:examples day-10b-examples)))))

  (testing "day-10b solution"
    (is (=  459 (ac/day-10b (read-data "day-10-input.txt"))))))

;;; Day 11 ;;;
(def day-11-examples
  {:example ["...#......"
             ".......#.."
             "#........."
             ".........."
             "......#..."
             ".#........"
             ".........#"
             ".........."
             ".......#.."
             "#...#....."]
   :result [374 8410]})

(deftest test-day-11a
  (testing "day-11a example"
    (is (= (first (:result day-11-examples))
           (ac/day-11a 2 (:example day-11-examples)))))
  (testing "day-11a solution"
    (is (= 9599070 (ac/day-11a 2 (read-data "day-11-input.txt"))))))

(deftest test-day-11b
  (testing "day-11b example"
    (is (= (second (:result day-11-examples))
           (ac/day-11b 100 (:example day-11-examples)))))
  (testing "day-11b solution"
    (is (= 842645913794 (ac/day-11b (* 1000 1000) (read-data "day-11-input.txt"))))))

;;; Day 12 ;;;

(def day-12-examples
  {:examples ["???.### 1,1,3"
              ".??..??...?##. 1,1,3"
              "?#?#?#?#?#?#?#? 1,3,1,6"
              "????.#...#... 4,1,1"
              "????.######..#####. 1,6,5"
              "?###???????? 3,2,1"]
   :results [1 4 1 1 4 10]
   :results-b [1 16384 1 16 2500 506250]})

(deftest test-day-12a
  (testing "day-12a example"
    (is (= (:results day-12-examples)
           (map (comp ac/day-12a vector) (:examples day-12-examples)))))
  (testing "day-12a solution"
    (is (= 7407 (ac/day-12a (read-data "day-12-input.txt"))))))

#_
(deftest test-day-12b
  (testing "day-12b example"
    (is (= (:results-b day-12-examples)
           (map (comp ac/day-12b vector) (:examples day-12-examples)))))
  (testing "day-12b solution"
    (is (= -1 (ac/day-12b (read-data "day-12-input.txt"))))))


;;; Day 13 ;;;

(def day-13-examples
  {:examples ["#.##..##."
              "..#.##.#."
              "##......#"
              "##......#"
              "..#.##.#."
              "..##..##."
              "#.#.##.#."
              ""
              "#...##..#"
              "#....#..#"
              "..##..###"
              "#####.##."
              "#####.##."
              "..##..###"
              "#....#..#"]
   :result [5 400]
   :result-b [300 100]})

(deftest test-day-13a
  (testing "day-13a example"
    (is (= (->> day-13-examples :result (apply +))
           (ac/day-13a (:examples day-13-examples))))
  (testing "day-13a solution"
    (is (= 30705 (ac/day-13a (read-data "day-13-input.txt")))))))

(deftest test-day-13b
  (testing "day-13b example"
    (is (= (->> day-13-examples :result-b (apply +))
           (ac/day-13b (:examples day-13-examples))))
  (testing "day-13b solution"
    (is (= 44615 (ac/day-13b (read-data "day-13-input.txt")))))))

;;; Day 14 ;;;

(def day-14-examples
  {:example ["O....#...."
             "O.OO#....#"
             ".....##..."
             "OO.#O....O"
             ".O.....O#."
             "O.#..O.#.#"
             "..O..#O..O"
             ".......O.."
             "#....###.."
             "#OO..#...."]
   :result 136})

(deftest test-day-14a
  (testing "day-14a example"
    (is (= (->> day-14-examples :result)
           (ac/day-14a (:example day-14-examples)))))
  (testing "day-14a solution"
    (is (= 107053 (ac/day-14a (read-data "day-14-input.txt"))))))

;;; Day 25 ;;;

(def day-25-examples
  {:example ["jqt: rhn xhk nvd"
             "rsh: frs pzl lsr"
             "xhk: hfx"
             "cmg: qnr nvd lhk bvb"
             "rhn: xhk bvb hfx"
             "bvb: xhk hfx"
             "pzl: lsr hfx nvd"
             "qnr: nvd"
             "ntq: jqt hfx bvb xhk"
             "nvd: lhk"
             "lsr: lhk"
             "rzs: qnr cmg lsr rsh"
             "frs: qnr lhk lsr"]
   :result 54})

(deftest test-day-25a
  (testing "day-25a example"
    (is (= (->> day-25-examples :result)
           (ac/day-25a (:example day-25-examples)))))
  (testing "day-25a solution"
    (is (= 507626 (ac/day-25a (read-data "day-25-input.txt"))))))