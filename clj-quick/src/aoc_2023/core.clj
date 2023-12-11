(ns aoc-2023.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [helins.interval.map :as imap]
            [instaparse.core :as insta]))

(defn dbg [label value]
  (prn label)
  (pprint value)
  value)

;;; Day 1 ;;;

(defn extract-digits [line]
  (let [digits (re-seq #"\d" line)]
     (Integer/parseInt (str (first digits) (last digits)))))

(defn day-1a [data]
  (->> data
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

(defn day-1b [data]
  (->> data
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
  (let [count (Integer/parseInt count)]
     (< (day-2-limits color) count)))

(defn parse-day-2a [lines]
  (for [line lines
        :let [[day & counts] (insta/parse day-2-grammar line)]
        :when (not (some over-limit? (partition 2 counts)))]
      (Integer/parseInt day)))

(defn day-2a [data]
  (->> data
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

(defn day-2b [data]
  (->> data
    (map parse-day-2b)
    (apply +)))

;;; Day 3 ;;;

(defn is-digit? [c]
  (contains? (set "0123456789") c))

(defn digit-other [c]
  (cond (is-digit? c) :num
        (= \. c)      :dot
        :else         :sym))

(defn day-3-parse-lines [line]
  (->> line
       (partition-by digit-other)
       (map #(apply str %))))

(defn entry-reduce-fn
  [{:keys [end]} ni]
  {:begin (if end (inc end) 0)
   :end (+ (if end end -1) (count ni))
   :item ni
   :type (digit-other (first ni))})

(defn in-perimeter [{nb :begin ne :end nc :row}
                    {sb :begin se :end sc :row}]
  (and (<= (dec nb) se)
       (<= sb (inc ne))
       (<= (dec nc) sc)
       (<= sc (inc nc))))

(defn find-real-parts [m]
  (let [{nums :num syms :sym} m]
    (for [n nums
          :when (some #(in-perimeter n %) syms)]
      (:item n))))

(defn day-3a [data]
  (->> data
   (map day-3-parse-lines)
   (map #(reductions entry-reduce-fn nil %)) 
   (mapcat (fn [i l] (map #(assoc % :row i) l)) (range))
   (group-by :type)
   (find-real-parts)
   (map #(Integer/parseInt %))
   (apply +)))

(defn find-meshed-gears [m]
  (let [{nums :num syms :sym} m]
    ;; TODO (partition 3 1 (partition-by)...)
    (for [n1 nums
          n2 nums
          :when (and (or (= (inc (:row n1)) (:row n2))
                         (= (+ 2 (:row n1)) (:row n2))
                         (and (= (:row n1) (:row n2))
                              (<  (:end n1) (:end n2))))
                     (some #(and (in-perimeter n1 %)
                                 (in-perimeter n2 %))
                           syms))]
      [(:item n1) (:item n2)])))

(defn day-3b [data]
  (->> data
   (map day-3-parse-lines)
   (map #(reductions entry-reduce-fn nil %)) 
   (mapcat (fn [i l] (map #(assoc % :row i) l)) (range))
   (group-by :type)
   (find-meshed-gears)
   (map (fn [[a b]] (* (Integer/parseInt a) (Integer/parseInt b))))
   (apply +)))

;;; Day 4 ;;;

(def day-4-grammar
  (insta/parser
   "<card> = <'Card'> <' '>+ number <': '> winners <' | '> candidates
    winners = numberseq
    candidates = numberseq
    <numberseq> = <' '>? number (<' '>+ number)*
    <number> = #'[0-9]+'"))

(defn day-4-intersection [[_ [_ & winners] [_ & candidates]]]
  (count (set/intersection (set winners) (set candidates))))

(defn day-4a [data]
  (->> data
   (map day-4-grammar)
   (map day-4-intersection)
   (map #(int (Math/floor (Math/pow 2 (dec %)))))
   (apply +)))

;; NOTE: Problem statement doesn't say
;; what happens when you go past the number of cards

(defn apply-winners [match-counts]
  (let [accumulator (volatile! (vec (repeat (count match-counts) 1)))]
    (doseq [[i count] (map-indexed vector match-counts)]
      (doseq [j (range (inc i) (+ (inc i) count))]
        (vswap! accumulator update j #(+ % (@accumulator i)))))
    @accumulator))

(defn day-4b [data]
  (->> data
   (map day-4-grammar)
   (map day-4-intersection)
   (apply-winners)
   (apply +)))

;;; Day 5 ;;;

(def day-5-grammar
  (insta/parser
   "<almanac> = seeds (<'\n\n'> mapping)*
    seeds = <'seeds:'> (<' '> number)+
    mapping = #'[a-z]+' <'-to-'> #'[a-z]+' <' map:'> (<'\n'> entry)*
    <entry> = number <' '> number <' '> number
    <number> = #'[0-9]+'"))

(def day-5-transforms
  {:seeds (fn [& seeds] (map #(Long/parseLong %) seeds))
   :mapping (fn [from to & entries]
               (let [entries (map  #(Long/parseLong %) entries)]
                 [from to (partition 3 entries)]))})

(defn build-imap [entries]
  (reduce (fn [m [dst src rng]]
            (imap/mark m src (+ src rng) [dst src rng]))
          imap/empty
          entries))

(defn build-maps [mappings]
  {:next-map (into {} (map #(vec (take 2 %)) mappings))
   :imaps (zipmap (map first mappings)
                  (map #(build-imap  (nth % 2)) mappings))})

(defn mapped-item [n [dst src _]]
  (+ dst (- n src)))

(defn look-up [{:keys [next-map imaps] :as m} map-name items]
   (if map-name
     (let [new-items (map #(if-let [i (get (imaps map-name) %)]
                             (mapped-item % (first i))
                             %)
                          items)]
       (look-up m (next-map map-name) new-items))
     items))

(defn day-5a [data]
  (->
   (day-5-grammar data)
   (->> (insta/transform day-5-transforms))
   (as-> [seeds & mappings]
     (let [maps (build-maps mappings)]
       (look-up maps "seed" seeds)))
   (->> (apply min))))

(defn range-splitter [[start end]]
  (fn xf [rf]
    (let [base (atom [start end])]
      (fn step
        ([] (rf))
        ([acc] (if @base
                    (rf acc @base)
                    (rf acc)))
        ([acc input]
         (let [[a b] @base [x y] input]
           (cond
             (and (<  a x)(<  y b)) (do (reset! base [y b]) (-> acc (rf [a x]) (rf [x y])))
             (and (<= x a)(<= b y)) (do (reset! base   nil) (-> acc (rf [a b])))
             (and (<  a x)(<= b y)) (do (reset! base   nil) (-> acc (rf [a x]) (rf [x b])))
             (and (<= x a)(<  y b)) (do (reset! base [y b]) (-> acc (rf [a y]))))))))))

(defn range-sort [[a b][c d]]
  (cond
    (<= a b c d) -1
    (<= c d a b) 1
    :else (throw (ex-info "Unsorted ranges" {:a a :b b :c c :d d}))))

(defn range-look-up [{:keys [next-map imaps] :as m} map-name item-ranges]
  (if-let [cur-map (imaps map-name)]
    (let [ ranges (apply concat
                         (for [[a b] item-ranges
                               :let [ map-ranges (subseq cur-map >= a <= (dec b)) ; WORKAROUND: < b doesn't work
                                     map-ranges (map first map-ranges)]]
                           (transduce (range-splitter [a b]) conj [] map-ranges)))
          sorted-ranges (sort range-sort ranges)
          new-item-ranges (map (fn [[a b]]
                                 (let [ map-ranges (subseq cur-map >= a <= (dec b)) ; WORKAROUND: < b doesn't work
                                       _ (assert (<= (count map-ranges) 1))
                                       [[x _y] q :as found] (first map-ranges)
                                       [X _ _] (first q)]
                                   (if found
                                     [(+ X (- a x))
                                      (+ X (- b x))]
                                     [a b])))
                               sorted-ranges)]
      (range-look-up m (next-map map-name) new-item-ranges))
    item-ranges))

(defn day-5b [data]
  (->
   (day-5-grammar data)
   (->> (insta/transform day-5-transforms))
   (as-> [seeds & mappings]
         (let [maps (build-maps mappings)
               seeds (map (fn [[a b]] [a (+ a b)])
                          (partition 2 seeds))]
           (range-look-up maps "seed" seeds)))
   (->> (sort-by first))
   ffirst))

;;; Day 6 ;;;

(defn day-6a [[time-line dist-line]]
  (let [parse-fn (fn [line]
                   (let [[_label & nums] (re-seq #"\w+" line)]
                     (map #(Long/parseLong %) nums)))
        times (parse-fn time-line)
        dists (parse-fn dist-line)
        margins (for [[t d] (map vector times dists)
                      :let [trials (map #(* % (- t %)) (range (inc t)))]]
                  (count (filter #(< d %) trials)))]
    (apply * margins)))

(defn day-6b [[time-line dist-line]]
  (let [parse-fn (fn [line]
                   (let [[_label & nums] (re-seq #"\w+" line)]
                     (Long/parseLong (apply str nums))))
        time (parse-fn time-line)
        dist (parse-fn dist-line)
        trials (map #(* % (- time %)) (range (inc time)))]
      (- (inc time)
         (* 2 (count (take-while #(<= % dist) trials))))))

(comment
  (require 'clojure.test)
  (require 'aoc-2023.core :reload)
  (require 'aoc-2023.core-test :reload)
  (time (clojure.test/run-tests 'aoc-2023.core-test))
  ;; Keep kondo happy
  [day-1a day-1b day-2a day-2b day-3a day-3b day-4a day-4b day-5a day-5b day-6a day-6b])