# Advent of Code 2024
# Day 02
# https://adventofcode.com/2024/day/2

(use ../util)

(def real-input (slurp "input.txt"))

(def test-input ```
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9```)

# Part 1

## Use this variable to switch between test and real data
(def active-input real-input)

(def peg ~{:main (split "\n" :report)
           :report (group (split " " :number))
           :number (number :d+)
           :whitespaces (some " ")})

(def reports (peg/match peg active-input))

(defn diff-list [report]
  (if (< (length report) 2) @[]
    (seq [i :range [1 (length report)]] (- (report i) (report (- i 1))))))

(defn is-safe? [report]
  (let [diffs (diff-list report)]
    (and (or (all |(< $ 0) diffs) (all |(> $ 0) diffs))
         (all |(and (>= (math/abs $) 1) (<= (math/abs $) 3)) diffs))))

(print "Part 1")
(print (count |(is-safe? $) reports)) # Expected: 390  || Test: 2 

# Part 2

(defn removed-at [arr n]
  "Return new array with the nth element removed."
  (array/remove (array/slice arr) n))

(defn damp-report [report]
  "Retrun a list of reports with the nth element removed."
  (seq [i :range [0 (length report)]] (removed-at report i)))


(defn is-damp-safe? [report]
  (let [diffs (diff-list report)]
    (or (is-safe? report) (any? (map |(is-safe? $) (damp-report report))))))


(print "\n")
(print "Part 2")
(print (count |(is-damp-safe? $) reports)) # Expected: 439  || Test: 4

