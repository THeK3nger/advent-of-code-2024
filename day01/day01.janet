# Advent of Code 2024
# Day 01
# https://adventofcode.com/2024/day/1

(use ../util)

(def real-input (slurp "input.txt"))

(def test-input ```
3   4
4   3
2   5
1   3
3   9
3   3```)

# Part 1

## Use this variable to switch between test and real data
(def active-input real-input)

(def peg ~{:main (split "\n" :pair)
           :pair (group (* :number :whitespaces :number))
           :number (number :d+)
           :whitespaces (some " ")})

(defn unzip [pairs]
  (let [first-column (map |(get $ 0) pairs)
        second-column (map |(get $ 1) pairs)]
    {:first first-column
     :second second-column}))

(def parsed-input (unzip (peg/match peg active-input)))

(defn sort-and-diff [input]
  "Sort the two columns and compute the absolute difference between the elements."
  (let [first-column (input :first)
        second-column (input :second)
        sorted-first (sort first-column)
        sorted-second (sort second-column)
        zipped (zip sorted-first sorted-second)
        diffs (map |(math/abs (- (get $ 0) (get $ 1))) zipped)]
    diffs))

(print "Part 1")
(pp (sum (sort-and-diff parsed-input))) # Expected: 1873376  || Test: 11 

# Part 2

(defn similarity
  "Compute the similarity score for n according to the AoC Day One rules. 
  That is: count the number of times n appears in the second column and
  multiply it by n."
  [input n]
  (let [second-column (input :second)
        c (count |(= $ n) second-column)]
    (* n c)))

(defn sum-similarities [input]
  "Sum the similarity score for all elements in the first column."
  (sum (map |(similarity input $) (input :first))))

(print "\nPart 2")
(pp (sum-similarities parsed-input)) # Expected 18997088 || Test: 31

