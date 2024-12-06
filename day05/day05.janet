# Advent of Code 2024
# Day 05
# https://adventofcode.com/2024/day/5

(use ../util)

(def real-input (slurp "input.txt"))

(def test-input ```
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47```)

# Part 1

## Use this variable to switch between test and real data
(def active-input real-input)

(def [rules updates] (string/split "\n\n" active-input))

## This is the PEG for the first part of the problem. It just matches the multiplication codes.
(def peg-rules ~{:main (split "\n" :rule)
                 :rule (group (* (constant :from) :page "|" (constant :to) :page))
                 :page (number :d+)})

(def peg-updates ~{:main (split "\n" :update)
                   :update (group (split "," :page))
                   :page (number :d+)})

## Let's parse the rules and updates
(def rules (map |(table ;$) (peg/match peg-rules rules)))
(def updates (peg/match peg-updates updates))

(defn middle-page [page-update]
  ``Return the middle page of a page update.``
  (let [middle (math/floor (/ (length page-update) 2))]
    (page-update middle)))

(defn comes-before? [a b rules]
  ``Return true if page a comes before page b in the rules.``
  # This let generates a list of pages that a should come before b according to the rules.
  (let [should-come-after-a-rules (map |($ :to) (filter |(= ($ :from) a) rules))]
    (contain? should-come-after-a-rules b)))

(defn page-is-correct-order? [page rest rules]
  ``A page is in the correct order if it comes before all the other pages in the list.``
  (if (empty? rest)
    true
    (all |(comes-before? page $ rules) rest)))

(defn is-valid-update? [page-update rules]
  ``An update is valid if all the pages are in the correct order.``
  (if (empty? page-update) true
    (let [[p & rest] page-update]
      (and (page-is-correct-order? p rest rules) (is-valid-update? rest rules)))))

## Compute the Solution Part 1
(def valid-updates (filter |(is-valid-update? $ rules) updates))
(def solution1 (sum-map |(middle-page $) valid-updates))

(print "Part 1")
(print "Result: " solution1) # Expect 5452 (TEST: 143)

# Part 2

(def invalid-updates (filter |(not (is-valid-update? $ rules)) updates))

(defn reorder-update [page-update rules]
  (sorted page-update (fn [a b] (comes-before? a b rules))))

## Compute the Solution Part 2
(def reordered-updates (map |(reorder-update $ rules) invalid-updates))
(def solution2 (sum-map |(middle-page $) reordered-updates))

(print "\n")
(print "Part 2")
(print "Result: " solution2) # Expect 4598 (TEST: 123)

