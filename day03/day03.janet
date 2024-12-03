# Advent of Code 2024
# Day 03
# https://adventofcode.com/2024/day/3

(use ../util)

(def real-input (slurp "input.txt"))

(def test-input ```
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))```)
(def test-input2 ```
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))```)

# Part 1

## Use this variable to switch between test and real data
(def active-input real-input)

## This is the PEG for the first part of the problem. It just matches the multiplication codes.
(def peg ~{:main (split "\n" :code)
           :code (group (any (+ (group :mul) 1)))
           :mul (* "mul" "(" :number "," :number ")")
           :number (number :d+)})

(def codes (peg/match peg active-input))

## A sequence of unnecessary verbose functions to calculate the result.
(defn mul-list [lst] (sum-map |(* ($ 0) ($ 1)) lst))
(defn mul-codes [codes] (sum-map mul-list codes))

(print "Part 1")
(print "Test Input (Should be 161): " (mul-codes (peg/match peg test-input)))
(print "Result: " (mul-codes codes)) # Expect 170807108

# Part 2

## This is the PEG for the second part of the problem. It matches the multiplication codes and the do/don't codes.
## Note that, for simplicity, do/don't codes are mapped into "T" and "F" strings respectively.
## Therefore the result is a non-homogeneous list of strings and tuples:
## e.g., @[@[2,4], "T", @[3,7], @[4,5], "F", @[32,8]]
(def peg2 ~{:main (split "\n" :code)
            :code (group (any (+ (group :mul) :do :dont 1)))
            :mul (* "mul" "(" :number "," :number ")")
            :do (/ (* "do()") "T")
            :dont (/ (* "don't()") "F")
            :number (number :d+)})

(defn filter-code [code initial-state]
  ``Filter out the codes that are not active. 
  It returns a tuple with the filtered codes and the latest status of the enabled/disabled multiplication flag.
  
  - code (list[tuple|T|F]): The code to be filtered.
  - initial-state (bool): The initial state of the multiplication flag.``
  (var result @[])
  (var active initial-state)
  (for i 0 (length code)
    (let [v (code i)]
      (cond (= v "T") (set active true)
        (= v "F") (set active false)
        (true? active) (array/push result v))))
  [result active])

(defn filter-codes [codes]
  ``Filter out the codes that are not active.
  
  This function keeps track of the latest status of the enabled/disabled multiplication flag
  in between lines.``
  (var active true)
  (seq [i :range [0 (length codes)]]
    (let [[filtered new-state] (filter-code (codes i) active)]
      (set active new-state)
      filtered)))

(def new_codes (peg/match peg2 active-input))

(print "\n")
(print "Part 2")
(print "Test Input (Should be 48): " (mul-codes (filter-codes (peg/match peg2 test-input2))))
(print "Result: " (mul-codes (filter-codes new_codes))) # Expected 74838033

