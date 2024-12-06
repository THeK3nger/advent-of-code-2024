# Advent of Code 2024
# Day 04
# https://adventofcode.com/2024/day/4

(use ../util)

(def real-input (slurp "input.txt"))

(def test-input ```
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX```)


# Part 1

## Use this variable to switch between test and real data
(def active-input real-input)

(def peg ~{:main (split "\n" :row)
           :row (group (some :cell))
           :cell (<- (+ "X" "M" "A" "S"))})

(def wordgrid (peg/match peg active-input))

#(pp wordgrid)

(defn next-letter [letter]
  (case letter
    "X" "M"
    "M" "A"
    "A" "S"
    "S" nil))

(defn count-xmas-at-pos-rec [grid row col letter direction]
  (if (in-grid? grid row col)
    (let [cell (get-cell grid row col)
          new-char (next-letter letter)
          new-cell (next-cell grid row col direction)]
      (if (= cell letter)
        (if (nil? new-char)
          1
          (if (and (not (nil? new-cell)) (in-grid? grid (new-cell 0) (new-cell 1)))
            (count-xmas-at-pos-rec grid (new-cell 0) (new-cell 1) new-char direction)
            0))
        0))
    0))

(defn count-xmas-at-pos [grid row col]
  (if (in-grid? grid row col)
    (sum (map |(count-xmas-at-pos-rec grid row col "X" $) [:n :s :e :w :ne :nw :se :sw]))
    0))

(defn count-xmas [grid]
  (sum (map (fn [pos] (count-xmas-at-pos grid (pos 0) (pos 1))) (range-positions grid))))

(print "Part 1")
(print "Test Input (Should be 18): " (count-xmas (peg/match peg test-input)))
(print "Result: " (count-xmas wordgrid)) # Expect 2633

# Part 2

(defn not-on-border [grid row col]
  (and (not= row 0) (not= col 0) (not= row (dec (length grid)) (not= col (dec (length (grid 0)))))))

(defn diagonal-x-mas [grid row col diagonal]
  (assert (not-on-border grid row col))
  (assert (in-grid? grid row col))
  (if (= diagonal :major)
    (let [cell (get-cell grid row col)
          sw-cell (get-cell grid (inc row) (dec col))
          ne-cell (get-cell grid (dec row) (inc col))]
      (and (= cell "A") (or (and (= sw-cell "M") (= ne-cell "S")) (and (= sw-cell "S") (= ne-cell "M")))))
    (let [cell (get-cell grid row col)
          se-cell (get-cell grid (inc row) (inc col))
          nw-cell (get-cell grid (dec row) (dec col))]
      (and (= cell "A") (or (and (= se-cell "M") (= nw-cell "S")) (and (= se-cell "S") (= nw-cell "M")))))))

(defn is-x-mas [grid row col]
  (if (in-grid? grid row col)
    (if (not-on-border grid row col)
      (let [cell (get-cell grid row col)]
        (and (diagonal-x-mas grid row col :major) (diagonal-x-mas grid row col :minor)))
      false)
    false))

(defn count-x-mas [grid]
  (sum (map (fn [pos] (if (is-x-mas grid (pos 0) (pos 1)) 1 0)) (range-positions grid))))

(print "\n")
(print "Part 2")
(print "Test Input (Should be 9): " (count-x-mas (peg/match peg test-input)))
(print "Result: " (count-x-mas wordgrid))
