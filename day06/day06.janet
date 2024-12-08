# Advent of Code 2024
# Day 05
# https://adventofcode.com/2024/day/5

(use ../util)

(def real-input (slurp "input.txt"))

(def test-input ```
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...```)

# Part 1

## Use this variable to switch between test and real data
(def active-input real-input)

(def peg ~{:main (split "\n" :row)
           :row (group (some :cell))
           :cell (<- (+ "." "#" "^"))})

(def grid (peg/match peg active-input))

(defn find-start-position [grid]
  (let [row (find-index (fn [row] (contain? row "^")) grid)
        col (find-index (fn [cell] (= cell "^")) (grid row))]
    [row col]))

(defn is-obstacle? [grid row col]
  (if (in-grid? grid row col)
    (= (get-cell grid row col) "#")
    false))

(defn direction-symbol [direction]
  (case direction
    :n "^"
    :e ">"
    :s "v"
    :w "<"))


(defn is-visited? [grid row col direction]
  ``Check if a cell has been visited. 
  Note that, for this problem purposes, a cell is considered visited if we reache it
  again IN THE SAME DIRECTION``
  (if (in-grid? grid row col)
    (= (get-cell grid row col) (direction-symbol direction))
    false))

(defn turn [direction]
  (case direction
    :n :e
    :e :s
    :s :w
    :w :n))

(defn is-visited-cell [sym]
  (some (fn [x] (= x sym)) ["^" ">" "v" "<"]))

# I am pretty sure this function can be simplified a lot. But whatever. 
(defn navigate [grid]
  (var [r c] (find-start-position grid))
  (var direction :n)
  (set-cell grid r c "^")
  (var is-loop false)
  (while (in-grid? grid r c)
    (let [next-cell (next-cell grid r c direction)]
      (if (nil? next-cell)
        (do (set is-loop false) (break))
        (if (is-obstacle? grid (next-cell 0) (next-cell 1))
          (set direction (turn direction))
          (if (is-visited? grid (next-cell 0) (next-cell 1) direction)
            (do (set is-loop true) (break))
            (do
              (set r (next-cell 0))
              (set c (next-cell 1))
              (set-cell grid r c (direction-symbol direction))))))))
  is-loop)

(defn count-visited [grid]
  (sum-map (fn [row] (count (fn [cell] (is-visited-cell cell)) row)) grid))

(print "Part 1")
(print "Result: " (count-visited grid)) # Expect 4973 (TEST: 41)

## Part 2

(defn get-visited-positions [grid]
  (seq [r :range-to [0 (length grid)]
        c :range-to [0 (length (grid 0))]
        :when (is-visited-cell (get-cell grid r c))] [r c]))

# I am computing all the visited positions of the "Part 1" path, becase 
# if a single new obstacle must create a loop, it must be placed in one of thoos positions. 
(def visited-positions (get-visited-positions grid))

# Then I reset the grid. 
(def grid (peg/match peg active-input))

(defn solve2 []
  (def all-positions visited-positions)
  (var result 0)
  (each [r c] all-positions
    (do
      #Reset the grid
      (def grid (peg/match peg active-input))
      (when (and (not (is-obstacle? grid r c)) (not (is-visited? grid r c :n)))
        (do
          (prinf "Setting obstacle in %d %d\r" r c)
          (set-cell grid r c "#")
          (if (navigate grid) (set result (inc result)))))))
  result)

(print "\n")
(print "Part 2")
(print "Result: " (solve2) "                        ") # Expect 1482 (TEST: 6) 
# The extra spaces are to overwrite the "Setting obstacle in %d %d" message...

