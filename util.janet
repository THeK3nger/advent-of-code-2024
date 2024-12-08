# This macro comes from the tutorial I am following at
# https://www.youtube.com/watch?v=yLsLIofgIs8
(defmacro fold-loop [init f dsl & body]
  (with-syms [$result $f]
    ~(let [,$f ,f]
       (var ,$result ,init)
       (loop ,dsl
         (set ,$result (,$f ,$result (do ,;body))))
       ,$result)))

(defn zip [a b]
  (map (fn [x y] [x y]) a b))

(defn sum-map [f coll]
  "A very stupid function that sums the results of applying f to each element of an iterable."
  (sum (map f coll)))

## ARRAY UTILS

# I am not sure if there is already a function like this in Janet
(defn contain? [arr elem]
  "Check if an element is in an array."
  (some (fn [x] (= x elem)) arr))

## GRID UTILS

(defn in-grid? [grid row col]
  (and (>= row 0) (< row (length grid))
       (>= col 0) (< col (length (grid 0)))))

(defn get-cell [grid row col]
  (if (in-grid? grid row col)
    ((grid row) col)
    nil))

(defn set-cell [grid row col value]
  (if (in-grid? grid row col)
    (set ((grid row) col) value)))

(defn neighbors [grid row col]
  (seq [r :range-to [(dec row) (inc row)]
        c :range-to [(dec col) (inc col)]
        :unless (not (in-grid? grid r c))] [r c]))

(defn next-cell [grid row col direction]
  (def nc (case direction
            :n [(dec row) col]
            :s [(inc row) col]
            :e [row (inc col)]
            :w [row (dec col)]
            :ne [(dec row) (inc col)]
            :nw [(dec row) (dec col)]
            :se [(inc row) (inc col)]
            :sw [(inc row) (dec col)]))
  (if (in-grid? grid (nc 0) (nc 1))
    nc
    nil))

(defn range-positions [grid]
  (seq [r :range-to [0 (length grid)]
        c :range-to [0 (length (grid 0))]] [r c]))

(defn grid-print [grid]
  (each row grid
    (apply print row)))
