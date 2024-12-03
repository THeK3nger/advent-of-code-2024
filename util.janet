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
