(defn my_map [func coll]
  (reduce
   (fn [x y] (concat x [(func y)])) []
   coll))

(defn my_filter
  [pred coll]
  (reduce
   (fn [x y] (if (pred y) (concat x [y]) x)) []
   coll))


(map dec '(4 5 6))
;; => (3 4 5)
(my_map dec '(4 5 6))
;; => (3 4 5)
(filter (fn [x] (zero? (mod x 3))) (range 0 10))
;; => (0 3 6 9)
(my_filter (fn [x] (zero? (mod x 3))) (range 0 10))
;; => (0 3 6 9)