;; 2. Решето Эратосфена
;; 2.2. Реализуйте бесконечную последовательность простых чисел

(defn is-not-crossed-out
  [x p]
  (if (= x p)
    true
    (not= (mod x p) 0)))

(defn eratosthen
  [n]
  (if (< n 4)
    (range 2 (+ n 1))
    (loop [sieve (range 2 (+ n 1)) p 1]
      (let [p (first (filter (fn [x] (> x p)) sieve))]
        (if (> (* p p) n)
          sieve
          (recur (filter (fn [x] (is-not-crossed-out x p)) sieve) p))))))

(defn get-nth-prime
  [n]
  (if (< n 0)
    IndexOutOfBoundsException
    (loop [max-n (+ n 2)]
      (let [res (nth (eratosthen max-n) n false)]
        (if (= res false)
          (recur (* max-n 10))
          res)))))

(def naturals
  (lazy-seq (cons 0 (map inc naturals))))

(def primals 
  (map get-nth-prime naturals))

(take 3 primals)
;; => (2 3 5)
(take 10 primals)
;; => (2 3 5 7 11 13 17 19 23 29)
(take 15 primals)
;; => (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)