;; 2. Решето Эратосфена
;; 2.1. Напишите функцию, которая ищет n-ое простое число с помощью решета Эратосфена

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
           

(eratosthen 5)
;; => (2 3 5)
(eratosthen 20)
;; => (2 3 5 7 11 13 17 19)
(eratosthen 30)
;; => (2 3 5 7 11 13 17 19 23 29)

(defn get-nth-prime
  [n]
  (if (< n 0)
    IndexOutOfBoundsException
    (loop [max-n (+ n 2)]
      (let [res (nth (eratosthen max-n) n false)]
        (if (= res false)
          (recur (* max-n 10))
          res)))))

(get-nth-prime -1)
;; => java.lang.IndexOutOfBoundsException
(get-nth-prime 0)
;; => 2
(get-nth-prime 1)
;; => 3
(get-nth-prime 2)
;; => 5
(get-nth-prime 3)
;; => 7
(get-nth-prime 4)
;; => 11
(get-nth-prime 100)
;; => 547

