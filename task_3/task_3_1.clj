;; Реализовать функцию (оператор), принимающую аргументом функцию от одной переменной f и
;; возвращающую функцию одной переменной, вычисляющую (численно) выражение:
;; ...
;; Можно использовать метод трапеций с постоянным шагом.
;; При оптимизации исходить из того, что полученная первообразная будет использоваться для
;; построения графика (т.е. вызываться многократно в разных точках)
;; 3.1. Оптимизируйте функцию с помощью мемоизации
(ns task-3.task-3-1 (:use clojure.test))

(def STEP 0.01)

(defn trapezoidal-step
  ([func x0 h]
   (* (* (+ (func x0) (func (+ x0 h))) h) 0.5)))

(defn line [x] x)
(defn square [x] (* x x))
(defn f5x [x] (* x 5))

(defn integrate-it 
  ([func x] (integrate-it func 0 x STEP 0))
  ([func x n] (integrate-it func 0 x (/ x n) 0))
  ([func x0 x h acc]
   (if (< x0 x) 
     (recur func (+ x0 h) x h (+ acc (trapezoidal-step func x0 h)))
     acc)))

(integrate-it line 10)
;; => 50.0
(integrate-it square 2)
;; => 2.6666666800000067

(defn nth-trapezoidal-v3
  [func nth-mem idx]
   (if (> idx 0)
     (+ (nth-mem func nth-mem (dec idx)) 
        (trapezoidal-step func (* (dec idx) STEP) STEP))
     0))

(def nth-trapezoidal-mem (memoize nth-trapezoidal-v3))

(defn get-antiderivative-by-idx [func]
  (fn [idx] (nth-trapezoidal-mem func nth-trapezoidal-mem idx)))

(defn get-antiderivative-mem
  ([func]
   (let [partial-results (memoize (get-antiderivative-by-idx func))]
     (fn [x]
       (let [idx (int (/ x STEP))]
         (partial-results idx))))))
         
(defn integrate-it-mem [func x]
  ((get-antiderivative-mem func) x))

(defn -main []
  (time (integrate-it line 4))
  ;; "Elapsed time: 2.122419 msecs"
  (time (integrate-it-mem line 2))
  ;; "Elapsed time: 2.553924 msecs"
  (time (integrate-it-mem  line 4))
  ;; "Elapsed time: 1.850245 msecs"
  (shutdown-agents))

; тестирование
(defn closer [x0 x1] ( < (abs (- x0 x1)) 0.1))

(deftest functions-test
  (is (closer (integrate-it-mem square 1) 0.3))
  (is (closer (integrate-it-mem square 2) 2.7))
)
(run-tests 'task-3.task-3-1)
;; {:test 1, :pass 2, :fail 0, :error 0, :type :summary}
