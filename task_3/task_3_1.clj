;; Реализовать функцию (оператор), принимающую аргументом функцию от одной переменной f и
;; возвращающую функцию одной переменной, вычисляющую (численно) выражение:
;; ...
;; Можно использовать метод трапеций с постоянным шагом.
;; При оптимизации исходить из того, что полученная первообразная будет использоваться для
;; построения графика (т.е. вызываться многократно в разных точках)
;; 3.1. Оптимизируйте функцию с помощью мемоизации
(ns task-3.task-3-1 (:use clojure.test))

(defn trapezoidal-step
  ([func x0] (trapezoidal-step func x0 0.01))
  ([func x0 h]
   (* (* (+ (func x0) (func (+ x0 h))) h) 0.5)
   ))

(defn line [x] x)
(defn square [x] (* x x))
(defn f5x [x] (* x 5))

(defn integrate-it 
  ([func x] (integrate-it func 0 x (/ x 10000) 0))
  ([func x n] (integrate-it func 0 x (/ x n) 0))
  ([func x0 x h acc]
   (if (< x0 x) 
     (recur func (+ x0 h) x h (+ acc (trapezoidal-step func x0 h)))
     acc)
   ))

(integrate-it line 10)
;; => 50.0
(integrate-it square 2)
;; => 2.6666666800000067


(def integrate-it-mem-v1 (memoize integrate-it))

(def trapezoidal-step-mem (memoize trapezoidal-step))
(def STEP 0.001)
(defn integrate-it-mem-v2
  ([func x] (integrate-it func 0 x STEP 0))
  ([func x0 x h acc]
   (if (< x0 x)
     (recur func (+ x0 h) x h (+ acc (trapezoidal-step-mem func x0 h)))
     acc)))

(defn -main []
  (time (integrate-it square 50))
  ;; "Elapsed time: 33.458193 msecs"
  (time (integrate-it-mem-v1 square 70))
  ;; "Elapsed time: 33.294403 msecs"
  (time (integrate-it-mem-v1 square 70))
  ;; "Elapsed time: 1.011789 msecs"
  (time (integrate-it-mem-v2 square 70))
  ;; "Elapsed time: 32.953003 msecs"
  (time (integrate-it-mem-v2 square 30))
  ;; "Elapsed time: 7.057069 msecs"
  (time (integrate-it-mem-v2 f5x 100))
  ;; "Elapsed time: 37.195136 msecs"
  (time (integrate-it-mem-v2 f5x 50))
  ;; "Elapsed time: 11.562308 msecs"
  (time (integrate-it square 30))
  ;; "Elapsed time: 43.246127 msecs"
  (time (integrate-it f5x 50))
  ;; "Elapsed time: 29.390655 msecs"
  (shutdown-agents))

; тестирование
(defn closer [x0 x1] ( < (abs (- x0 x1)) 0.001))

(deftest functions-test
  (is (closer (integrate-it-mem-v2 square 1) 0.3333))
  (is (closer (integrate-it-mem-v2 square 2) 2.67))
)
(run-tests 'task-3.task-3-1)
;; {:test 1, :pass 2, :fail 0, :error 0, :type :summary}
