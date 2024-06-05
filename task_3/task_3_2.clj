;; Реализовать функцию (оператор), принимающую аргументом функцию от одной переменной f и
;; возвращающую функцию одной переменной, вычисляющую (численно) выражение:
;; ...
;; Можно использовать метод трапеций с постоянным шагом.
;; При оптимизации исходить из того, что полученная первообразная будет использоваться для
;; построения графика (т.е. вызываться многократно в разных точках)
;; 3.2. Оптимизируйте функцию с помощью бесконечной последовательности частичных решений

(ns task-3.task-3-2 (:use clojure.test))

(defn trapezoidal-step
  ([func x0] (trapezoidal-step func x0 0.01))
  ([func x0 h]
   (* (* (+ (func x0) (func (+ x0 h))) h) 0.5)))


(defn integrate-it
  ([func x] (integrate-it func 0 x (/ x 10000) 0))
  ([func x n] (integrate-it func 0 x (/ x n) 0))
  ([func x0 x h acc]
   (if (< x0 x)
     (recur func (+ x0 h) x h (+ acc (trapezoidal-step func x0 h)))
     acc)))

(def STEP 0.001)
(defn integrate-seq
  ([func]
   (iterate
    (fn [[sum a]]
      (list (+ sum (trapezoidal-step func a STEP)) (+ a STEP)))
    (list 0 0))))

(defn integrate-it-lazy
  ([func x]
   (let [idx (int (/ x STEP))]
     (first (nth (integrate-seq func) idx)))))

(defn square [x] (* x x))
(defn line [x] x)
(take 11 (integrate-seq square))
(integrate-it-lazy square 1)
;; => 0.33335000000000037

(defn -main []
  (time (integrate-it square 50))
  ;; "Elapsed time: 33.458193 msecs"
  (time (integrate-it-lazy square 1))
  ;; "Elapsed time: 3.425206 msecs"
  (time (integrate-it-lazy square 2))
  ;; "Elapsed time: 1.949 msecs"
  (time (integrate-it-lazy square 10))
  ;; "Elapsed time: 8.653976 msecs"
  (time (integrate-it-lazy square 5))
  ;; "Elapsed time: 4.217122 msecs"
  (time (integrate-it-lazy square 100))
  ;; "Elapsed time: 49.542781 msecs"
  (time (integrate-it-lazy square 50))
  ;; "Elapsed time: 24.290204 msecs"
  (time (integrate-it-lazy square 50))
  ;; "Elapsed time: 24.410488 msecs"
  (shutdown-agents))

; тестирование
(defn closer [x0 x1] ( < (abs (- x0 x1)) 0.001))

(deftest functions-test
  (is (closer (integrate-it-lazy square 1) 0.3333))
  (is (closer (integrate-it-lazy square 2) 2.6667))
)
(run-tests 'task-3.task-3-2)
;; {:test 1, :pass 2, :fail 0, :error 0, :type :summary}