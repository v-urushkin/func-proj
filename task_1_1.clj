;; Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;; состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;; 1.1. Решите задачу с помощью элементарных операций над последовательностями и рекурсии

(defn _comb_str
  "Последовательно конкатинируем строку с последовательностью симовлов с условием,
   что последний символ строки не равен символу из последовательности.  
   ### Args
   *string*: строка;  
   *sym_seq*: последовательность символов.
  "
  [string sym_seq]
  (if (> (count sym_seq) 0)
    (if (= (str (last string)) (first sym_seq))
      (_comb_str string (rest sym_seq))
      (concat (list (str string (first sym_seq)))
              (_comb_str string (rest sym_seq))))
    nil ;; like no-op
    )
  )

(_comb_str "a" ["a", "b", "c"])
;; => ("ab" "ac")

(defn _comb_seq 
  "Попарно (каждую с каждым) конкатинируем последовательность строк,
   и последовательность символов с условием, что не будет повторяющихся символов.
   ### Args
   str_seq: последовательность строк;  
   sym_seq: последовательность символов.
  "
  [str_seq sym_seq]
  (if (> (count str_seq) 0)
    (concat (_comb_str (first str_seq) sym_seq)
            (_comb_seq (rest str_seq) sym_seq))
    nil
    )
  )

(_comb_seq ["ab" "ac" "ba" "bc" "ca" "cb"] ["a" "b" "c"])
;; => ("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")

(defn combine_sym
  "Функция возвращает список всех строк длины n, состоящих из этих символов 
   и не содержащих двух одинаковых символов, идущих подряд.
   ### Args
   *sym_seq*: последовательность сиволов;  
   *n*: число симоволов в комбинации"
  [sym_seq n]
  (if (> n 1)
    (_comb_seq (combine_sym sym_seq (dec n)) sym_seq)
    sym_seq
    )
  )

(combine_sym ["a" "b" "c"] 2)
;; => ("ab" "ac" "ba" "bc" "ca" "cb")
