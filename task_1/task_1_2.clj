;; Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;; состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;; 1.1. Решите задачу с помощью элементарных операций над последовательностями и рекурсии
;; Перепишите программу 1.1. так, чтобы все рекурсивные вызовы были хвостовыми

;;факториал
(defn fact-1 [n]
(if (> n 0)
  (* n (fact-1 (dec n)))
  1))

(fact-1 4)

(defn fact-2
  ([n] (fact-2 n 1))
  ([n acc] (if (> n 0)
             (recur (dec n) (* n acc))
             acc)))

(defn _comb_str
  "Последовательно конкатинируем строку с последовательностью симовлов с условием,
   что последний символ строки не равен символу из последовательности.  
   ### Args
   *string*: строка;  
   *sym_seq*: последовательность символов;
   *acc*:
  "
  ([string sym_seq] (_comb_str string sym_seq (list))) 
  ([string sym_seq acc]
  (if (> (count sym_seq) 0)
    (if (= (str (last string)) (first sym_seq))
      (recur string (rest sym_seq) acc)
      (recur string (rest sym_seq)
      (concat (list (str string (first sym_seq)))
              acc)
       )
     )
    acc
    )
   )
  )

(_comb_str "a" ["a", "b", "c", "d"])
;; => ("ab" "ac")

(defn _comb_seq
  "Попарно (каждую с каждым) конкатинируем последовательность строк,
   и последовательность символов с условием, что не будет повторяющихся символов.
   ### Args
   *str_seq*: последовательность строк;  
   *sym_seq*: последовательность символов;
   *acc*:
  "
  ([str_seq sym_seq] (_comb_seq str_seq sym_seq (list)))
  ([str_seq sym_seq acc]
  (if (> (count str_seq) 0)
    (recur
     (rest str_seq)
     sym_seq
     (concat (_comb_str (first str_seq) sym_seq)
             acc)
     )
    acc
    )
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
  ([sym_seq n] (combine_sym sym_seq n sym_seq))
  ([sym_seq n acc]
   (if (> n 1)
     (recur sym_seq (dec n) (_comb_seq acc sym_seq))
     acc)
   )
  )

(combine_sym ["a" "b" "c"] 2)
