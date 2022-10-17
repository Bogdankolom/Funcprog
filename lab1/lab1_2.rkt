#lang racket
;Коломійченко ІПЗ43 Лаб1 Варіант 8 (завдання 2)
(define (zifr num); процедура підрахунку суми цифр числа 
    (if (< num 10)
        num
        (+ (remainder num 10) (zifr (/ (- num (remainder num 10)) 10)))))
(display "Введіть k:") 
(define K (read))
(display "Введіть s:")
(define S (read))
(define i1 (expt 10 (- K 1)))
(define i2 (-(expt 10 K)1))
(define sum 0)
(for ([n1 (in-range i1 i2)]);підрахунок в циклі кількості чисел, сума цифр яких = S
(when (= S (zifr n1))
   (set! sum(+ sum 1))))
(display sum)