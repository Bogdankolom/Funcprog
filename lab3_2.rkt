#lang racket
;======================================
; розрахунок інтеграла з використанням
;процедур як параметрів та Lambda форми
;=====================================
;шаблон процедури для суми значень
(define (sum-integral term a next b)
  (if (> a b)
    0
  (+ (term a)
  (sum-integral term (next a) next b))))
; процедура розрахунку інтеграла методом трапецій
(define (integral f a b dx)
  (* (+(/(+ (f a) (f b)) 2)(sum-integral f (+ a dx)
          (lambda (x) (+ x dx))
          (- b dx)))
     dx))
; процедура розрахунку інтеграла методом лівих прямокутників
(define (integral_lp f a b dx)
  (* (sum-integral f a
          (lambda (x) (+ x dx))
          (- b dx))
     dx))
;функція
(define (func x)
     ( cos (- x (sin x))))

(display "інтеграл по методу трапецій від 0 до pi =")
(integral  func  0  3.14  0.001);call procedure
(display "інтеграл по методу лівих прямокутників від 0 до pi =")
(integral_lp  func  0 3.14  0.001);call procedure
