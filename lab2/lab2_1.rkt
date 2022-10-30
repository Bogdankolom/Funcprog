#lang racket

(define (iter i) ; ітерації значень [-2, 2] для функції Scheme
  (cond
    ((and (>= i -2) (<= i 2))
      (display i)
      (display " = ")
      (display (test_sqrt i))
      (newline)
      (iter (+ i 0.5))
    )
    (else 
      (display "")
    )
  )
)


(define (test_sqrt x) ;функція що рахує вбудованою функцією Scheme
  (cond
    ((and (< -2 x) (<= 0))
     (+ (sqrt (- x 1)) (sqrt (+ x 1)))
      )
    ((and (< 0 x) (< 2))
     (/ 1 (sqrt (- (* x x) 1)))     
     )
    (else "Incorrect value")
    )
)


(define (iter_Talior i) ;Фунція що проходить ітерації значень [-2, 2] для ряду Тейлора 
   (cond
    ((and (>= i -2) (<= i 2))
      (display i)
      (display " = ")
      (display (Tailor_series i)) 
      (newline)
      (iter (+ i 0.5))
    )
    (else 
      (display "")
    )
  )
)



(define (Tailor_series x) ;розрахунок функції з корнем квадратним по Ряду тейлора
  (cond
     ((and (< -2 x) (<= 0))
     (+ (sqrt_t (- x 1) ) (sqrt_t (+ x 1) ))
         )
    ((and (< 0 x) (< 2))
     (/ 1 (sqrt_t ((- (* x x) 1) )))     
     ) 
    (else "Incorrect value")
    )
 )

(define (sqrt_t x) ; розрахунок кореня за рядом Тейлора
  (define (sqrt_iter yn n)
    (cond ((= n 10) yn) ; точність обчисления
          ((<= x 0) 0) ; якщо x <= 0, повернути 0
          (else
           (sqrt_iter (/(+ yn (/ x yn)) 2) (+ n 1))))) ; розрахунок наступного наближення
  
  (sqrt_iter 1 0))

  


(display "Вбудована функція")
(newline)
(iter -2)

(newline)
(newline)

(display "за розкладом Tailor:")
(newline)
(iter_Talior -2)