#lang racket
; завдання 1
(display "\n --------------------------------------- \n Завдання 1 \n --------------------------------------- \n")
; повертає n число Фібоначчі
(define (get_fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (get_fib (- n 1))
                (get_fib (- n 2)))
         )
  )
)

; повертає список розміром n чисел Фібоначчі
(define (build_fib_list n)
  (build-list n (lambda (x) (get_fib x)))
)

; перевіряє чи задане число є простим чи ні
(define (is_simple num)
  (is_simple_iter num 2)
)

(define (is_simple_iter num current)
  (cond ((or (= num 0) (= num 1)) #f)
        ((and (< current num) (= (modulo num current) 0)) #f)
        ((and (< current num) (not (= (modulo num current) 0))) (is_simple_iter num (+ current 1)))
        (else #t)
  )
)

; повертає список простих чисел із заданого списку
(define (get_simple lst)
  (filter (lambda (x) (is_simple x)) lst)
)

(display "Кількість чисел Фібоначчі: ")
(define number (read))

(display "Список чисел Фібоначчі: ")
(define lisf (build_fib_list number))
(display lisf)
(newline)
(display "Номер елемента: ")
(define num_el (read))
(list-ref lisf num_el)
(display "Сума елементів списку чисел Фібоначчі: ")
(foldl + 0 lisf)
(display "елемент n+2 чисел Фібоначчі: ")
(- (get_fib (+ number 1)) 1)
(display "Список простих чисел Фібоначчі: ")
(get_simple lisf)
; завдання 2
(display "\n --------------------------------------- \n Завдання 2 \n --------------------------------------- \n")
(define backlog (reverse(cdr lisf))) ;використовується реверсований список чисел Фібоначчі з першого завдання
(display "Початковий Беклог ")
(display backlog)
(define nspr ; кількість спринтів по 2 завдання
(if (even? (length backlog))  
 (/(length backlog) 2)
      (+(quotient (length backlog) 2) 1)))
(define (scrum blog n) ; процедура спринтів 
  (cond ((> n nspr)
        (display "\n Беклог вичерпано"))
        ((= (length blog)1)
        (begin (display "\n Спринт № ")
               (display n)
               (display "\n Бали = ")
               (display blog)))
        (else (begin (display "\n Спринт № ")
               (display n)
               (display "\n Бали = ")
               (display (+ (car blog) (list-ref blog 1)))
               (display "\n =================================== \n Беклог ")
               (display (list-tail blog 2))
               (scrum (list-tail blog 2) (+ n 1))))))
(scrum backlog 1)