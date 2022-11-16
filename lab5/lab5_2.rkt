#lang racket
(define (real x) (car x)); дійсна та уявна частина
(define (image x) (cdr x))	


(define (print-compl x)	; друк координат зображення комплексного числа в декартовій системі координат
 (display " (")
  (display (real x))
 (display " ; ")
   (display (image x))
           (display ")") )
 


(define (make-from-mag-ang r arg); побудова комплексного числа в алгебраїчній формі за радіусом та аргументом
  (cons (* r (cos arg)) (* r (sin arg))))


(define (mag-ang r a );відомо модуль та аргумент 
     (display "\n 1) відомо модуль=")
    (display r)
  (display " та аргумент=")
   (display a)
    (cons (make-from-mag-ang r a) (make-from-mag-ang (* -1 r) a) )
    (display " \n ГМТ - дві точки з координатами \n")
    (print-compl  ( make-from-mag-ang r a  ) )
    (display "  та ")
    (print-compl  ( make-from-mag-ang (* -1 ) a  ) )
    
    )

(define (ang a); відомо аргумент 
    (display "\n 2) відомо аргумент=")
    (display a)
    (display "\n ГМТ - пряма що проходить через точки \n")
     (print-compl  ( make-from-mag-ang 1 a  ) )
    (display "  та ")
      (print-compl  ( make-from-mag-ang 0 a  ) )
    )

(define (modul m ); відомо модуль 
    (display "\n 3) відомо модуль=")
    (display m)
    (display "\n ГМТ - коло з центром в точці (0 ; 0) та радіусом ")
     (display m)
    
    )

(display "Три варіанти ГМТ, що зображують комплексне число, задане геометрично, в декартовій системі координат:")
( mag-ang 2 0.6  )

(ang 0.6)

(modul 2 )