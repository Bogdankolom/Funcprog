#lang racket
;==========================================
;lab3 var 8
;===========================================
; значення точності
(define eps 0.001)
; похідна
(define (deriv)
  (lambda (x)
         (+ 2 (* 2 (sin x)))))
; друга похідна
(define (deriv-2)
  (lambda (x)
    (* 2 (cos x))))

; пошук кореня рівняння f (x) =0 методом половинного ділення
;процедура пошуку серединної точки, що претендує на значення кореня
; f  - функція, корінь якої погтрібно визначити,
; neg-point, pos-point - діапазони, в межах яких знаходиться корінь рівняння 
(define (search f neg-point pos-point)
     (let ((midpoint (average neg-point pos-point))) ; midpoint - серединна точка діапазону
          (if (close-enough? neg-point pos-point)    ;якщо точність досягнуто 
              midpoint                           ; тоді корень рівняння в точці midpoint
              (let ((test-value (f midpoint)))   ; інакше визначити діапазон для пошуку нового претендента на корінь
                (cond ((positive? test-value)   ; якщо значення функції в точці  midpoint додатнє
                           (search f neg-point midpoint)) ;шукати корінь на відрізку зліва від серединної точки
                          ((negative? test-value)          ;інакше шукати корінь на відрізку справа від серединної точки
                           (search f midpoint pos-point))
                           (else midpoint))))))
;-------------середнє арифметичне двох чисел------------------
(define (average x y)
     (/ (+ x y) 2))
;--------------перевірка на досягнення точності-----------------
(define (close-enough? x y)
      (< (abs (- x y)) eps))
;--------------задана функція -----------------
(define (funk x )
      (-(- (* x x)(* 2 (cos x)))1))
;--------------реалізація методу половинного ділення-----------
; f  - досліджувана функція, a,b - ліва та права межі пошуку коренів рівняння
(define (half-interval-method f a b)
       (let ((a-value (f a))
                (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
          (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else
             (error "У аргументів не різні знаки " a b)))))

;--------------реалізація методу Ньютона -----------
; наступне наближення
(define (newton-next f)
  (lambda (x)
    (- x (/ (f x) ((deriv) x)))))
(define num_x 0) ; номер кореня наближення
; метод Ньютона
(define (newton-method f guess e)
  (let ((next-guess ((newton-next f) guess))) ; пошук наступного кореня 
       (cond ((> num_x 20)(display "Зациклювання"))
      ((< (abs (f guess)) e) ; якщо f(наближення) менше заданої точності, значить знайдено корінь
                      (display "пошук кореня рівняння f (x) = 0 методом Ньютона \n")
                      (display guess)
                      (newline)
           (display "перевірка достовірності результату через підстановку значення кореня в рівняння \n")
           (display (f guess)))
      (else
           (set! num_x (+ num_x 1))
           (newton-method f next-guess e))))); перехід на наст. ітерацію з наст. наближенням
; Ньютон: проверка заданного интервала
(define (newton-interval f a b e)
  (display "======Newton method======") (newline) 
  (cond ((> (* (f a) ((deriv-2) a))) ; умова сходження для початкового наближення лівої межі
         (newton-method f a e)
         )
        ((> (* (f b) ((deriv-2) b))) ; умова сходження для початкового наближення правої межі
         (newton-method f b e)
         )
        (else
         (display "error borders"))))
;--------виклик процедур--------------------------
(display "пошук кореня рівняння f (x) = 0 методом половинного ділення \n")
(half-interval-method funk 0.0 3.0)     ;виклик методу користувача
(display "перевірка достовірності результату через підстановку значення кореня в рівняння \n")
(funk (half-interval-method funk 0.0 3.0))  ;виклик стандартного методу з підстановкою знайденого кореня
;================================
(newline)
(newton-interval funk 0.0 3.0 eps)
