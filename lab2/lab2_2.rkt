#lang racket
(define (calc-fraction acc)
  (calc-fraction-high-iter 0 acc))

(define (calc-fraction-high-iter iter-count acc)
  (let ((prev (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t iter-count)))))
        (next (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t (+ iter-count 1)))))))
  (if (< (abs (- next prev)) acc)
      next
      (calc-fraction-high-iter (+ iter-count 1) acc))))

(define (calc-fraction-iter is-even iter-count)
  (let ((num (if is-even 2 1)))
  (cond ((equal? 0 iter-count) (/ 1 num))
        (else
         (/ 1 (+ num (calc-fraction-iter (not is-even) (- iter-count 1))))))))
(newline)
(calc-fraction 0.01)
