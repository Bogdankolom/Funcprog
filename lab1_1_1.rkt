#lang racket

( define (dob a b)
 (define (dob-times a b acc)
( if ( = b 0 ) 
acc
(dob-times a (- b 1) ( + a acc))))
( if ( = b 0 ) 
0
 ( if ( = a 0 ) 
0
 ( if ( = a 1 ) 
b
( if ( = b 1) 
a
(dob-times a b 0))))))
(display "Введіть а:") 
(define A (read))
(display "Введіть b:")
(define B (read))
(display (dob A B))