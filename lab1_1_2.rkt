#lang racket

( define (dob a b) 
( if ( = a 0 ) 
0
( if ( = b 0 ) 
0
( if ( = a 1 ) 
b
( if ( = b 1) 
a
 ( + a ( dob a (- b 1)  
) ) )))))
(display "Введіть а:") 
(define A (read))
(display "Введіть b:")
(define B (read))
(display (dob A B))
