#lang racket/base

; determine if three values define a triangle
(define (triangle? a b c)
    (and  
        (> (+ b c) a) 
        (> (+ a b) c)
        (> (+ a c) b) ) )

(define (triangle a b c)
    (cond ( (not (triangle? a b c) ) 'no-triangle) 
        ( (= a b c) 'equilateral)
        ( (or (= a b) (= a c) (= b c) ) 'isosceles)
        ( else 'scalene ) ) )

