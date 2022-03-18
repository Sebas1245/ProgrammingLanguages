#lang racket 

(define (calcPerimeter radius)
    (* (* 2 radius) pi))

(define (calcArea radius)
    (* (* radius radius) pi))

(define (calcDiameter radius)
    (* 2 radius))

(define (circle radius property)
    (cond 
        ((eq? property 'perimeter) (calcPerimeter radius) )
        ((eq? property 'area) (calcArea radius))
        ((eq? property 'diameter) (calcDiameter radius))
        (else 'invalid_property)
    )
)

(define (largest a b c d)
    (cond 
    ((and (>= a b) (>= a c) (>= d)) a) 
    ((and (>= b c) (>= c)) b)
    ((>= c d) c)
    (else d)))

(define (factorial n)
    (if (zero? n) 
        1
        (* n (factorial (- n 1)))))

(define (sum n)
    (if (= n 1)
        1
        (+ n (sum (- n 1))) 
    ))