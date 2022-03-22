#lang racket

(define (count 1st)
    (if (null? 1st)
        0
        (+ 1 (count (cdr 1st)))))

(define (zeros n)
    (if (zero? n)
        '()
        (cons 0 (zeros (- n 1)))))

(define (count-atoms lst)
    (cond 
        ((null? lst) 0)
        ((list? (car lst)) 
            (+ (count-atoms (car lst))
                (count-atoms (cdr lst))
            )
        )
        (else (+ 1 (count-atoms (cdr lst))))
    )
)