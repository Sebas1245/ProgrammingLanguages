#lang racket

(define (add-matrices m1 m2)
    (if (null? m1)
        m1
        (cons (add-rows (car m1) (car m2))
                (add-matrices (cdr m1) (cdr m2)))
    )
)

(define (add-rows r1 r2)
    (if (null? r1)
        r1
        (cons (+ (car r1) (car r2))
              (add-rows (cdr r1) (cdr r2)))
    )
)


