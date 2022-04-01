#lang racket

(define (sum-matrix matrix)
    (apply + (map (lambda (row) (apply + row)) matrix))
)

(define (transpose-matrix matrix)
    (apply map list matrix)
)

(define (apply-lists procs l1 l2)
    (if (null? procs)
        '()
        (cons (apply-procs (car procs) l1 l2) (apply-lists (cdr procs) l1 l2))
    )
)

(define (apply-procs function l1 l2)
    (if (null? l1)
        '()
        (cons (function (car l1) (car l2)) (apply-procs function (cdr l1) (cdr l2)))
    )
)