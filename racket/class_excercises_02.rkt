#lang racket

; Code a function that serves to raisa a number to a certain non-negative integer power (a^b)
(define (pow a b)
    (if (zero? b) 1
        (* a (pow a (sub1 b)))))

; factorial with tail recursion , a more efficient recursion method
(define (factorial n)
    (factorial-aux n 1))

(define (factorial-aux n r)
    (if (<= n 1) r
        (factorial-aux (- n 1) (* n r))))

(define (series-sum n)
    (if (= 1 n) 3.0
        (+  1 (/ 2 n)  (series-sum (- n 1)))))

(define (odds n)
    (if (< n 10)
        (if (odd? n) 1 0)
        (+  (odds (remainder n 10)) ; for the rest of the digitds
            (odds (quotient n 10)) ; for the current digit 
        )
    )
)

(define (fibo num)
    (if (< num 3) 1
        (+  (fibo (- num 1))
            (fibo (- num 2))
        )
    )
)

; tail recursive fibonacci series
(define (fibot num)
    (fibot-aux num 1 1))

(define (fibot-aux n p2 p1)
    (if (< n 3)
        p1
        (fibot-aux (- n 1) p1 (+ p2 p1))))