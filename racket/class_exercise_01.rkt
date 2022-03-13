#lang racket 

(define pi 3.1416)
(define (calcPerimeter radius)
    (* (* 2 radius) pi))

(define (calcArea radius)
    (* (* radius radius) pi))

(define (calcDiameter radius)
    (* 2 radius))
;;; (define (circle number string)
;;;     (if (= string "circle")
;;;         "got circle"
;;;         "didnt get circle"))

(calcPerimeter 4)
(calcArea 4)
(calcDiameter 4)
