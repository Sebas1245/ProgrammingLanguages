#lang racket

; Problem 1
(define (smallest-pair a b c d e)
  (cond ((and (< a c) (< a d) (< a e) (< b c) (< b d) (< b e) (if (<= a b) #t #f)) (printf "~a-~a" a b))
        ((and (< a c) (< a d) (< a e) (< b c) (< b d) (< b e)) (printf "~a-~a" b a))
        ((and (< a b) (< a d) (< a e) (< c b) (< c d) (< c e) (if (<= a c) #t #f)) (printf "~a-~a" a c))
        ((and (< a b) (< a d) (< a e) (< c b) (< c d) (< c e)) (printf "~a-~a" c a))
        ((and (< a b) (< a c) (< a e) (< d b) (< d c) (< d e) (if (<= a d) #t #f)) (printf "~a-~a" a d))
        ((and (< a b) (< a c) (< a e) (< d b) (< d c) (< d e)) (printf "~a-~a" d a))
        ((and (< a b) (< a c) (< a d) (< e b) (< e c) (< e d) (if (<= a e) #t #f)) (printf "~a-~a" a e))
        ((and (< a b) (< a c) (< a d) (< e b) (< e c) (< e d)) (printf "~a-~a" e a))
        ((and (< b a) (< b d) (< b e) (< c a) (< c d) (< c e) (if (<= b c) #t #f)) (printf "~a-~a" b c))
        ((and (< b a) (< b d) (< b e) (< c a) (< c d) (< c e)) (printf "~a-~a" c b))
        ((and (< b a) (< b c) (< b e) (< d a) (< d c) (< d e) (if (<= b d) #t #f)) (printf "~a-~a" b d))
        ((and (< b a) (< b c) (< b e) (< d a) (< d c) (< d e)) (printf "~a-~a" d b))
        ((and (< b a) (< b c) (< b d) (< e a) (< e c) (< e d) (if (<= b e) #t #f)) (printf "~a-~a" b e))
        ((and (< b a) (< b c) (< b d) (< e a) (< e c) (< e d)) (printf "~a-~a" e b))
        ((and (< c a) (< c b) (< c e) (< d a) (< d b) (< d e) (if (<= c d) #t #f)) (printf "~a-~a" c d))
        ((and (< c a) (< c b) (< c e) (< d a) (< d b) (< d e)) (printf "~a-~a" d c))
        ((and (< c a) (< c b) (< c d) (< e a) (< e b) (< e d) (if (<= c e) #t #f)) (printf "~a-~a" c e))
        ((and (< c a) (< c b) (< c d) (< e a) (< e b) (< e d)) (printf "~a-~a" e c))
        ((and (< d a) (< d b) (< d c) (< e a) (< e b) (< e c) (if (<= d e) #t #f)) (printf "~a-~a" d e))
        (else (printf "~a-~a" e d)))

; Problem 2
(define (logarithm y n)
    (if (= n 1)
        (sub1 y)
        (
            +
            (logarithm y (sub1 n)) 
            (* (expt (sub1 0) (sub1 n)) (/ (expt (sub1 y) n) n))
        )
    )
)

; Problem 3
 (define (sequence n m)
  (cond ((= n 1) (asc m))
        ((even? n) (s (- n 1) m) (newline) (des m))
        (else (s (- n 1) m) (newline) (asc m))))
     
(define (des n)
   (cond ( (= n 1)
      (display 1))
      (else (display n) (display " ") (des (- n 1)) )))

(define (asc n)
   (cond ( (= n 1)
      (display 1))
      (else (asc (- n 1)) (display " ")(display n))))

; Problem 4
(define (repeat lst)
  (if(null? lst)
     '()
     (append (repeat-n (car lst))
             (repeat (cdr lst))
  )))

(define (repeat-n n)
  (repeatAux n n))

(define (repeatAux num cont)
  (if (zero? cont)
      '()
      (cons num (repeatAux num(- cont 1)))))

; Problem 5
(define (counters lst)
  (if (null? lst)
      '()
      (intPos (car lst)(counters (cdr lst))
  )))

(define (intPos pos cnts)
  (cond ((and (= pos 1) (null? cnts)) '(1))
        ((and (> pos 1) (null? cnts)) (cons 0 (intPos (- pos 1) '())))
        ((= pos 1) (cons (+ 1 (car cnts)) (cdr cnts)))
        (else (cons (car cnts) (intPos (- pos 1) (cdr cnts))))
))

; Problem 6
(define (integers . lst)
  (if (empty? lst)
     0
     (enteros (append* lst))
   )
)

(define (enteros lst)
  (if (empty? lst)
    0
    (if (integer? (car lst))
        (+ 1 (enteros (cdr lst)))
        (+ 0 (enteros (cdr lst)))))
)

; Problem 7 
(define (prime-factors n) 
    (if (= n 1) 
        '()
        (cons (divisor n 2) (prime-factors (quotient n (divisor n 2))))
    )
)

(define (divisor n m)
    (if (= (remainder n m) 0)
        m
        (divisor n (+ m 1))
    )
)

; Problem 8 
(define (shape lst n m)
    (cond 
        ((= n 0) '() )
        ((null? lst) (cons (create-sublist lst m) '()))
        (else (cons (create-sublist lst m) (shape (slice-list lst m) (sub1 n) m)))
    )
)

(define (slice-list lst m)
    (cond 
        ((null? lst) '())
        ((= m 1) (cdr lst))
        (else (slice-list (cdr lst) (sub1 m)))
    )
)

(define (make-hyphens m)
    (if (= m 0)
        '()
        (cons '- (make-hyphens (sub1 m)))
    )
)

(define (create-sublist lst m)
    (if (= m 0)
        '()
        (if (null? lst)
            (append (make-hyphens m) '())
            (cons (car lst) (create-sublist (cdr lst) (sub1 m)))
        )
    )
)

; Problem 9
(define (enumerate lst)
  (enum-aux lst 1 1)
)

(define (enum-aux lst nivel pos)
  (cond ((null? lst)'())
        ((not (list? (car lst))) (cons (list nivel pos) (enum-aux (cdr lst) nivel (+ 1 pos))))
        (else (cons (enum-aux (car lst) (+ 1 nivel) 1) (enum-aux (cdr lst) nivel (+ 1 pos))))
        )
  )
