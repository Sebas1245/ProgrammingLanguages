#lang racket
; Assignment 04
; Problems on Data Structures
; Authors: 

; Excercise 1: Binary trees
(define AB 
    '(8 (5 (2 () ())  
            (7 () ())) 
        (9 () 
            (15 (11 () ()) 
                () )))) 
(define CD
'(a (b (() ()) ()) (d () ())))

(define EG
'(a (d () ()) (b (() ()) ()) ))

;;; (8 (5 () ()) (9 () ()))

; problem a)
(define (binary-tree? bin_tree)
    (cond 
        ((null? bin_tree) #t)
        ((and 
            (not (list? (car bin_tree)))
            (list? (cadr bin_tree))
            (list? (car (cddr bin_tree)))
        ) (and (binary-tree? (cadr bin_tree)) (binary-tree? (car (cddr bin_tree))))) 
        (else #f)
))

; problem b) 
(define (get-minors bt val) 
    (if (null? bt)
        '()
        (if (> val (car bt))
            (make-bt-list bt)
            (get-minors (cadr bt) val)
        )
    )
)

(define (make-bt-list bt)
    (if (null? bt)
        '()
        (append (cons (car bt) (make-bt-list (cadr bt))) (make-bt-list (car (cddr bt))))
    )
)
; problem c)
(define (longest-branch bin_tree)
    (if (null? bin_tree)
        '()
        (if  (> 
                (length (cons (car bin_tree) (longest-branch (car (cddr bin_tree)))))
                (length (cons (car bin_tree) (longest-branch (cadr bin_tree)))))
            (cons (car bin_tree) (longest-branch (car (cddr bin_tree))))
            (cons (car bin_tree) (longest-branch (cadr bin_tree)))
        )
    )
)

; Exercise 2: Graphs
(define g '( 
    (A (B 2) (D 10))
    (B (C 9) (E 5)) 
    (C (A 12) (D 6)) 
    (D (E 7)) 
    (E (C 3)) )
)

(define h '( 
    (A (B 2) (D 10))
    (B (B 9) (E 5)) 
    (C (B 12) (D 6)) 
    (D (B 7)) 
    (E (B 3)) )
) 

(define (destination-nodes graph node)
    (cond 
        ((null? graph) '())
        ((eq? node (caar graph)) (get-dest-nodes (cdar graph)))
        (else (destination-nodes (cdr graph) node))
    )
)

(define (get-dest-nodes adj-list)
    (if (null? adj-list)
        '()
        (cons (caar adj-list) (get-dest-nodes (cdr adj-list)))
    )
)

(define (source-nodes graph node)
    (if (null? graph) 
        '()
        (append (get-source-nodes (cdar graph) (caar graph) node) (source-nodes (cdr graph) node)) 
    )
)

(define (get-source-nodes adj-list parent-node node)
    (if (null? adj-list)
        '()
        (if (eq? (caar adj-list) node)
            (cons parent-node (get-source-nodes (cdr adj-list) parent-node node))
            (get-source-nodes (cdr adj-list) parent-node node)
        )
    )
)

(define (delete-arc graph node1 node2)
    (cond 
        ((null? (car graph)) '())
        ((eq? (caar graph) node1) 
            (cons (cons (caar graph) (remove-arc (cdar graph) node2)) (cdr graph))
        )
        (else 
            (cons (car graph) (delete-arc (cdr graph) node1 node2))
        )
    )
)

(define (remove-arc adj-lst node2)
    (cond 
        ((null? adj-lst) '())
        ((eq? (caar adj-lst) node2) (remove-arc (cdr adj-lst) node2))
        (else (cons (car adj-lst) (remove-arc (cdr adj-lst) node2)))
    )
)

; Problem 3: Higher order functions
; ex c)
(define mat1 '((1 2 3)(0 2 1))) 
(define mat2 '((4 0 3 1)(5 1 2 1)(6 0 1 1)))

(define (multmat matrix1 matrix2)
    (map 
        (lambda (row)
            (map 
                (lambda (col)
                    (apply +
                        (map * row col)))
                (apply map list matrix2)))
        matrix1)
)
