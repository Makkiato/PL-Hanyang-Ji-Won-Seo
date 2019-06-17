#lang racket
(provide (all-defined-out))

;(define a ' (8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))         
;(define aSame ' (8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (13 (10 () ()) (14 () ()))))
;(define aDiffer ' (8 (3 (1 () (2 () ())) (6 (4 () ()) (7 () ()))) (13 (10 () ()) (14 () ()))))
;(define b '(6 (7 ()()) (8 ()())))
;(define c '(1 () (2 () (3 () (4 () ())))))
;(define cSame '(4 (3 (2 (1 () ()) ()) ()) ()))


(define (check_bst list)(
                    let ([this (car list)]
                    [leftList  (car (cdr list))]
                    [rightList (car (cdr (cdr list)))])
                    (and
                    (if (empty? leftList) #t (and (< (car leftList) this) (check_bst leftList)))
                    (if (empty? rightList) #t (and (< this (car rightList)) (check_bst rightList)))
                     )))
                    

(define (apply func list)(
                    let ([this (car list)]
                    [leftList  (car (cdr list))]
                    [rightList (car (cdr (cdr list)))])
                    (cons (func this)
                    (cons (if (empty? leftList) '() (apply func leftList))
                    (cons (if (empty? rightList) '() (apply func rightList)) empty))
                     )))

;(define (simplify list) (
;                    let ([this (car list)]
;                    [leftList  (car (cdr list))]
;                    [rightList (car (cdr (cdr list)))])
;                     (append (if (empty? leftList) empty (simplify leftList)) (cons this null) (if (empty? rightList) empty (simplify rightList)))))


(define (equals list1 list2)(
                    letrec ([simplify (lambda (list) (
                                                    let ([this (car list)]
                                                         [leftList  (car (cdr list))]
                                                         [rightList (car (cdr (cdr list)))])
                                                     (append (if (empty? leftList) empty (simplify leftList)) (cons this null) (if (empty? rightList) empty (simplify rightList)))))]
                            [simple1  (simplify list1)]
                            [simple2 (simplify list2)])
                     (equal? simple1 simple2)))
