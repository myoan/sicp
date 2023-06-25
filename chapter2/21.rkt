#lang racket

(define (square x) (* x x))
(define (map proc items)
	(if (null? items)
	null
	(cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
	(if (null? items)
	null
	(cons (square (car items)) (square-list (cdr items)))))
(define (square-list-map items)
	(map square items))

(print (square-list (list 1 2 3 4))) (newline)
(print (square-list-map (list 1 2 3 4))) (newline)
; (1 4 9 16)
