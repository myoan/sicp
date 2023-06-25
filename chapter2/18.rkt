#lang racket
(define (reverse li)
	(if (null? (cdr li))
	li
	(append (reverse (cdr li)) (list (car li)))))

(print (reverse (list 1 2 3 4))) (newline)