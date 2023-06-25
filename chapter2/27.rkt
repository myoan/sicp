#lang racket

(define (reverse li)
	(if (null? (cdr li))
	li
	(append (reverse (cdr li)) (list (car li)))))

(define (deep-reverse li)
	(if (pair? li)
	(append (deep-reverse (cdr li)) (list (deep-reverse (car li))))
	li))

(display "reverse: ")
(print (reverse (list (list 1 2) (list 3 4)))) (newline)
(display "deep-reverse: ")
(print (deep-reverse (list (list 1 2) (list 3 4)))) (newline)