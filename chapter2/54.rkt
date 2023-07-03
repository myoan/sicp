#lang racket

(define (my-equal? x y)
	(cond
		((and (not (pair? x)) (not (pair? y))) (eq? x y))
		((or (not (pair? x)) (not (pair? y))) #f)
		(else (and
			(my-equal? (car x) (car y))
			(my-equal? (cdr x) (cdr y))))))

(print (my-equal? '(this is a list) '(this is a list))) (newline)
(print (my-equal? '(this is a list) '(this (is a) list))) (newline)