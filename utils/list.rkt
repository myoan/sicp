#lang racket

(provide accumulate
	enumerate-interval
	flatmap
	filter
	any?)

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define (enumerate-interval low high)
	(if (> low high)
		null
		(cons low (enumerate-interval (+ 1 low) high))))

(define (flatmap proc sec)
	(accumulate append null (map proc sec)))

(define (filter predicate sequence)
	(cond
		((null? sequence) null)
		((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (any? proc sec)
	(accumulate (lambda (x y) (or x y)) #f (map proc sec)))