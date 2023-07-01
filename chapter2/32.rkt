#lang racket

(define (subsets s)
	(print s) (newline)
	(if (null? s)
		(list null)
		(let ((rest (subsets (cdr s))))
			(append rest
				(map (lambda (x) (cons (car s) x)) rest)))))

(print (subsets (list 1 2 3))) (newline)