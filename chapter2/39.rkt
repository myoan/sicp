#lang racket

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define fold-right accumulate)
(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
		result
		(iter (op result (car rest))
			(cdr rest))))
	(iter initial sequence))

(define (reverse seq)
	(if (null? (cdr seq))
	seq
	(append (reverse (cdr seq)) (list (car seq)))))

(print (reverse (list 1 2 3 4))) (newline)

(define (reverse-fold-right seq)
	(fold-right (lambda (x y) (append y (list x))) null seq))

(print (reverse-fold-right (list 1 2 3 4))) (newline)

(define (reverse-fold-left seq)
	(fold-left (lambda (x y) (append (list y) x)) null seq))

(print (reverse-fold-left (list 1 2 3 4))) (newline)