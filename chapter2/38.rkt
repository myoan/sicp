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

; 1 / (2 / 3) = 3/2
(print (fold-right / 1 (list 1 2 3))) (newline)
; (1 / 2) / 3 = 1/6
(print (fold-left / 1 (list 1 2 3))) (newline)
; (1 (2 (3 ())))
(print (fold-right list null (list 1 2 3))) (newline)
; (((() 1) 2) 3)
(print (fold-left list null (list 1 2 3))) (newline)
