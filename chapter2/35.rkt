#lang racket

(define (accumulate op initial sequence)
	(display "accumulate ") (print sequence) (newline)
	(cond ((pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence))))
		(else initial)))


; (define (count-leaves t)
; 	(cond ((null? t) 0)
; 		((not (pair? t)) 1)
; 		(else (+ (count-leaaves (car t))
; 				(count-leaaves (cdr t))))))

(define (count-leaves t)
	(accumulate + 0
		(map (lambda (sub) (if (pair? sub) (count-leaves sub) 1)) t)))

(define x (cons (list 1 2) (list 3 4)))
(print (count-leaves (list x x))) (newline)