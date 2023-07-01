#lang racket

; (define (horner-eval x coefficient-sequence)
; 	(if (pair? coefficient-sequence)
; 	(+ (car coefficient-sequence) (* x (horner-eval x (cdr coefficient-sequence))))
; 	0))

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
		0
		coefficient-sequence))

(print (horner-eval 2 (list 1 3 0 5 0 1))) (newline)
; 79