#lang racket

(define us-coins (list 1 5 10 25 50))
(define uk-coins (list 0.5 1 2 5 10 20 50 100))

(define (no-more? li) (null? li))
(define (except-first-denomination li) (cdr li))
(define (first-denomination li) (car li))

(define (charge coins amt)
	(define (cc amount coin-values)
		(cond ((= amount 0) 1)
			((or (< amount 0) (no-more? coin-values)) 0)
			(else
				(+ (cc amount (except-first-denomination coin-values))
					(cc (- amount (first-denomination coin-values)) coin-values)))))
	(cc amt coins))

(display "pattern of charge at us-coins: ")
(charge us-coins 100) (newline)
(display "pattern of charge at uk-coins: ")
(charge uk-coins 100) (newline)