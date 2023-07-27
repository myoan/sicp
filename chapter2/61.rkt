#lang racket

(define (element-of-set? x set)
	(cond ((null? set) #f)
		((= x (car set)) #t)
		((< x (car set)) #f)
		(else (element-of-set? x (cdr set)))))

(define (sorted-adjoin-set x set)
	(cond ((null? set) (cons x set))
		((= x (car set)) set)
		((> x (car set)) (cons (car set) (sorted-adjoin-set x (cdr set))))
		(else (cons x set))))

; (print (sorted-adjoin-set 5 '(1 3 5 7))) (newline)
; (print (sorted-adjoin-set 5 '(1 3 6 7))) (newline)

(define (sort set)
	(define (insert x set)
		(cond ((null? set) (list x))
			((< x (car set)) (cons x set))
			(else (cons (car set) (insert x (cdr set))))))
	(define (bubble-sort x set result)
		(cond ((null? set) (insert x result))
			(else (bubble-sort (car set) (cdr set) (insert x result)))))
	(bubble-sort (car set) (cdr set) '()))

(print (sort'(4 1 5 3 7))) (newline)