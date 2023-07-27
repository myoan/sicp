#lang racket

(define (element-of-set? x set)
	(cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))

(print (adjoin-set 7 '(1 3 5 7))) (newline)
(print (adjoin-set 7 '(1 3 5 6))) (newline)

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1) (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

(print (intersection-set '(7 3 2) '(1 3 5 6))) (newline)

(define (union-set set1 set2)
	(cond ((null? set1) set2)
		((element-of-set? (car set1) set2)
			(union-set (cdr set1) set2))
		(else (cons (car set1) (union-set (cdr set1) set2)))))

(print (union-set '(7 3 2) '(1 3 5 6))) (newline)