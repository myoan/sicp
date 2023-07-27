#lang racket

(define (sorted-union-set set1 set2)
	(cond 
		((null? set2) set1)
		((null? set1) set2)
		((< (car set1) (car set2)) (cons (car set1) (sorted-union-set (cdr set1) set2)))
		((= (car set1) (car set2)) (sorted-union-set (cdr set1) set2))
		(else (cons (car set2) (sorted-union-set set1 (cdr set2))))))

(print (sorted-union-set '(1 2 3) '(4 5))) (newline)
(print (sorted-union-set '(1 3 5) '(2 4))) (newline)
(print (sorted-union-set '(1 3 5) '(2 3 4))) (newline)