#lang racket

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define (accumulate-n op initial seqs)
	(if (null? (car seqs))
		null
		(cons (accumulate op initial (map car seqs))
			(accumulate-n op initial (map cdr seqs)))))

(define (dot-product v w)
	(accumulate + 0 (map * v w)))

; 4 + 10 + 18 = 32
(print (dot-product (list 1 2 3) (list 4 5 6))) (newline)

(define (matrics-*-vector m v)
	(map (lambda (x) (dot-product x v)) m))

(define matrics-1 (list
	(list 1 2 3 4)
	(list 4 5 6 6)
	(list 6 7 8 9)))

(define matrics-2 (list
	(list 1 2 4)
	(list 4 2 1)
	(list 7 2 9)
	(list 2 4 6)))

; (30 56 80)
(print (matrics-*-vector matrics-1 (list 1 2 3 4))) (newline)

(define (transpose t)
	(accumulate-n cons null t))

; 30 
(define (matrics-*-matrics m1 m2)
	(map (lambda (v) (matrics-*-vector m1 v)) (transpose m2)))

(print (matrics-*-matrics matrics-1 matrics-2)) (newline)