#lang racket

(define (square-tree tree)
	(cond 
		((null? tree) null)
		((not (pair? tree))
			(* tree tree))
		(else
			(cons
			(square-tree (car tree))
			(square-tree (cdr tree))))))

(square-tree (list 1
	(list 2 (list  3 4) 5)
	(list 6 7)))

; (map (lambda (x) (* x x)) (list 1 2 3 4))

(define (square-tree-map tree)
	(define (square-sub-tree sub-tree)
		(cond
			((null? sub-tree) null)
			((pair? sub-tree) (square-tree-map sub-tree))
			(else (* sub-tree sub-tree))))
	(map square-sub-tree tree))

(square-tree-map (list 1
	(list 2 (list  3 4) 5)
	(list 6 7)))