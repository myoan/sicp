#lang racket
; abstract execise 2.30

(define (tree-map proc tree)
	(map
		(lambda (sub-tree)
			(cond
				((pair? sub-tree) (tree-map proc sub-tree))
				(else (proc sub-tree))))
		tree))

(define (square-tree tree)
	(define (square x) (* x x))
	(tree-map square tree))

(define items (list 1
	(list 2 (list  3 4) 5)
	(list 6 7)))

(display "square: ")
(print (square-tree items)) (newline)

; double number 
(define (double-tree tree)
	(map (lambda (sub-tree)
		(cond
			((pair? sub-tree) (double-tree sub-tree))
			(else (* sub-tree 2))))
		tree))

(display "double-tree ")
(print (double-tree items)) (newline)

; double number if it's odd
(define (double-if-odd tree)
	(map (lambda (sub-tree)
		(cond
			((pair? sub-tree) (double-if-odd sub-tree))
			(else (if (even? sub-tree)
				sub-tree
				(* 2 sub-tree)))))
		tree))

(display "double-if-odd ")
(print (double-if-odd items)) (newline)

; negative
(define (negative tree)
	(tree-map (lambda (item) (* -1 item)) tree))

(display "negative ")
(print (negative items)) (newline)