#lang racket

(define (fringe items)
	(cond
		((null? items) null)
		((pair? items)
			(append (fringe (car items)) (fringe (cdr items))))
		(else
			(list items))))

(define x (list (list 1 2) (list 3 4)))
(display (fringe x)) (newline)
(display (fringe (list x x))) (newline)
(display (fringe (list 7 (list x x) 8))) (newline)