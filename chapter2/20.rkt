#lang racket

(define (same-parity x . li)
	(define (check-parity y) (= (remainder (- y x) 2) 0))
	(define (do-parity l) 
		(if (null? l)
			'()
			(if (check-parity (car l))
				(cons (car l) (do-parity (cdr l)))
				(do-parity (cdr l)))))
	(cons x (do-parity li)))

(display "(1 2 3 4 5 6 7) -> ")
(display (same-parity 1 2 3 4 5 6 7)) (newline)
(display "(2 3 4 5 6 7) -> ")
(display (same-parity 2 3 4 5 6 7)) (newline)