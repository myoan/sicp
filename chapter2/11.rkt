#lang racket
(define (make-interval a b) (cons a b))
(define upper-bound cdr)
(define lower-bound car)

(define (mul-interval x y)
	(let (
		(lx (lower-bound x))
		(ux (upper-bound x))
		(ly (lower-bound y))
		(uy (upper-bound y)))
		(cond
			; + + + +
			((and (> lx 0) (> ux 0) (> ly 0) (> uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; - + + +
			((and (< lx 0) (> ux 0) (> ly 0) (> uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; - - + +
			((and (< lx 0) (< ux 0) (> ly 0) (> uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; - - - +
			((and (< lx 0) (< ux 0) (< ly 0) (> uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; - - - -
			((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; - + - +
			((and (< lx 0) (> ux 0) (< ly 0) (> uy 0))
				(make-interval (min (* lx ly) (* ux uy)) (max (* lx ly) (* ux uy))))
			; - + - -
			((and (< lx 0) (> ux 0) (< ly 0) (< uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; + + - +
			((and (> lx 0) (> ux 0) (< ly 0) (> uy 0))
				(make-interval (* lx ly) (* ux uy) ))
			; + + - -
			((and (> lx 0) (> ux 0) (< ly 0) (< uy 0))
				(make-interval (* lx ly) (* ux uy) )))))

(define i1 (make-interval 1 5))
(define i2 (make-interval -2 2))

(display "div: ")
(print (mul-interval i1 i2)) (newline)