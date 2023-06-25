#lang racket
(define (make-interval a b) (cons a b))
(define upper-bound cdr)
(define lower-bound car)
(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
	(make-interval (- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
	(mul-interval
		x
		(make-interval (/ 1.0 (upper-bound y))
						(/ 1.0 (lower-bound y)))))

(define i1 (make-interval 2 4))
(define i2 (make-interval 6 8))

(display "lower-bound: ")
(print (lower-bound i1)) (newline)
(display "upper-bound: ")
(print (upper-bound i1)) (newline)
(display "add: ")
(print (add-interval i1 i2)) (newline)
(display "sub: ")
(print (sub-interval i1 i2)) (newline)
(display "mul: ")
(print (mul-interval i1 i2)) (newline)
(display "div: ")
(print (div-interval i1 i2)) (newline)