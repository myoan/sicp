; a: Define product and factorial as recursive

(define (product term a next b)
	(if (> a b)
		1
		(* (term a) (product term (next a) next b))))

(define (factorial n)
	(define (nop a) a)
	(define (next a) (+ a 1))
	(product nop 1 next n))

(define (display-factorial term n)
	(display "factorial[")
	(display n)
	(display "] ")
	(display (term n))
	(newline))

(display-factorial factorial 2)
(display-factorial factorial 3)
(display-factorial factorial 4)
(display-factorial factorial 5)

; Approximation of PI by John Walli's
; PI / 4 = (2 * 4 * 4 * 6 * 6) / (3 * 3 * 5 * 5* 7)

(define (johns-pi times)
	(define (top-term n)
		(if (even? n) (+ n 2) (+ n 1)))
	(define (btm-term n)
		(if (odd? n) (+ n 2) (+ n 1)))
	(define (next n) (+ n 1))
	(/ (* 1.0 (product top-term 1 next times))
		(product btm-term 1 next times)))

(display "John's PI: ")
(display (* 4 (johns-pi 160)))
(newline)

; b: Define product and factorial as iterative
(define (product-iter term a next b)
	(define (iter n result)
		(if (> n b)
			result
			(iter (next n) (* (term n) result))))
	(iter a 1))

(define (factorial-iter n)
	(define (nop a) a)
	(define (next a) (+ a 1))
	(product-iter nop 1 next n))

(display-factorial factorial-iter 5)
