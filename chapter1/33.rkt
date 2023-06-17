; Desine filtered-accumulate
; a. Define sum of square of prime numbers

(define (filtered-accumulate combiner null-value flt term a next b)
	(if (> a b)
		null-value
		(if (flt a)
			(combiner (term a) (filtered-accumulate combiner null-value flt term (next a) next b))
			(filtered-accumulate combiner null-value flt term (next a) next b))))

(define (nop n) n)
(define (square x) (* x x))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
	(= (remainder b a) 0))

(define (prime? n)
	(if (= n 1)
		#f
		(= n (smallest-divisor n))))

(define (next n) (+ n 1))
(define (sum-combiner x y) (+ x y))

(define (sum-of-square-of-prime term a next b)
	(filtered-accumulate sum-combiner 0 prime? term a next b))

; 4 + 9 + 25 + 49 = 87
(display "sum-of-square-of-prime: ")
(display (sum-of-square-of-prime square 1 next 10))
(newline)

; a. Desine product of relative prime

(define (product-combiner x y) (* x y))
(define (gcd x y)
	(if (= y 0)
		x
		(gcd y (remainder x y))))
(define (relative-prime? i n)
	(= (gcd i n) 1))

(define (product-of-relative-prime term next n)
	(define (inner-relative-prime? i)
		(relative-prime? i n))
	(filtered-accumulate product-combiner 1 inner-relative-prime? term 1 next n))

; 1, 3, 7, 9 = 189
(display "product-of-relative-prime: ")
(display (product-of-relative-prime nop next 10))
(newline)
