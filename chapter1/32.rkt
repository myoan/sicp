; a. Desine recursive accumulate
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner a (accumulate combiner null-value term (next a) next b))))

(define (term n) n)
(define (next n) (+ n 1))

(define (sum-combiner x y) (+ x y))
(define (sum term a next b)
	(accumulate sum-combiner 0 term a next b))

(display "sum(recursive): ")
(display (sum term 1 next 10))
(newline)

(define (product-combiner x y) (* x y))
(define (product term a next b)
	(accumulate product-combiner 1 term a next b))

(display "product(recursive): ")
(display (product term 1 next 5))
(newline)

; b. Define iterative accumulate
(define (accumulate-iter combiner null-value term a next b)
	(define (iter n result)
		(if (> n b)
			result
			(iter (next n) (combiner result (term n)))))
	(iter a null-value))

(define (sum-iter term a next b)
	(accumulate-iter sum-combiner 0 term a next b))

(display "sum(iterative): ")
(display (sum-iter term 1 next 10))
(newline)

(define (product-iter term a next b)
	(accumulate-iter product-combiner 1 term a next b))

(display "product(iterative): ")
(display (product-iter term 1 next 5))
(newline)
