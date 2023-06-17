; Define iterative sum function

(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))))
	(iter a 0))

(define (inc n) (+ n 1))
(define (cube n) (* n n n ))
(define (sum-cubes a b)
	(sum cube a inc b))

(display "sum-cubes ")
(display (sum-cubes 1 10))
(newline)
