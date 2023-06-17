(define (sum term a next b)
	(if (> a b)
	0
	(+ (term a)
		(sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n ))
(define (sum-cubes a b)
	(sum cube a inc b))

(display "sum-cubes ")
(display (sum-cubes 1 10))
(newline)

(define (pie n) (/ 1.0 (* n (+ n 2))))
(define (inc-4 n) (+ n 4))
(define (sum-pie a b)
	(sum pie a inc-4 b))

(display "sum-pies ")
(display (* (sum-pie 1 300) 8))
(newline)

(define (add n) n)
(define (sum-integer a b)
	(sum add a inc b))

(display "sum-integer ")
(display (sum-integer 1 10))
(newline)

(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (sum f (+ a (/ dx 2)) add-dx b) dx))

(display "integral ")
(display (integral cube 0 1 0.01))
(newline)

(define (composit-simpsons-rule f a b dx)
	(define (simpsons-next n)
		(+ n (* 2 dx)))
	(* (/ (- b a) (/ (- b a) dx) 3)
	(+ (f a)
		(* 4 (sum f (+ a dx) simpsons-next b))
		(* 2 (sum f (+ a (* 2 dx)) simpsons-next b))
		(f b))))

(display "composit-simpsons-rule: ")
(display (composit-simpsons-rule cube 0 1 0.01))
(newline)
