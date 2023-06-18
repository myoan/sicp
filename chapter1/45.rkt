(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? a b)
		(< (abs (- a b)) tolerance))
	(define (try guess)
		; (display "try: ")
		; (display guess) (newline)
		(let ((next (f guess)))
		(if (close-enough? guess next)
			next
			(try next))))
	(try first-guess))

(define (diverge-sqrt x)
	(fixed-point (lambda (y) (/ x y)) 1.0))

; diverge-sqrt does not converge, but loop y1 and y2
; y2 = x / y1
; y3 = x / y2 = x / (x / y1) = y1
; (display (diverge-sqrt 4.0))

(define (sqrt x)
	(fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (compose f g)
	(lambda (x) (f (g x))))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (repeat f n)
	(if (= n 1)
		f
		(compose f (repeat f (- n 1)))))

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (third-sqrt x)
	(fixed-point (lambda (y) (average y (/ x (square y)))) 1.0))

(define (power x n)
	(if (< n 1)
		1
		(* x (power x (- n 1)))))

(define (fourth-sqrt x)
	(fixed-point (repeat (average-damp (lambda (y) (/ x (power y 3)))) 2) 1.0))

(define (n-th-sqrt x n times)
	(fixed-point
		((repeat average-damp times)
		(lambda (y)
			(/ x (power y (- n 1)))))
		1.0))

(define (examine n times)
	(display "n-th-sqrt(")
	(display (power 2 n))
	(display " ")
	(display n)
	(display " ")
	(display times)
	(display "): ")
	(display (n-th-sqrt (power 2 n) n times)) (newline))

(define (try-examine start end times)
	(define (try a)
		(examine a times)
		(if (= a end)
			"done"
			(try (+ a 1))))
	(try start))

(try-examine 2 3 1)
(try-examine 4 7 2)
(try-examine 8 15 3)
(try-examine 16 31 4)
(try-examine 32 63 5)

; times = floor(log 2 nth)
