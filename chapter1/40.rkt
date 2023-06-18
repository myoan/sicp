; newton-method
(define tolerance 0.000001)

(define (average a b) (/ (+ a b) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let
		  	((next (f guess)))
			(display guess)
			(newline)
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (deriv g)
	(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
(define (cube x) (* x x x))

(display "deriv cube 5: ")
(display ((deriv cube) 5))

(define (newtons-transform g)
	(lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
	(fixed-point (newtons-transform g) guess))

(define (cubic a b c)
	(lambda (x)
		(+ (* x x x) (* a x x) (* b x) c)))

(display (newtons-method (cubic 1 1 1) 1))
