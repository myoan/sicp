(define (iterative-improve good-enough? improve)
	(lambda (guess)
		; (display guess) (newline)
		(if (good-enough? guess)
			guess
			((iterative-improve good-enough? improve) (improve guess)))))

(define (sqrt x)
	((iterative-improve
		(lambda (guess) (< (abs (- (square guess) x)) 0.0001))
		(lambda (guess) (average guess (/ x guess))))
	1.0))

(display "sqrt: ")
(display (sqrt 9)) (newline)

(define (fixed-point f first-guess)
	((iterative-improve
		(lambda (guess) (< (abs (- (f guess) guess)) 0.00001))
		(lambda (guess) (f guess)))
	first-guess))

(display "new-fixed-point: ")
(display (fixed-point cos 1.0)) (newline)
