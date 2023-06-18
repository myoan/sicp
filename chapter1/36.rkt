(define tolerance 0.000001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let
		  	((next (f guess)))
			((display guess)
			(newline)
			(if (close-enough? guess next)
				next
				(try next)))))
	(try first-guess))

(display (fixed-point (lambda (y) (/ (log 1000) (log y))) 1.1))
(newline)
