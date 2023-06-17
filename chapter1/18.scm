(define (fast-multi2 a b)
	(define (halve x) (/ x 2))
	(define (double x) (* x 2))
	(define (even? x)
		(= (remainder x 2) 0))
	(define (fast-multi2-iter a b c)
		(cond ((= b 0) c)
			  ((even? b) (fast-multi2-iter (* 2 a) (/ b 2) c))
			  (else (fast-multi2-iter a (- b 1) (+ c a)))))
	(fast-multi2-iter a b 0))
