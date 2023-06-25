; (define (mul a b)
; 	(if (= b 0)
; 		0
; 		(+ a (mul a (- b 1)))))

(define (fast-multi a b)
	(define (halve x) (/ x 2))
	(define (double x) (* x 2))
 	(define (even? x)
 		(= (remainder x 2) 0))
	(cond ((= b 1) a)
			((even? b) (fast-multi (double a) (halve b)))
			(else (+ a (fast-multi a (- b 1))))))
