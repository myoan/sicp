; tangent

(define (cont-frac n d k)
	(define (cont-frac-recur a b)
		(if (> a b)
			1
			(/ (n a) (- (d a) (cont-frac-recur (+ a 1) b)))))
	(cont-frac-recur 1 k))

(define (tangent-cf x k)
	(cont-frac
		(lambda (i) (* 1.0 (expt x i)))
		(lambda (i) (- (* 2 i) 1))
		k))

(display "tangent 1 = 1.55740...")
(newline)
(newline)
(display "tangent-cf(1 3): ")
(display (tangent-cf 1 3))
(newline)

(display "tangent-cf(1 5): ")
(display (tangent-cf 1 5))
(newline)

(display "tangent-cf(1 10): ")
(display (tangent-cf 1 10))
(newline)
