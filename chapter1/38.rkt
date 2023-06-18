; De Fractionibus Continuis

(define (cont-frac n d k)
	(define (cont-frac-recur a b)
		(if (> a b)
			1
			(/ (n a) (+ (d a) (cont-frac-recur (+ a 1) b)))))
	(cont-frac-recur 1 k))

(define (napier k)
	(cont-frac
		(lambda (i) 1.0)
		(lambda (i)
			(if (= (remainder i 3) 2)
				(* (+ (quotient i 3) 1) 2)
				1))
		k))

(display "napier(3): ")
(display (+ 2 (napier 3)))
(newline)

(display "napier(5): ")
(display (+ 2 (napier 5)))
(newline)

(display "napier(10): ")
(display (+ 2 (napier 10)))
(newline)
