(define (make-rat x y)
	(define (g x))
	(let ((g (gcd x y)))
	(if  (or
			(and (< x 0) (< y 0))
			(and (> x 0) (> y 0)))
		(cons (abs (/ x g)) (abs (/ y g)))
		(cons (* -1 (abs (/ x g))) (abs (/ y g))))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat d1 d2)
	(make-rat
		(+ (* (numer d1) (denom d2))
			(* (numer d2) (denom d1)))
		(* (denom d1) (denom d2))))

(define (sub-rat d1 d2)
	(make-rat
		(- (* (numer d1) (denom d2))
			(* (numer d2) (denom d1)))
		(* (denom d1) (denom d2))))

(define (mul-rat d1 d2)
	(make-rat
		(* (numer d1) (numer d2))
		(* (denom d1) (denom d2))))

(define (div-rat d1 d2)
	(make-rat
		(* (numer d1) (denom d2))
		(* (denom d1) (numer d2))))

(define (equal-rat? d1 d2)
	(and
		(= (numer d1) (numer d2))
		(= (denom d1) (denom d2))))

(define (print-rat d)
	(display (numer d)) 
	(display "/")
	(display (denom d))
	(newline))

(print-rat (add-rat (make-rat 1 3) (make-rat 2 5)))
(print-rat (add-rat (make-rat 1 9) (make-rat 5 9)))
(print-rat (add-rat (make-rat -1 9) (make-rat 5 -9)))
