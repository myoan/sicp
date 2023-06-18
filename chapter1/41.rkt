; double
(define (inc n) (+ n 1))
(define (add g)
	(lambda (x) (+ (g x) 1)))
(define (double g)
	(lambda (x) (g (g x))))

(display "double inc: ")
(display ((double inc) 1)) (newline)

(display "(((add add) inc) 5: ")
(display ((add inc) 5))
(newline)

(display "(((double double) inc) 5: ")
(display (((double double) inc) 5))
(newline)

(display "(((double (double double)) inc) 5: ")
; (((double (double (double x))) inc) 5)
; ((double (double (double (double x))) inc) 5)
; 
; (inc (inc (inc (inc (inc (inc 5)))))) = 11
(display (((double (double double)) inc) 5))
(newline)
