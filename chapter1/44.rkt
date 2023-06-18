(define (compose f g)
	(lambda (x) (f (g x))))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (repeat f n)
	(define (iter f g n)
		(if (= n 0)
			(lambda (x) (g x))
			(iter f (compose f g) (- n 1))))
	(iter f (lambda (x) x) n))

(define dx 0.00001)
(define (smooth f)
	(lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
	(repeat (smooth f) n))

((n-fold-smooth inc 10) 3)
