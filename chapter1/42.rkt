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

((repeat square 2) 5)
