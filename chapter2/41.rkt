#lang racket

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define (accumulate-interval low high)
	(if (> low high)
		null
		(cons low (accumulate-interval (+ 1 low) high))))

(define (flatmap proc sec)
	(accumulate append null (map proc sec)))

(define (unique-triple n)
	(flatmap (lambda (x) x)
		(flatmap (lambda (x) x)
			(map (lambda (i)
				(map (lambda (j) 
					(map (lambda (k) (list k j i))
						(accumulate-interval 1 (- j 1))))
					(accumulate-interval 2 (- i 1))))
			(accumulate-interval 3 n)))))

(print (unique-triple 5)) (newline)

(define (combination-sum n s)
	(filter
		(lambda (x) (= (+ (car x) (cadr x) (caddr x)) s))
		(unique-triple n))
)

(print (combination-sum 5 9)) (newline)