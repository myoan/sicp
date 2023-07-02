#lang racket

(require "../utils/list.rkt") 

(define (unique-triple n)
	(flatmap (lambda (x) x)
		(flatmap (lambda (x) x)
			(map (lambda (i)
				(map (lambda (j) 
					(map (lambda (k) (list k j i))
						(enumerate-interval 1 (- j 1))))
					(enumerate-interval 2 (- i 1))))
			(enumerate-interval 3 n)))))

(print (unique-triple 5)) (newline)

(define (combination-sum n s)
	(filter
		(lambda (x) (= (+ (car x) (cadr x) (caddr x)) s))
		(unique-triple n))
)

(print (combination-sum 5 9)) (newline)