#lang racket

; --- Prime computation

(define (square x) (* x x))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
	(= (remainder b a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

; --- accumulate

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define (accumulate-interval low high)
	(if (> low high)
		null
		(cons low (accumulate-interval (+ 1 low) high))))

(define (filter predicate sequence)
	(cond
		((null? sequence) null)
		((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

; ---

(define (flatmap proc sec)
	(accumulate append null (map proc sec)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
	(map (lambda (i)
		(map (lambda (j) (list i j))
			(accumulate-interval 1 (- i 1))))
	(accumulate-interval 2 n)))

(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum?
			(flatmap (lambda (x) x)
				(unique-pairs n)))))

(print (prime-sum-pairs 6)) (newline)