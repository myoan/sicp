#lang racket
(define (runtime) (current-inexact-milliseconds))

; --- Prime computation

(define (square x) (* x x))

(define (divides? a b)
	(= (remainder b a) 0))

(define (prime? n)
	(fast-prime? n 10))

; --- Timing

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
	(report-prime (- (runtime) start-time))
	"nothing"))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))

(define (search-for-primes start-range end-range)
	(if (even? start-range)
		(search-for-primes (+ 1 start-range) end-range)
		(cond ((> start-range end-range)
				(newline) (display "done"))
				(else (timed-prime-test start-range)
	(search-for-primes (+ 2 start-range) end-range)))))


(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp)
			(remainder (square (expmod base (/ exp 2) m))
			m))
		(else (remainder (* base (expmod base (- exp 1) m))
			m))))

(define (format-test n)
	(define (try-it a)
		(newline)
		(display "format-test ")
		(display a)
		(display " ")
		(display n)
		(display " = ")
		(display (expmod a n n))
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		((format-test n) (fast-prime? n (- times 1)))
		(else false)))
