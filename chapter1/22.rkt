#lang racket
(define (runtime) (current-inexact-milliseconds))

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

; three smallest-primes larger than number and reported time(us)
; - 1009:    2.19
; - 1013:    1.7
; - 1019:    3.9
; - 10007:   5.3
; - 10009:   6.1
; - 10037:   5.3
; - 100003:  14.8
; - 100019:  12.4
; - 100043:  15.6
; - 1000003: 51
; - 1000033: 51
; - 1000037: 51

