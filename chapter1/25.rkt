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
	(remainder (fast-expt base exp) m))

(define (fast-expt b n)
	(define (square x) (* x x))
	(define (expt-iter a b n)
		(cond ((= n 0) a)
				((even? n) (expt-iter a (square b) (/ n 2)))
				(else (expt-iter (* a b) b (- n 1)))))
	(expt-iter 1 b n))

(define (format-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		((format-test n) (fast-prime? n (- times 1)))
		(else false)))
