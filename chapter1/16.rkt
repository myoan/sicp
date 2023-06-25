; (define (expt b n)
; 	(if (= n 0)
; 		1
; 		(* b (expt b (- n 1)))))
; 
; (define (fast-expt b n)
; 	(define (square x) (* x x))
; 	(define (even? n)
; 		(= (remainder n 2) 0))
; 	(cond ((= n 0) 1)
; 			((even? n) (square (fast-expt b (/ n 2))))
; 			(else (* b (fast-expt b (- n 1))))))

(define (expt2 b n)
	(define (expt-iter ans b n)
		(if (= n 0)
		  ans
		  (expt-iter (* ans b) b (- n 1))))
	(expt-iter 1 b n))

(define (fast-expt2 b n)
	(define (square x) (* x x))
	(define (expt-iter a b n)
		(cond ((= n 0) a)
				((even? n) (expt-iter a (square b) (/ n 2)))
				(else (expt-iter (* a b) b (- n 1)))))
	(expt-iter 1 b n))
