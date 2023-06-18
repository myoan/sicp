(define (cons x y)
	(* (expt 2 x) (expt 3 y)))

(define (car p)
	(define (iter n cnt)
		(if (or (= n 0) (= 1 (remainder n 2)))
			cnt
			(iter (/ n 2) (+ cnt 1))))
	(iter p 0))

(define (cdr p)
	(define (iter n cnt)
		(if (or (= n 0) (= 1 (remainder n 3)))
			cnt
			(iter (/ n 3) (+ cnt 1))))
	(iter p 0))

(display (car (cons 2 3))) (newline)
(display (car (cons 5 3))) (newline)
(display (cdr (cons 2 3))) (newline)
(display (cdr (cons 2 5))) (newline)
