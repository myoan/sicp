; (define (f n)
; 	(if (< n 4)
; 		n
; 		(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
	(define (f-itr a b c count)
		(if (= count 0)
			c
			(f-itr b c (+ (* 3 a) (* 2 b) c) (- count 1))))
	(if (< n 4)
		n
		(f-itr 1 2 3 (- n 3))))
