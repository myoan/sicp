; Pascal's Triangle
; 
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1

(define (pascal depth index)
	(cond ((< index 0) 0)
		((> index depth) 0)
		((= index depth) 1)
		((= index 1) 1)
		(else
			(+ (pascal (- depth 1) (- index 1))
				(pascal (- depth 1) index)))))
