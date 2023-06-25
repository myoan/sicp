(define (p) (p))
(define (test x y)
	(if (= x 0)
		0
		y))

; If machine is normal-order-evaluation, it expands test methods, eval if clause and return 0
; If machine is applicative-order-evaluation, it eval args first.
; So it eval p, eval p, and p...
(test 0 (p))
