(define (sum-of-square-of-largest-two x y z)
	(cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
		((and (< y x) (< y z)) (+ (* x x) (* z z)))
		(else (+ (* x x) (* y y)))))
(sum-of-square-of-largest-two 1 2 3)
(sum-of-square-of-largest-two 2 1 3)
(sum-of-square-of-largest-two 3 2 1)
