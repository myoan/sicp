(define (charge amt)
	(define (cc amount kinds-of-coins)
		(cond ((= amount 0) 1)
			((or (< amount 0) (= kinds-of-coins 0)) 0)
			(else (+
					(cc (- amount (kinds kinds-of-coins))
						kinds-of-coins)
					(cc amount (- kinds-of-coins 1))))))
	(define (kinds koc)
		(cond ((= koc 1) 50)
			((= koc 2) 25)
			((= koc 3) 10)
			((= koc 4) 5)
			((= koc 5) 1)
			(else 0)))
	(cc amt 5))
