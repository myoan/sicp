#lang racket

(require "../utils/list.rkt")

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list null)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (reset-of-queens)
						(map
							(lambda (new-row) (adjoin-position new-row k reset-of-queens))
							(enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))

(define (adjoin-position row col rest-of-queens) 
	(cons (list row col) rest-of-queens))

(define (safe? k poses)
	(define (hit-line? src dst dx dy)
		(cond
			((or (< (car src) 1) (< (+ k 1) (car src))) #f)
			((or (< (cadr src) 1) (< (+ k 1) (cadr src))) #f)
			((and (= (car src) (car dst)) (= (cadr src) (cadr dst)))
				#t)
			(else (hit-line? (list (+ (car src) dx) (+ (cadr src) dy)) dst dx dy))))
	(define (hit? src dst)
		(or
			(hit-line? src dst -1 -1)
			(hit-line? src dst 0 -1)
			(hit-line? src dst 1 -1)))
	(define (safe-iter src others)
		(cond
			((null? others) #t)
			(else (and
				(not (hit? src (car others)))
				(safe-iter src (cdr others))))))
	(if (null? (cdr poses))
		#t
		(and
			(safe-iter (car poses) (cdr poses))
			(safe? k (cdr poses)))))

(define (display-queens-result n)
	(display "[queens ")
	(print n)
	(display "](len: ")
	(print (length (queens n)))
	(display "): ")
	(print (queens n)) (newline))

; (display-queens-result 3)
(display-queens-result 4)
; (display-queens-result 5)
; (display-queens-result 8)
