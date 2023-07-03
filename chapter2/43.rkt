#lang racket

(require "../utils/list.rkt")

(define (evas-queens board-size)
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

(define (luises-queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list null)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (new-row)
						(map (lambda (rest-of-queens)
							(adjoin-position new-row k rest-of-queens))
						(queen-cols (- k 1))))
					(enumerate-interval 1 board-size)))))
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

(define (runtime) (current-inexact-milliseconds))

(define (time-queens proc n)
	(let ((start (runtime)))
		(proc n)
		(proc n)
		(proc n)
		(proc n)
		(proc n)
		(display (/ (- (runtime) start) 5))))

(display "Eva's 4: ") (print (time-queens evas-queens 4)) (newline)
(display "Luise's 4: ") (print (time-queens luises-queens 4)) (newline)
(display "Eva's 5: ") (print (time-queens evas-queens 5)) (newline)
(display "Luise's 5: ") (print (time-queens luises-queens 5)) (newline)
(display "Eva's 6: ") (print (time-queens evas-queens 6)) (newline)
(display "Luise's 6: ") (print (time-queens luises-queens 6)) (newline)
(display "Eva's 7: ") (print (time-queens evas-queens 7)) (newline)
(display "Luise's 78: ") (print (time-queens luises-queens 7)) (newline)
(display "Eva's 8: ") (print (time-queens evas-queens 8)) (newline)
(display "Luise's 8: ") (print (time-queens luises-queens 8)) (newline)