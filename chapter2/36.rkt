#lang racket

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(define (transpose-list seq)
	(if (pair? (cdr seq))
		(cons (list (car seq)) (transpose-list (cdr seq)))
		(list seq)))

(print (transpose-list (list 1 2 3))) (newline)
(print (list (list 1) (list 2) (list 3))) (newline)

(define (accumulate-n op initial seqs)
	(if (null? (car seqs))
		null
		(cons (accumulate op initial (map car seqs))
			(accumulate-n op initial (map cdr seqs)))))

(print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))) (newline)

(define (transpose t)
	(accumulate-n cons null t))
(print (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))) (newline)