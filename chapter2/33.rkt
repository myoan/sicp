#lang racket

(define (filter predicate sequence)
	(cond
		((null? sequence) null)
		((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(print (filter odd? (list 1 2 3 4 5))) (newline)
; (1 3 5)

(define (accumulate op initial sequence)
	(if (pair? sequence)
		(op (car sequence) (accumulate op initial (cdr sequence)))
		initial))

(print (accumulate + 0 (list 1 2 3 4 5))) (newline)
; 15

(print (accumulate * 1 (list 1 2 3 4 5))) (newline)
; 120

(print (accumulate cons null (list 1 2 3 4 5))) (newline)
; (1 2 3 4 5)

(define (accumulate-interval low high)
	(if (> low high)
		null
		(cons low (accumulate-interval (+ 1 low) high))))

(print (accumulate-interval 2 7)) (newline)
; (2 3 4 5 6 7)

(define (accumulate-tree tree)
	(cond
		((null? tree) null)
		((pair? (car tree)) (append (accumulate-tree (car tree)) (accumulate-tree (cdr tree))))
		(else (cons (car tree) (accumulate-tree (cdr tree))))))

(print (accumulate-tree (list 1 (list 2 (list 3 4) 5)))) (newline)
; (1 2 3 4 5)

(define (sum-odd-squares tree)
	(define (square x) (* x x))
	(accumulate + 0 (map square (filter odd? (accumulate-tree tree)))))


(print (sum-odd-squares (list 1 2 3 4 5))) (newline)
; (1 2 3 4 5)

(define (even-fibs n)
	(define (fib m)
		(if (< m 2)
			1
			(+ (fib (- m 1)) (fib (- m 2)))))
	(accumulate cons null (filter even? (map fib (accumulate-interval 0 n))))
)

(print (even-fibs 10)) (newline)
; (2 8 34)

(define (my-map op sequence)
	(accumulate (lambda (x y) (cons (op x) y)) null sequence))

(print (my-map (lambda (x) (* x x)) (list 1 2 3))) (newline)
; 1 4 9

(define (my-append seq1 seq2)
	(accumulate cons seq2 seq1))

(print (my-append (list 1 2 3) (list 4 5 6))) (newline)
; 1 2 3 4 5 6

(define (my-length sequence)
	; (accumulate + 0 (map (lambda (x) 1) sequence)))
	(accumulate (lambda (x y) (+ y 1)) 0 sequence))

(print (my-length (list 1 2 3 4 5))) (newline)
; 3