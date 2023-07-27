#lang racket

(define (square x) (* x x))

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (add-complex z1 z2)
	(print z1) (newline)
	(print z2) (newline)
	(make-from-real-imag
		(+ (real-part z1) (real-part z2))
		(+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
	(make-from-real-imag
		(- (real-part z1) (real-part z2))
		(- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
	(make-from-mag-ang
		(* (magnitude z1) (magnitude z2))
		(+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
	(make-from-mag-ang
		(/ (magnitude z1) (magnitude z2))
		(- (angle z1) (angle z2))))

(define (make-from-real-imag x y)
	(display "hogehoge") (newline)
	((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'rectangular) r a))

(define (attach-tag type-tag contents)
	(cons type-tag contents))
(define (type-tag datum)
	(if (pair? datum) (car datum)
		(error "bad tagged datum: TYPE-TAG: " datum)))
(define (contents datum)
	(if (pair? datum) (cdr datum)
		(error "bad tagged datum: TYPE-TAG: " datum)))

(define (rectangular? z)
	(eq? (type-tag z) 'rectangular))
(define (polar? z)
	(eq? (type-tag z) 'polar))

(define (install-rectangular-package)
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z)
		(sqrt (+ (square (real-part z))
			(square (imag-part z)))))
	(define (angle z)
		(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(cons (* r (cos a)) (* r (sin a))))
	
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag '(rectangular)
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang '(rectangular) 
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

(install-rectangular-package)
(print (add-complex (make-from-real-imag 3 5) (make-from-real-imag 2 4))) (newline)