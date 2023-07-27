#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (operands x) (cdr x))

(define (deriv exp var)
	(cond ((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		((sum? exp)
			(deriv-sum (operands exp) var))
		((product? exp)
			(deriv-product (operands exp) var))
		(else (error "unknown expressioon type: DERIV" exp))))

(define (make-sum oprs)
	(define (inner-make-sum initial os)
		(cond
			((null? os) 
				(if (= (length initial) 2)
					(cadr initial)
					initial))
			((=number? (car os) 0) (inner-make-sum initial (cdr os)))
			(else (inner-make-sum (append initial (list (car os))) (cdr os)))))
	(inner-make-sum '(+) oprs))

(define (make-product oprs)
	(define (inner-make-product initial os)
		(cond
			((null? os) initial)
			((=number? (car os) 0) 0)
			((=number? (car os) 1) (inner-make-product initial (cdr os)))
			(else (inner-make-product (append initial (list (car os))) (cdr os)))))
	(inner-make-product '(*) oprs))

(define (deriv-sum oprs v)
	(define (deriv-var os)
		(if (null? os)
			null
			(cons (deriv (car os) v) (deriv-var (cdr os)))))
	(make-sum (deriv-var oprs)))

(define (deriv-product oprs v)
	(define (comb os t)
		(if (eq? (car os) t)
			(deriv t v)
			(car os)))
	(define (var os t)
		(if (null? os)
			null
			(cons (comb os t) (var (cdr os) t))))
	(define (inner-deriv-product os cur v)
		(if (null? cur)
			null
			(cons
				(make-product (var os (car cur)))
				(inner-deriv-product os (cdr cur) v))))
	(make-sum(inner-deriv-product oprs oprs v)))

(print (deriv-product '(x y (+ x 3)) 'x)) (newline)