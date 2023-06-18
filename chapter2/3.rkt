(define (make-point x y)
	(cons x y))
(define (x-point p)
	(car p))
(define (y-point p)
	(cdr p))

; implements A
; (define (make-rectangle p1 p2)
; 	(cond
; 		((and (< (x-point p1) (x-point p2)) (< (y-point p1) (y-point p2)))
; 			(cons (make-point (x-point p1) (y-point p2)) (make-point (x-point p2) (y-point p1))))
; 		((and (< (x-point p1) (x-point p2)) (> (y-point p1) (y-point p2)))
; 			(cons p1 p2))
; 		((and (> (x-point p1) (x-point p2)) (< (y-point p1) (y-point p2)))
; 			(cons p2 p1))
; 		((and (> (x-point p1) (x-point p2)) (> (y-point p1) (y-point p2)))
; 			(cons (make-point (x-point p2) (y-point p1)) (make-point (x-point p1) (y-point p2))))
; 		(else (error "area is zero"))))
;
; (define (up-left-pt r) (car r))
; (define (up-right-pt r) (make-point (x-point (cdr r)) (y-point (car r))))
; (define (btm-left-pt r) (make-point (x-point (car r)) (y-point (cdr r))))
; (define (btm-right-pt r) (cdr r))

; implements B
(define (make-rectangle p1 p2)
	(cond
		((and (< (x-point p1) (x-point p2)) (< (y-point p1) (y-point p2)))
			(cons (make-point (x-point p1) (y-point p2))
				(cons p2
				(cons p1 (make-point (x-point p2) (y-point p1))))))
		((and (< (x-point p1) (x-point p2)) (> (y-point p1) (y-point p2)))
			(cons p1
				(cons (make-point (x-point p2) (y-point p1))
				(cons (make-point (x-point p1) (y-point p2)) p2))))
		((and (> (x-point p1) (x-point p2)) (< (y-point p1) (y-point p2)))
			(cons p2
				(cons (make-point (x-point p1) (y-point p2))
				(cons (make-point (x-point p2) (y-point p1)) p1))))
		((and (> (x-point p1) (x-point p2)) (> (y-point p1) (y-point p2)))
			(cons (make-point (x-point p2) (y-point p1))
				(cons p1
				(cons p2 (make-point (x-point p1) (y-point p2))))))
		(else (error "area is zero"))))

(define (up-left-pt r) (car r))
(define (up-right-pt r) (car (cdr r)))
(define (btm-left-pt r) (car (cdr (cdr r))))
(define (btm-right-pt r) (cdr (cdr (cdr r))))

(define (length p1 p2)
	(sqrt (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2))))))
(define (area r)
	(* (length (up-left-pt r) (btm-left-pt r)) (length (up-right-pt r) (up-left-pt r))))
(define (parimeter r)
	(+ (* (length (up-left-pt r) (btm-left-pt r)) 2) (* (length (up-right-pt r) (up-left-pt r)) 2)))

(define (display-rectangle r)
	(display (up-left-pt r))
	(display (up-right-pt r)) (newline)
	(display (btm-left-pt r))
	(display (btm-right-pt r)) (newline)
	(display "area: ")
	(display (area r)) (newline)
	(display "parimeter ")
	(display (parimeter r)) (newline)
)

(display "when (4, 7) (8, 1)") (newline)
(display-rectangle (make-rectangle (make-point 4 7) (make-point 8 1)))
