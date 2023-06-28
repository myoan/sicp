#lang racket

(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

; a

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length br) (car br))
(define (branch-structure br) (car (cdr br)))

; b

(define (total-weight m)
	(cond
		; 両方おもり
		((and
			(number? (branch-structure (left-branch m)))
			(number? (branch-structure (right-branch m))))
				(+
					(branch-structure (left-branch m))
					(branch-structure (right-branch m))))
		; 左がおもり
		((number? (branch-structure (left-branch m)))
				(+
					(branch-structure (left-branch m))
					(total-weight (branch-structure (right-branch m)))))
		; 右がおもり
		((number? (branch-structure (right-branch m)))
				(+
					(total-weight (branch-structure (left-branch m)))
					(branch-structure (right-branch m))))
		(else (+
				(total-weight (branch-structure (left-branch m)))
				(total-weight (branch-structure (right-branch m)))))))

(define mb (make-mobile
	(make-branch 3 (make-mobile (make-branch 2 5) (make-branch 4 2)))
	(make-branch 2 7)))

(display "total-weight mobile") (newline)
(print (total-weight mb)) (newline)

; c

; balanced?
; (= (torque (left m)) (torque (right m)))
; torque m
; if m.left is not struct and m.right is not struct:
; 	(= m.left.len * m.left.weight m.right.len * m.right.weight)
; else if m.left is not struct:
;	if !(baranced? m.left) #f
; 	m.left.len * m.left.weight + m.right.len * (weight m.right)
; else if m.right is not struct:
;	if !(baranced? m.right) #f
; 	m.right * m.right + m.left * (weight m.left)
; else:
; 	(= m.left.len * (weight m.left) m.right.len * (weight m.right))

(define (left-structure m) (branch-structure (left-branch m)))
(define (right-structure m) (branch-structure (right-branch m)))

(define (balanced? m)
	; (print m) (newline)
	(define (torque br)
		; (display "torque") (newline)
		; (print br) (newline)
		(if (number? (branch-structure br))
			(* (branch-length br) (branch-structure br))
			(* (branch-length br) (total-weight (branch-structure br)))))
	(cond
		; 両方がおもり
		((and
			(number? (left-structure m))
			(number? (right-structure m)))
				; (display "both are weight") (newline)
				(= (torque (left-branch m)) (torque (right-branch m))))
		; 左がおもり
		((number? (left-structure m))
			; (display "left is weight") (newline)
			(and
				(balanced? (right-structure m))
				(= (torque (left-branch m)) (torque (right-branch m)))))
		; 右がおもり
		((number? (right-structure m))
			; (display "right is weight") (newline)
			(and
				(balanced? (left-structure m))
				(= (torque (left-branch m)) (torque (right-branch m)))))
		(else
			; (display "else") (newline)
			(and
				(balanced? (right-structure m))
				(balanced? (left-structure m))
				(= (torque (left-branch m)) (torque (right-branch m)))))))


(display "baranced? mobile") (newline)
(print (balanced? mb)) (newline)

(define mb2 (make-mobile
	(make-branch 3 (make-mobile (make-branch 2 6) (make-branch 4 3)))
	(make-branch 9 3)))

(display "baranced? balanced-mobile") (newline)
(print (balanced? mb2)) (newline)

; d

; % diff -u chapter2/29.rkt chapter2/29-d.rkt
; --- chapter2/29.rkt     2023-06-28 09:53:04
; +++ chapter2/29-d.rkt   2023-06-28 09:57:22
; @@ -1,17 +1,17 @@
;  #lang racket
;  
;  (define (make-mobile left right)
; -       (list left right))
; +       (cons left right))
;  
;  (define (make-branch length structure)
; -       (list length structure))
; +       (cons length structure))
;  
;  ; a
;  
;  (define (left-branch m) (car m))
; -(define (right-branch m) (car (cdr m)))
; +(define (right-branch m) (cdr m))
;  (define (branch-length br) (car br))
; -(define (branch-structure br) (car (cdr br)))
; +(define (branch-structure br) (cdr br))