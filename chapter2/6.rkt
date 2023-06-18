(define inc (lambda (f) (+ f 1)))
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
(define (add-0 n) (lambda (f) (lambda (x) ((n f) x))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
; 
(display "zero ")
(print ((zero inc) 0)) (newline)
(print (((add-1 zero) inc) 0)) (newline)
(print (((add-0 zero) inc) 0)) (newline)
(print (((add-0 one) inc) 0)) (newline)
(display "one ")
(print ((one inc) 0)) (newline)
(display "two ")
(print ((two inc) 0)) (newline)

; one: add-1 zero
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f x)))

; two: add-1 one
; (add-1 (lambda (f) (lambda (f) (lambda (x) (f x)))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define (add n m) (lambda (f) (lambda (x) ((m f) ((n f) x)))))
(define (mul n m) (lambda (f) (lambda (x) ((m (n f)) x))))
(define (pow n m) (lambda (f) (lambda (x) (((m n) f) x))))

(display "add two three: ")
(print (((add two three) inc) 0)) (newline)
(display "mul two three: ")
(print (((mul two three) inc) 0)) (newline)
(display "pow two three: ")
(print (((pow two three) inc) 0)) (newline)
