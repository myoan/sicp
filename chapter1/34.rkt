(define (square x) (* x x))
(define (f g) (g 2))

(display "square ")
(display (f square))
(newline)

(display (f f))
