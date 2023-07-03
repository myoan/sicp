#lang racket

(print (list 'a 'b 'c)) (newline)
(print (list (list 'george))) (newline)
(print (cdr '((x1 x2) (y1 y2)))) (newline)
(print (cadr '((x1 x2) (y1 y2)))) (newline)
(print (pair? (car '(a short list)))) (newline)
(print (memq 'red '((red shoes) (blue socks)))) (newline)
(print (memq 'red '(red shoes blue socks))) (newline)