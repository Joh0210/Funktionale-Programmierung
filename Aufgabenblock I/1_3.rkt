#lang racket

; 1.3
; Schreiben Sie eine Funktion my-find, die eine Liste lst und ein Argument x erhält und mittels equal? prüft,
; ob das Element in der Liste enthalten ist. Die Funktion soll #t antworten, wenn dem so ist, oder mit #f wenn nicht.


(define (my-find lst x)
  (ormap (lambda (a) (equal? a x)) lst))


(my-find (list 1 2 3 4 5) 3)
(my-find (list "AB" #t 3 "S") #t)
(my-find '() "A")