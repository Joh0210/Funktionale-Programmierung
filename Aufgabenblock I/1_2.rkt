#lang racket

; 1.2
; Schreiben Sie eine Funktion, die eine natürliche Zahl n erhält und eine Liste der Zahlen von n bis 1 erzeugt.


(define (my-reverse lst)
  (foldl cons '() lst))


(my-reverse '(1))
(my-reverse '(1 2))
(my-reverse '(1 2 3))
(my-reverse (list 1 2 8 4 5))
(my-reverse '())