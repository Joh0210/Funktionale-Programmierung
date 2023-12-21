#lang racket

; 1.6
; Schreiben Sie eine Funktion repeated, die eine Funktion f und eine natürliche Zahl n erhält und dann f n-mal ausführt. Beispiel:
; ((repeated sqr 2) 3) -> 81 
; ((repeated add1 10) 1) -> 11

(define (repeated f n)
  (cond
    ((or (not (integer? n)) (<= n 1)) f) 
    (else (compose f (repeated f (- n 1))))))

((repeated sqr 2) 3)
((repeated add1 10) 1)
((repeated sqr 0) 3)
((repeated sqr -10) 3)
((repeated sqr "A") 3)