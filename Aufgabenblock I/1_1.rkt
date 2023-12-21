#lang racket

; 1.1
; Schreiben Sie eine Funktion, die eine natürliche Zahl n erhält und eine Liste der Zahlen von n bis 1 erzeugt.

(define (sequence n)
  (define (sequence-inner n acc)
    (cond
      ((<= n 1) acc)
      (else (sequence-inner (- n 1) (append acc (list(- n 1)))))))
  (cond
    ((or (not (integer? n)) (< n 1)) null)  
    (else (sequence-inner n (list n)))))



(sequence 10)
(sequence 1)
(sequence 5.5)
(sequence 0)
(sequence -10)
(sequence "A")