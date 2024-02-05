#lang typed/racket

; 1.1
; Schreiben Sie eine Funktion, die eine natürliche Zahl n erhält und eine Liste der Zahlen von n bis 1 erzeugt.

(: sequence (-> Positive-Exact-Rational (Listof Exact-Rational)))
(define (sequence n)
  (: sequence-inner (-> Exact-Rational (Listof Exact-Rational) (Listof Exact-Rational)))
  (define (sequence-inner n acc)
    (cond
      ((<= n 1) acc)
      (else (sequence-inner (- n 1) (append acc (list(- n 1)))))))

  (sequence-inner n (list n)))



(sequence 10)
(sequence 1)
; (sequence 0)
; (sequence -10)