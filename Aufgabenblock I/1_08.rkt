#lang racket

; 1.8
; Ein Nutzer schreibt sich ein if neu, da es ja cond gibt:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (deleate-all-flies)
  (print "Everything has been deleted!")
  #f)


(new-if (= 0 0) #t (deleate-all-flies))

(if (= 0 0) #t (deleate-all-flies))
