#lang racket

; 1.8
; Ein Nutzer schreibt sich ein if neu, da es ja cond gibt:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (delete-all-flies)
  (print "Everything has been deleted!")
  #f)


(if (= 0 0) #t (delete-all-flies))

(new-if (= 0 0) #t (delete-all-flies))
