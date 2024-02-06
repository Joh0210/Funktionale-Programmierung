#lang racket

; 1.13
; Überführen Sie die Funktion sum in eine endrekursive Fassung. Die Funktion wurde wie folgt definiert:

(define (old-sum term a next b)
  (cond
    ((> a b) 0)
    (else (+ (term a)
             (old-sum term (next a) next b)))))

; new
(define (sum term a next b)
  (define (sum-inner term a next b acc)
    (cond
      ((> a b) acc)
      (else (sum-inner term (next a) next b (+ acc (term a))))))

  (sum-inner term a next b 0))


; Tests
(old-sum sqr 1 add1 0)
(sum sqr 1 add1 0)

(old-sum sqr 1 add1 10)
(sum sqr 1 add1 10)

(old-sum sqr 1 (lambda (a) (+ a 5)) 10)
(sum sqr 1 (lambda (a) (+ a 5)) 10)

(old-sum sqr 1 (lambda (a) (* a 1.23)) 10)
(sum sqr 1 (lambda (a) (* a 1.23)) 10)

(old-sum sqr -1 add1 1)
(sum sqr -1 add1 1)