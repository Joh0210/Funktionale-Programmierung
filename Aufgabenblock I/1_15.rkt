#lang racket

; 1.15
; Schreiben Sie eine Funktion flatten, die eine beliebig tief verschachtelte Listen-Struktur in eine Liste "glättet":

; (flatten '((a) b (c (d) e) ()))
; '(a b c d e)


(define (flatten lst)
  (define (flatten-by-1 x lst)
    (cond
      ((pair? x) (append x lst))
      ((empty? x) lst)
      (else (cons x lst))
    ))

  (cond
    ((ormap pair? lst) (flatten (foldr flatten-by-1 null lst)))
    (else lst)))
  

(flatten '((a) b (c (d) e) ()))
(flatten (list(list(list(list "A") "A")) "A"))

