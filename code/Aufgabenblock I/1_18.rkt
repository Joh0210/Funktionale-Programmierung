#lang typed/racket

; 1.18
; Gegeben sei die folgende Funktion in Typed Racket:

; (: bar (-> (U Integer Boolean String) Integer))
; (define (bar x)
; (cond
; ((number? x) (string-length (number->string x)))
; (else (string-length x))))

; Wenn man diese Funktion versucht zu kompilieren, erhält man eine Fehlermeldung vom Type Checker. Erläutern Sie die Fehlermeldung und korrigieren Sie die Funktion.



(: bar (-> (U Integer Boolean String) Integer))
(define (bar x)
  (cond
    ((number? x) (string-length (number->string x)))
    ((boolean? x) 2) ; string-length("#t) = string-length("#f) = 2
    (else (string-length x))))