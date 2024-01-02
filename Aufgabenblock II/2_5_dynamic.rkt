#lang racket

; 2.5 Dynamic

(define x 10)

(define (mach-was)
  (cond
    (x "Zweig 1")
    (else "Zweig 2")))

; Aus einer Library kopiert: 
(define (super-wichtige-rechnung)
  ; + 100 Zeilen Code
  (set! x #f) ; eigentlich define. x würde in den nachvollgenden Zeilen gebraucht werden
  ; + 70 Zeilen Code
  (define y "test") ; y würde in den nachvollgenden Zeilen gebraucht werden
  ; + 20 Zeilen Code
  (string-append "Super wichtige Rechnung!"))

(mach-was) ; -> "Zweig 1"
; +30 Zeilen
(super-wichtige-rechnung)
; +14 Zeilen
(mach-was) ; -> "Zweig 2"
