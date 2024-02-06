#lang racket

; 1.12.1
; Zeigen Sie an zwei selbst erstellten Beispielen die Nutzung von Funktionen höherer Ordnung auf.

; Prüfen, ob eine Liste ein Element enthält:

; Direkte Auswertung:
(ormap (lambda (a) (equal? a 3)) (list 1 2 3 4 5))
(ormap (lambda (a) (equal? a #t)) (list "AB" #t 3 "S"))
(ormap (lambda (a) (equal? a "A")) '())

; Eingebacken:
(define (my-find lst x)
  (ormap (lambda (a) (equal? a x)) lst))


(my-find (list 1 2 3 4 5) 3)
(my-find (list "AB" #t 3 "S") #t)
(my-find '() "A")