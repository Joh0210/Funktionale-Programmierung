#lang racket

; 1.12.2
; Zeigen Sie an zwei selbst erstellten Beispielen die Nutzung von Funktionen höherer Ordnung auf.

; Man hat eine Liste von dem monatlichen Brutto Gehalt der Mitarbeiter, und will herausfinden wie viele über 4.000 € verdienen:

(define (count-over-4000 lst)
  (length (filter (lambda (a) (cond ((> a 4000) #t) (else #f))) lst)))

(count-over-4000 (list 1477 4192 814 457 5245 4000 974 8474)) ; 3