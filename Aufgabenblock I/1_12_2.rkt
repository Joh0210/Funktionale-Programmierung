#lang racket

; 1.12.2
; Zeigen Sie an zwei selbst erstellten Beispielen die Nutzung von Funktionen höherer Ordnung auf.

; Ermitteln wie viele Angestellten, aus einer Liste von dem monatlichen Brutto Gehalt der Mitarbeiter, über 4.000 € verdienen:

; Direkte Auswertung:
(length (filter (lambda (a) (cond ((> a 4000) #t) (else #f))) (list 1477 4192 814 457 5245 4000 974 8474)))
(length (filter (lambda (a) (cond ((> a 4000) #t) (else #f))) '()))

; Eingebacken:
(define (count-over-4000 lst)
  (length (filter (lambda (a) (cond ((> a 4000) #t) (else #f))) lst)))

(count-over-4000 (list 1477 4192 814 457 5245 4000 974 8474)) ; 3
(count-over-4000 '()) ; 3