#lang racket

; 1.12.1
; Zeigen Sie an zwei selbst erstellten Beispielen die Nutzung von Funktionen höherer Ordnung auf.

; Man hat eine Liste an Messdaten eines Sensors. Da der Sensor manchmal werte über 100 ausgibt,
; welche das System nicht weiter verarbeiten kann, müssen alle Werte über 100 auf 100 gesetzt werden: 

(define (adjust_limit lst)
  (map (lambda (a) (cond ((<= a 100) a) (else 100))) lst))

(adjust_limit (list 39 192 10 23 100 91 894 29))