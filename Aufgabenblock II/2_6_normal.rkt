#lang racket

; 2.6: Normale Auswertung

; Überlegen Sie ein Beispiel, in dem verzögerte Auswertung sinnvoll sein kann.
; Wie würden Sie die verzögerte Auswertung umsetzen? Begründen Sie Ihr Vorgehen.

(define (entscheidung kondition res)
  (cond
    (kondition res)
    (else "wird nicht benötigt")))

(define (aufwendige-berechnung)
  (sleep 3)
  "wird benötigt")

(entscheidung #t (aufwendige-berechnung)) ; Es wird 3 sec gewartet
(entscheidung #t (aufwendige-berechnung)) ; Es wird 3 sec gewartet
(entscheidung #f (aufwendige-berechnung)) ; Es wird 3 sec gewartet