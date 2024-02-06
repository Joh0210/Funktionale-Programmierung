#lang racket

; 2.6: Verzögerte Auswertung: thunk

; Überlegen Sie ein Beispiel, in dem verzögerte Auswertung sinnvoll sein kann.
; Wie würden Sie die verzögerte Auswertung umsetzen? Begründen Sie Ihr Vorgehen.

(define (entscheidung kondition thunk)
  (cond
    (kondition (thunk))
    (else "wird nicht benötigt")))

(define aufwendige-berechnung (lambda ()
  (sleep 3)
  "wird benötigt"))

(entscheidung #t aufwendige-berechnung) ; Es wird 3 sec gewartet
(entscheidung #t aufwendige-berechnung) ; Es wird 3 sec gewartet
(entscheidung #f aufwendige-berechnung) ; Es wird nicht gewartet