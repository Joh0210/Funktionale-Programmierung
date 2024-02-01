#lang racket

; 1.4
; Schreiben Sie eine Funktion my-max, die die höchste Zahl einer Liste von Zahlen ermittelt und nutzen Sie dazu eine fold-Funktion.

(define (my-max lst)
  (define (max-of-two a b)
    (cond
      ((>= a b) a)
      (else b)))

  (cond
    ((or (empty? lst) (not (andmap number? lst))) (error"ungueltige eingabe für die Funktion \"my-max\". Nicht leere Liste an Zahlen benötigt!"))
    (else (foldl max-of-two (first lst) (rest lst)))))

(my-max (list -10))
(my-max (list -10 -100))
(my-max (list -10.4 -100))
(my-max (list (* -10 -10) 2 -100 4 5))
(my-max '())
; (my-max (list "0" "A"))
