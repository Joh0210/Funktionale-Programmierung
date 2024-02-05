#lang racket

; 1.5
; Schreiben Sie ein weiteres my-map unter Benutzung von einer fold-Funktion (entscheiden Sie selbst welches).

(define (my-map f lst)
  (cond
    ((empty? lst) empty)
    (else
     (foldl
      (lambda (a b) (append b (list (f a))))
      (list (f (first lst))) (rest lst)))))

(my-map sqr `())
(my-map sqr (list 2))
(my-map sqr (list 2 1 4 5))