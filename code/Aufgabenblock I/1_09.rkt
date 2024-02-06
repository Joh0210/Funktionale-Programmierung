#lang racket

; 1.9
; Schreiben Sie endrekursive Fassungen von zwei beliebigen Funktionen, die auf Listen operieren (bspw. eine Funktion, die jedes Element einer Liste quadriert).

; 1
(define (my-map f lst)
  (define (my-map-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-map-inner f (rest lst) (append acc (list (f (first lst))))))))

  (my-map-inner f lst '()))

; 2
(define (my-foldl f start lst)
    (cond
      ((empty? lst) start) ; acc
      (else (my-foldl f (f (first lst) start) (rest lst)))))

; Test für my-map
(my-map sqr (list 1 2 3 4 5 6))
(my-map sqr '())

; Test für my-foldl
(my-foldl cons '() '(1 6 3 8 33))
(my-foldl cons '() '(1 2))
(my-foldl cons '() '(1 2 3))