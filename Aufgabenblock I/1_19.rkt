#lang typed/racket

; 1.19
; Schreiben Sie ein map und ein foldr in Typed Racket.

; 1
(: my-map (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (my-map f lst)
  (: my-map-inner (All (A B) (-> (-> A B) (Listof A) (Listof B) (Listof B))))
  (define (my-map-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-map-inner f (rest lst) (append acc (list (f (first lst))))))))

  (my-map-inner f lst '()))

; 2
(: my-foldr (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (my-foldr f start lst)
  (: my-foldl-inner (All (A B) (-> (-> A B B) (Listof A) B B)))
  (define (my-foldl-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-foldl-inner f (rest lst) (f (first lst) acc)))))

  (my-foldl-inner f (reverse lst) start))


; Test für my-map
(my-map sqr (list 1 2 3 4 5 6))
(my-map sqr '())

; Test für my-foldr
(my-foldr - 0 '(1 2 3 4))
(foldr - 0 '(1 2 3 4))
(foldl - 0 '(1 2 3 4))
; (foldr cons '() '(1 2))
; (my-foldr cons '() '(1 2))