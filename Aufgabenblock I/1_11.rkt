#lang racket

(define (my+ x y) (+ x y))
(define (my- x y) (- x y))

; identisch
(foldl my+ 0 (list 1 2 3 4))
(foldr my+ 0 (list 1 2 3 4))

; verschieden
(foldl my- 0 (list 1 2 3 4))
(foldr my- 0 (list 1 2 3 4))
