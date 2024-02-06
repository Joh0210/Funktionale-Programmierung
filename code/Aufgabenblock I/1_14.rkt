#lang racket

; 1.14
; Schreiben Sie eine Funktion insert-sort, die eine übergebene Liste von Zahlen mittels Sortierens durch Einfügen sortiert und das Ergebnis zurückliefert. Beispiele:

; (insert-sort '(2 4 1 6 4))
; '(1 2 4 4 6)
; (insert-sort '())
; '()

(define DIRECTION <=) ; Possible values: ">="(Maxsort) or "<="(Minsort)

(define (insert-sort lst)
  (define (insert x sorted)
    (cond
      ((empty? sorted) (list x))
      ((DIRECTION x (first sorted)) (cons x sorted))
      (else (cons (first sorted) (insert x (rest sorted))))))

  (define (insertion-sort-inner unsorted sorted)
    (cond
      ((empty? unsorted) sorted)
      (else (insertion-sort-inner (rest unsorted) (insert (first unsorted) sorted)))))

  

(cond
    ((not (andmap number? lst)) null)
    (else (insertion-sort-inner lst '()))))

(insert-sort '())
(insert-sort '("2" 4 6 4))
(insert-sort '(2 4 6 4))
(insert-sort '(5 52 10 3 1))
(insert-sort '(-9 7 5 83 1))