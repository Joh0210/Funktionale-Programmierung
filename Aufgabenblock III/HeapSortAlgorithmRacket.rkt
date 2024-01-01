#lang racket

; Generische Variante von "get-steps" um alle Datentypen zu unterstützen
; Es muss eine Funktion "translate-comparator" übergeben werden welche die Number-Comparators (>= <= > < =)
; für die jeweiligen Datentypen übersetzt
; Warnung: Inkonsistente Typen werden hier theoretisch unterstützt, jedoch muss die "translate-comparator"-Funktion damit klar kommen.
(define (get-steps-generic lst is-min-heap translate-comparator)

  ; Ermittelt die Position einer Parent-Node einer Node in einem binären Heap
  ; -1 falls die Node die Root Node ist 
  (define (get-parent-pos child-pos)
    (cond
      (( <= child-pos 0) -1)
      (else (inexact->exact(floor (/ (- child-pos 1) 2.0))))))

  ; Ermittelt ob die Heap-Condition des Binären Heaps gebrochen ist
  ; #f falls die Heap-Condtion nicht gebrochen ist
  ; Position der Child-Node an der die Heap-Condtion nicht erfüllt ist
  ; Sollte es 2 Kandidaten geben, die mit dem Parent getauscht werden können,
  ; wird der kleinere (minheap)/größere (maxheap)genommen.
  (define (heapify-broken? lst)
    (define (broken-at? lst pos)
      (cond
        ((empty? lst) #f)
        ((<= pos 0) #f)
        (((translate-comparator (cond (is-min-heap >=) (else <=))) (list-ref lst pos) (list-ref lst (get-parent-pos pos)))
         (broken-at? lst (- pos 1)))
        ; Fehlerfall:
        ((and (odd? pos) (= pos (- (length lst) 1))) pos)
        ; wähle das kleinere (minheap) der beiden Kinder (größere für dem Maxheap) 
        (((translate-comparator (cond (is-min-heap <=) (else >=)))
                  (list-ref lst pos) (list-ref lst ((cond ((odd? pos) +) (else -)) pos 1))) pos)
        ; die Position aller Linken Child-Nodes sind immer Ungerade
        (else ((cond ((odd? pos) +) (else -)) pos 1))))
    (broken-at? lst (- (length lst) 1)))

  ; Tauscht 2 Elemente einer Liste
  (define (swap pos1 pos2 lst)
    (define (swap-inner1 pre value1 pos2 lst)
      (cond
        ((and (<= pos2 0) (empty? lst)) (append pre (list value1)))
        ((<= pos2 0) (append pre (list value1) (rest lst)))
        (else (swap-inner1 (append pre (list (first lst))) value1 (- pos2 1) (rest lst)))
        ))

    (define (swap-inner2 pre pos1 pos2 lst)
      (cond
        ((<= pos1 0) (swap-inner1 (append pre (list (list-ref lst pos2))) (first lst) (- pos2 1) (rest lst)))
        (else (swap-inner2 (append pre (list (first lst))) (- pos1 1) (- pos2 1) (rest lst)))))

    (cond
      ((< pos1 0)(error "IndexOutOfBoundsException"))
      ((< pos2 0)(error "IndexOutOfBoundsException"))
      ((< pos1 pos2)(swap-inner2 null pos1 pos2 lst))
      ((> pos1 pos2)(swap-inner2 null pos2 pos1 lst))
      (else lst)))

  ; Wenn die Heap-Condition der liste gebrochen wurde,
  ; fürt diese Funktion genau einen Schritt aus um sie stückweise wieder her zu stellen.
  (define (heapify-step lst)
    (swap (get-parent-pos (heapify-broken? lst)) (heapify-broken? lst) lst))

  ; Baut den Heap Schrittweise auf
  (define (insert-step current ret lst)
    (cond
      ((heapify-broken? current) (insert-step (heapify-step current) (append ret (list current)) lst))
      ((empty? lst) (append ret (list current)))
      (else (insert-step (append current (list (first lst))) (cond ((empty? current) ret)(else (append ret (list current)))) (rest lst)))))

  ; Baut den Heap Schrittweise ab.
  ; Nutzt als ausgabgspunkt den letzen Schritt der insert-steps
  (define (delete-step insert-steps)
    (define (delete-step-inner current ret)
      (cond
        ((heapify-broken? current) (delete-step-inner (heapify-step current) (append ret (list current))))
        ((<= (length current) 1) (append ret (list current)))
        (else (delete-step-inner (cons (last current) (reverse (rest (reverse (rest current))))) (append ret (list current))))))

    (delete-step-inner (cons (last (last insert-steps)) (reverse (rest (reverse (rest (last insert-steps)))))) insert-steps))

  ; Interner Aufruf der Funktion
  (cond
    ((empty? lst) null) ; kann nicht sortiert werden
    ((<= (length lst) 1) (list lst)) ; ist bereits sortiert 
    (else (delete-step (insert-step null null lst))))
  )


; Die Standartfunktion unterstütz ausschließlich Numbers und Strings
; Alternative Datentypen können jedoch mit "get-steps-generic" verarbeitet werden.
(define (get-steps lst is-min-heap)

  ; Erstellt eine Funktion die number-comparator zu string-comparator überfürt,
  ; falls es sich bei der Liste um Strings handelt.
  (define (get-comparator-translater)
    (cond
      ((number? (first lst)) (lambda (comparator) comparator))
      ((string? (first lst))
       (lambda (comparator) (cond
         ((equal? comparator >=) string>=?)
         ((equal? comparator <=) string<=?)
         ((equal? comparator >) string>?)
         ((equal? comparator <) string<?)
         (else equal?))))
      (else (error "TypeException: Diese Funktion untersützt nur Listen aus nur Numbers oder nur Strings.\nVerwende statdessen die Funktion \"get-steps-generic\""))))

  (cond
    ((empty? lst) null) ; kann nicht sortiert werden
    ((<= (length lst) 1) (list lst)) ; ist bereits sortiert 
    ((nor (andmap number? lst) (andmap string? lst))
     (error "TypeException: Diese Funktion untersützt nur Listen aus nur Numbers oder nur Strings.\nVerwende statdessen die Funktion \"get-steps-generic\""))
    (else (get-steps-generic lst is-min-heap (get-comparator-translater)))))
  
  



(get-steps null #t)
(get-steps (list 7) #t)
(get-steps (list 7 2) #t)
(get-steps (list 6 3 5 2 4 1 7) #t)
(get-steps (list 7 2 3 4 5) #t)
(get-steps (list 7 2 3 4 5) #f)
(get-steps (list 1 1 1 1 1 1) #t)
(get-steps (list 1 1 1 1 1 1) #f)
(get-steps (list "T" "T" "T" "T" "T" "T") #t)
(get-steps (list "T" "T" "T" "T" "T" "T") #f)
(get-steps (list "Hallo" "Test" "A" "B" "K" "X") #t)
(get-steps (list "Hallo" "Test" "A" "B" "K" "X") #f)
(get-steps (list "Hallo" "Test" "A" "B" "K" "X") #f)
(get-steps-generic (list
                    (list "Hallo" "Test" "A" "B" "K" "X")
                    (list "E" "A" "AX")
                    (list "E" "A" "A" "AX")
                    (list 1 1 1 1 1 1))
                   #f (lambda (comparator) (lambda (a b) (comparator (length a) (length b)))))
; (get-steps (list (cons 1 2) (cons 1 2) (cons 1 2) (cons 1 2)) #f)
; (get-steps (list 1 1 1 1 1 "A") #f)
