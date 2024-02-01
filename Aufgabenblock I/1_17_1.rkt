#lang racket

; 1.17
; Implementierten Sie eine Funktion fib-stream,
; die einen Strom von zwei-elementigen Listen (n (fib(n))) erzeugt, wobei n die natürlichen Zahlen durchläuft.
; Die Funktion soll nicht jede Fibonacci-Zahl unabhängig berechnen.

;> (define a (fib-stream))
;> a
;((0 0) . #<promise>)
;> (tail a)
;((1 1) . #<promise>)
;> (tail (tail a))
;((2 1) . #<promise>)
;> (tail (tail (tail a)))
;((3 2) . #<promise>)
;> (tail (tail (tail (tail a))))
;((4 3) . #<promise>)
;> (tail (tail (tail (tail (tail a)))))
;((5 5) . #<promise>)

; Stream Logik:
(define the-empty-stream '())
(define head car)
(define stream-empty? empty?)

(define-syntax stream-cons
  (syntax-rules ()
      ((cons-stream x y)
       (cons x (delay y)))))

(define (tail s) (force (cdr s)))
  
; fib-stream:
(define (fib-stream)
  (define (interation s1 s2)
    (cond
      ((stream-empty? s1) s2)
      ((stream-empty? s2) s1)
      (else (stream-cons (list
                          (+ (first (head s2)) 1)
                          (+ (second (head s1)) (second (head s2))))
                         (interation
                          (tail s1)
                          (tail s2))))))
  
  (stream-cons (list 0 0)
               (stream-cons (list 1 1)
                            (interation (fib-stream) (tail (fib-stream))))))



; für Tests:
(define (get-at s n)
  (cond
    ((<= n 0) (head s))
    (else (get-at (tail s) (- n 1)))))


(define a (fib-stream))
a
(tail a)
(tail (tail a))
(tail (tail (tail a)))
(tail (tail (tail (tail a))))
(tail (tail (tail (tail (tail a)))))
(tail (tail (tail (tail (tail (tail a))))))
(tail (tail (tail (tail (tail (tail (tail a)))))))
(tail (tail (tail (tail (tail (tail (tail (tail a))))))))
(tail (tail (tail (tail (tail (tail (tail (tail (tail a)))))))))
(get-at a 10)
; zu Speicherintensiv:
; (get-at a 30) 

; (time ((const "<>") (get-at a 100)))