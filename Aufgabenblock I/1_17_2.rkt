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

; memory-fib:
(define fib
    (let ((table (make-hash)))
      (lambda (n)
        (let ((previously-computed-result (hash-ref table n #f)))
          (or previously-computed-result 
              (let ((result 
                     (cond
                       ((= n 0) 0)
                       ((= n 1) 1)
                       (else (+ (fib (- n 1)) (fib (- n 2)))))))
                (hash-set! table n result)
                result))))))


; fib-stream:

(define (fib-stream)
  (define (fib-stream-inner n)
    (stream-cons (list n (fib n)) (fib-stream-inner (+ n 1))))
  (fib-stream-inner 0))
                            



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
(get-at a 30)

(time ((const "<>") (get-at a 100000)))