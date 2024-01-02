#lang racket

; 2.8

; Überlegen Sie sich einen Anwendungsfall für Ströme (streams) und illustrieren Ihren Anwendungsfall an einer Implementierung in Racket. 
; Die Implementierung muss nicht voll funktionsfähig sein, es reicht ein Prototyp oder Pseudo-Code.

; Stream Logik:

(define the-empty-stream '())
(define head car)
(define stream-empty? empty?)

(define-syntax stream-cons
  (syntax-rules ()
      ((cons-stream x y)
       (cons x (delay y)))))

(define (tail s) (force (cdr s)))

; Funktion für endlose Menge
; findet das 1. Element des streams für das "bedingung" gilt
; ist die abbruchbedingung erfüllt, wird die suche abgebrochen. (const #f) falls man sich 100% sicher ist, dass element existiert.
(define (findf-endlos steam bedingung abbruchbedingung)
  (cond
    ((abbruchbedingung (head steam)) #f)
    ((bedingung (head steam)) (head steam))
    (else (findf-endlos (tail steam) bedingung abbruchbedingung))))

; endlose Menge
(define (natürliche-zahlen)
  (define (natürliche-zahlen-inner a)
    (stream-cons a (natürliche-zahlen-inner (+ a 1))))
  
  (natürliche-zahlen-inner 1))

; Testfunktion
(define (prim? n)
  (define (iterate i)
    (cond
      ((> (sqr i)  n) #t) 
      ((= (modulo n i) 0) #f)
      (else (iterate (+ i 2)))))

  (cond ((<= n 1) #f) 
        ((= n 2) #t)  
        ((even? n) #f) 
        (else (iterate 3))))

; Erste Primzahl > 100
(findf-endlos (natürliche-zahlen)
              (conjoin prim? (lambda (n) (> n 100)))
              (const #f))


