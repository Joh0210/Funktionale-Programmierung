#lang racket

; 1.20
; Schreiben Sie ein Makro infix, mit dem Sie die Addition zweier Zahlen in Infix-Schreibweise errechnen lassen können.
; Anwendungsbeispiel: (infix 1 + 2)

(define-syntax-rule (infix num1 op num2)
  (op num1 num2))

(infix 1 + 2)
(infix -5 - 88)
(infix 2 * 3)