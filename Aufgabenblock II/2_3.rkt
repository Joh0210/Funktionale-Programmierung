#lang racket

; 2.3
; Implementieren Sie eine Funktion mk-mp3-control, die ein Objekt zurückliefert, das die Kontrolleinheit eines MP3-Spielers repräsentiert. (Es sollen aber nicht wirklich Dateien abgespielt werden.)
; Das Objekt soll folgende Informationen speichern/zurückliefern:
; - Eine Liste, der gespeicherten MP3-Dateien (mit Dateiname und Dauer des Stücks), 
; - die Anzahl der Titel
; - der aktuelle Titel, der gerade abgespielt wird oder vorgewählt ist
; - den Abspielstatus, also ob derzeit ein Titel (welcher?) abgespielt wird oder stop, wenn kein Titel abgespielt wird. 

; Folgende Botschaften soll das Objekt verstehen: 
; - laden: Hinzufügen einer neuen MP3-Datei an das Ende der Titelliste, 
; - loeschen: Löschen einer Datei aus der Liste, Übergabe der Nummer des zu löschenden Titels
; - abspielen/stop: Ändern des Abspielstatus, 
; - vor/zurück: Titel erhöhen oder erniedrigen, sind keine Titel vorhanden oder ist das Ende oder 1 erreicht, so wird die Nachricht ignoriert, 
; - unbekannte Nachrichten werden ignoriert.

; Lieder 
(define (mk-lied date-name lied-name dauer)
  (define (zu-string)
    (string-append date-name ": " lied-name " (" dauer "min)"))

  (define (dispatch m)
    (cond
      ((equal? m 'zu-string) zu-string)
      ((equal? m 'datei-name) (lambda () date-name))
      ((equal? m 'lied-name) (lambda () lied-name))
      ((equal? m 'dauer) (lambda () dauer))
      (else (const "Unbekannte Nachricht!"))))
  dispatch)

(define (zu-string lied)
  ((lied 'zu-string)))

(define (datei-name lied)
  ((lied 'datei-name)))

(define (lied-name lied)
  ((lied 'lied-name)))

(define (dauer lied)
  ((lied 'dauer)))

; MP3 Players
(define (mk-mp3-control)
  (define lieder null)
  (define status "stop")
  (define ausgewählt 0)

  (define (ausgewaehlt-name)
    (cond
      ((<= (length lieder) 0)
       "Du hast kein Lied zum abspielen")
      (else
       (lied-name (list-ref lieder ausgewählt)))))

  (define (lieder-liste)
    (map (lambda (l) (zu-string l)) lieder))
  
  (define (abspielen/stop)    
    (cond
      ((<= (length lieder) 0)
       (set! status "stop")
       "Du hast kein Lied zum abspielen")
      ((equal? status "stop")
       (set! status "spielt musik")
       "Die Weidergabe wird fortgesetzt")
      (else
       (set! status "stop")
       "Die Weidergabe wird pausiert")))

  (define (get-status)
    (cond ((equal? status "stop") "stop") (else (string-append "Es wird gespielt: " (ausgewaehlt-name)))))
  
  (define (laden lied)
    (cond
      ; Keine dopplungen 
      ((findf
        (lambda (l) (equal? (datei-name l) (datei-name lied)))
        lieder) (string-append (datei-name lied) " ist schon enthalten"))
      ; lied hinzufügen
      (else (set! lieder (append lieder (list lied)))
            (string-append (datei-name lied) " hinzugefügt"))))

  (define (loeschen n)
    (define (auswahl-anpassen)
      (cond
        ((= (+ ausgewählt 1) n) (string-append " - Ausgewählt ist jetzte: " (ausgewaehlt-name)))
        ((> ausgewählt n) (set! ausgewählt (- ausgewählt 1)) "")
        (else "")))
    
    (define (loeschen-inner lst acc n)
      (cond
        ((empty? lst) acc)
        ((<= n 0) (append acc (rest lst)))
        (else (loeschen-inner (rest lst) (append acc (list (first lst))) (- n 1)))))
    
    (cond
      ((and (>= n 1) (<= n (length lieder)))
       (set! lieder (loeschen-inner lieder null (- n 1)))
       (string-append "Das " (number->string n) ". lied wurde entfernt" (auswahl-anpassen)))
      (else "So viele Lieder gibt es nicht")))

  (define (vor)
    (cond
      ((>= (+ ausgewählt 1) (length lieder)) "Ende erreicht")
      (else
       (set! ausgewählt (+ ausgewählt 1))
       (string-append "Ausgewählt: " (ausgewaehlt-name)))))

  (define (zurück)
    (cond
      ((< (- ausgewählt 1) 0) "Anfang erreicht")
      (else
       (set! ausgewählt (- ausgewählt 1))
       (string-append "Ausgewählt: " (ausgewaehlt-name)))))
  
  (define (dispatch m)
    (cond
      ((equal? m 'lieder-liste) lieder-liste)
      ((equal? m 'lieder-liste-roh) (lambda () lieder))
      ((equal? m 'anzahl) (lambda () (length lieder)))
      ((equal? m 'ausgewaehlt) ausgewaehlt-name)
      ((equal? m 'status) get-status)
      ((equal? m 'abspielen/stop) abspielen/stop)
      ((equal? m 'laden) laden)
      ((equal? m 'loeschen) loeschen)
      ((equal? m 'vor) vor)
      ((equal? m 'zurück) zurück)
      (else (const "Unbekannte Nachricht!"))))
  dispatch)

(define (lieder-liste mp3-control)
  ((mp3-control 'lieder-liste)))

(define (lieder-liste-roh mp3-control)
  ((mp3-control 'lieder-liste-roh)))

(define (anzahl mp3-control)
  ((mp3-control 'anzahl)))

(define (ausgewaehlt mp3-control)
  ((mp3-control 'ausgewaehlt)))

(define (status mp3-control)
  ((mp3-control 'status)))

(define (abspielen/stop mp3-control)
  ((mp3-control 'abspielen/stop)))

(define (laden mp3-control lied)
  ((mp3-control 'laden) lied))

(define (loeschen mp3-control nummer)
  ((mp3-control 'loeschen) nummer))

(define (vor mp3-control)
  ((mp3-control 'vor)))

(define (zurück mp3-control)
  ((mp3-control 'zurück)))

(define lied1 (mk-lied "test1.mp3" "My-Song1" "3:21"))
(define lied2 (mk-lied "test2.mp3" "My-Song2" "3:22"))
(define lied3 (mk-lied "test3.mp3" "My-Song3" "3:23"))
(define lied4 (mk-lied "test4.mp3" "My-Song4" "3:24"))
(define lied5 (mk-lied "test5.mp3" "My-Song5" "3:25"))
(zu-string lied1)
(lied-name lied1)

(define mp3-player1 (mk-mp3-control))
(ausgewaehlt mp3-player1)
(anzahl mp3-player1)
(lieder-liste mp3-player1)
(laden mp3-player1 lied1)
(laden mp3-player1 lied2)
(laden mp3-player1 lied3)
(laden mp3-player1 lied2)
(laden mp3-player1 lied4)
(laden mp3-player1 lied5)
(anzahl mp3-player1)
(lieder-liste mp3-player1)

(loeschen mp3-player1 2)
(loeschen mp3-player1 1)
(loeschen mp3-player1 10)
(loeschen mp3-player1 -1)
(anzahl mp3-player1)
(lieder-liste mp3-player1)
(status mp3-player1)
(abspielen/stop mp3-player1)
(status mp3-player1)
(ausgewaehlt mp3-player1)