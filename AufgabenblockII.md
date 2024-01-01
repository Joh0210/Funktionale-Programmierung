# Aufgabenblock II

## Aufgabe 1 -> Todo: Ergänzen?
Wenn Sie eine neue Sprache erlernen, erwarten Sie eher `applicative order` oder `normal order`? Zeigen Sie die Vorzüge Ihrer Erwartung auf. Welche Auswertungsstrategie ist für Sie natürlicher? Begründen Sie Ihre Antwort. (Ergänzen Sie Ihre Antwort im Laufe des Semester ggf. noch einmal, nachdem Sie den Lambda-Kalkül kennengelern haben.)

### Wiederholung
Hierfür ist es wichtig die beiden Begriffe klar einzuordnen:
- **Applicative Order** wertet die Argumente einer Funktion vor der Anwendung der Funktion selbst aus. Das entspricht einer "normalen" Reihenfolge, in der zuerst die Teile und dann das Ganze betrachtet wird.
- **Normal Order/Lazy Evaluation** hingegen wertet die Argumente erst dann aus, wenn sie tatsächlich benötigt werden. Das entspricht einer "faulen" Reihenfolge, bei der nur das ausgewertet wird, was unbedingt erforderlich ist.

### Antwort
Diese Frage lässt sich für mich nicht pauschal beantworten, sondern ist abhängig von dem gewählten Paradigma.

#### Imperativ und Objektorientiert
Erwartung von **Applicative Order**.

Bei der imperativen und objektorientierten Programmierung wird oft gebrauch von Zustandsänderungen, Objektanpassungen und allgemeinen Seiteneffekte in Funktionen, Prozeduren und Methoden gemacht.
<br>Aufgrund von Übersichtlichkeit sollten Funktionen und Prozeduren Seiteneffekte zwar primär vermeiden, jedoch sind sie für das Paradigma erlaubt und auch vorgesehen.

Hierfür ist es entsprechend wichtig, das Argument einer Funktion bereits vollständig ausgewertet sind, bevor der Funktionsrumpf ausgewertet wird, da sonst potenziell Seiteneffekte die Argumente beeinflussen könnten.

Beispiel:
```java 
public class Main {
    public static void main(String[] args){
        MyInt myTestInt = new MyInt(1);

        printOriginalAndPlus1(myTestInt, myTestInt.getValue());
    }

    public static void printOriginalAndPlus1(MyInt myInt, int original){
        myInt.add(1);   //Seiteneffekt
        System.out.println("Plus1: " + myInt.getValue());
        System.out.println("Original: " + original);
    }

    private static class MyInt{
        // Nicht relevant
        // siehe 2_1_Objektorientiert.java
    }
}
```

- **Applicative Order**: Hier wird das erwartete Ergebnis ausgegeben:
    ```
    Plus1: 2
    Original: 1
    ```
- **Normal Order/Lazy Evaluation**: Das Argument `original` wird quasi innerhalb des Funktionsrumpfes durch `myTestInt.getValue()` ersetzt, 
wodurch es durch den Seiteneffekt `myInt.add(1)` ebenfalls beinflusst iwrd und es zu folgender Ausgabe kommt
    ```
    Plus1: 2
    Original: 2
    ```

Auch wenn das Beispiel sehr künstlich ist, zeigt es das Problem welches **Normal Order/Lazy Evaluation** in imperativen und objektorientierten Programmiersprachen bringen würde.

Entsprechen zu erwarten ist **Applicative Order**, jedoch muss der Entwickler hierbei auf Seiteneffekte, Exception-Handling, etc. bereits bei Aufruf der Funktion achten.

#### Funktional
Erwartung von **Normal Order/Lazy Evaluation**.

Funktionale Programmiersprachen unterstützen zwar auch teilweise Objektmanipulation und Zustandsänderungen, jedoch beschreibt das grundlegende Paradigma, dass das Ergebnis einer Funktion nur von den Eingabedaten abhängig ist. 
Somit sollte der Wert der Argumente also unabhängig davon sein, an welcher Stelle sie im Programmcode aufgerufen werden. 

Funktionen die doch Seiteneffekte haben, z.B. das Löschen aller Dateien einer Datenbank, sollen in der Regel nur unter gewissen Konditionen aufgerufen werden.
Damit diese Funktionen als Argument übergeben werden können und nicht direkt bei dem Funktionsaufruf ausgewertet werden, ist also **Normal Order/Lazy Evaluation** zu erwarten.

Auf diese Weise lassen sich auch Ressourcen Sparen, da wenn ein Argument nicht benötigt wird, auch nicht berechnet wird. 
Hierbei wird **Lazy Evaluation** eher statt **Normal Order** erwartet, da bei einer **Lazy Evaluation** das Argument maximal ein mal berechnet wird, und so bei erneuter Abfrage im Funktionsrumpf nicht erneut berechnet werden muss, sondern den entsprechenden wert übergeben bekommt. Bei **Normal Order** würde diese Auswertung der Funktion bei jeder Abfrage des Arguments stattfinden.   

#### Logisch
Aufgrund von mangelnder Erfahrung mit dem Paradigma, erwarte ich keine der Auswertungsstrategien, sondern setze mich erst mit dem Paradigma auseinander.

#### Welche Auswertungsstrategie ist für mich natürlicher?
Da ich primär mit objektorientierten Sprachen gearbeitet habe, kommt mir **Applicative Order** natürlicher vor. 
Jedoch kann ich mir vorstellen, dass es anders wäre, wenn ich primär mit Sprachen gearbeitet hätte, welche **Normal Order/Lazy Evaluation** verwenden.

## Aufgabe 2
Gegeben folgende Racket-Code, der ein simples Bankkonto realisieren soll. Es soll möglich sein, ein neues Bankkonte einzurichten und Geld ein- und auszuzahlen.
```
(define (make-account money)
  (lambda (movement)
    (set! money (+ money movement))
    money))

(define a (make-account 10))
(define b (make-account 100))
```

Und gegeben die folgende Interaktion mit der Racket-REPL:
```
> a
#<procedure>
> b
#<procedure>
> (a 10)
20
> (b 10)
110
> (b 10)
120
```
Erläutern Sie anhand der Interaktion und des Programmcodes, wie in Racket Abschlussobjekte und zustandsorientierte Programmierung genutzt werden können, 
um Eigenschaften objektorientierter Programmierung umzusetzen. 
Sehen Sie Grenzen in der Umsetzung objektorientierter Programmierung in Racket? Wenn ja, welche und warum?

### Umsetzung
Racket selbst ist zwar eine funktionale Sprache, jedoch ermöglicht es durch Abschlussobjekte und Zustandsorientierung 
die implementierung von objektorientierten Konzepten.

Abschlussobjekte (Closures) sind Funktionen, mit einem Verweis auf ihre Definitionsumgebung. 
Also ist das "Objekt", dass durch die Funktion `(make-account 10)` erstellt wird also eigentlich auch nur eine Funktion, was auch in der REPL ausgabe sichtbar ist:
```
> a
#<procedure>
```

Da diese Funktion Zugriff auf ihre Definitionsumgebung hat, können dort Daten hinterlegt werden, welche von dem "Abschlussobjekte" genutzt werden können. 
Diese Daten repräsentieren also die Attribute des Objektes. In diesem Beispiel hat "Objekte" die durch `make-account` erstellt werden nur ein Attribut `money`.

Abschlussobjekte haben zusätzlich auch noch Funktionen, also Methoden, welche das Objekt ausführen kann. 
In diesem Fall haben die Objekte nur eine einzige Funktion, welche einen gewissen Betrag auf das Konto überweisen kann, oder abheben kann. 
Da es nur die eine Funktion gibt, ist das die Funktion, welche das Objekt darstellt. 

Möchte man die Funktion also mit einem Objekt aufrufen, nutzt man es als Funktion des Klammerausdrucks und den Parameter (`movement`) als 1. Argument
```
; 10 (€) einzalen:
(a 10)

; 10 (€) abheben:
(a -10)

; Kontostand prüfen:
(a 0)
```

Für gewöhnlich unterstütze ein Abschlussobjekt jedoch mehrere Funktionen. In dem Fall werden diese innerhalb der erzeugenden Funktion definiert. 
Zusätzlich wird noch eine Funktion definiert, welche als Argument eine "Message" übergeben bekommt, 
anhand derer die richtige Funktion in einer Sprungtabelle ausgewählt und mit den restlichen Parametern aufgerufen wird. 
Diese Auswahlfunktion ist dann die Funktion, welche das Objekt darstellt.

```
(define (make-account)
  (define money 0)  

  (define (withdraw amount)
    (cond ((>= money amount)
           (set! money (- money amount))
           money)
          (else "Guthaben nicht ausreichend!")))

  (define (deposit amount)
    (set! money (+ money amount))
    money)

  (define (balance)
    money)

  (define (dispatch m)
    (cond
      ((equal? m 'withdraw) withdraw)
      ((equal? m 'deposit) deposit)
      ((equal? m 'balance) balance)
      (else "Unbekannte Nachricht!")))

  dispatch)
  
; Erzeugung: 
(define acc1 (make-account))

; Einzahlen:
((acc1 'deposit) 100)
```

Auf diese Weise gibt es gewissermaßen auch Polymorphie oder sogenanntes "Duck-Typing" (wie in Python),
bei dem es der Funktion egal ist, um welche art von Objekt es sich handelt, solange es die eingegebene Funktion unterstützt.
"If it walks like a duck, swims like a duck, and quacks like a duck, then it probably is a duck." [Ursprung der Redewendung ist nicht eindeutig auf eine bestimmte Person zurückzuführen]

Abschlussobjekte alleine würden jedoch nicht ausreichen, um Objektorientierung nachstellen zu können, denn sie können erstmal nur Nachrichten empfangen, Nachrichten an andere Objekte senden, Auskunft über den eigenen Zustand geben und neue Objekte erzeugen.
Erst mit der Zustandsorientierung können die Attribute der Objekte manipuliert werden. Mit `set!` lassen sich Funktionen implementieren, welche die Attribute des Objektes anpassen könne, um so den Zustand des Objektes zu manipulieren.
<br>In dem gegebenen Beispiel findet z.B. eine Einzahlung statt, wodurch der die Variable, welche den Kontostand repräsentiert, verändert wird. 
```
> (b 10)
110
> (b 10)
120
```

### Grenzen und Schwächen
- **Vererbung**: Mir ist keine möglichkeit eingefallen, mit der auf Vererbung mit Abschlussobjekte ohne größere Umwege umsetzbar wäre.
<br>Ich glaube es wäre möglich das Parent-Objekt, als Attribut im Child-Objekt zu speichern, und unbekannte Massages des Childes an den Parent weiterzuleiten, jedoch ist das verhältnismäßig aufwendig.
- **Typprüfung**: Objektorientierte Programmiersprachen, wie Java, nutzen in der regel ein statisches Typsystem welches auch selbsterstellte Klassen als Typ wahrnimmt. 
Die einzige Typprüfung, die hier jedoch existiert, ist das, oben erwähnte "Duck-Typing", was dazu führt das potenziell komplett falsche Objekte einer Funktion übergeben werden können, 
potenziell ohne das es zu einem tatsächlichen Error kommt, sondern einfach nur falsche Ausgaben stattfinden. Jedoch haben auch andere Sprachen, wie z.B. Python, die selbe gefahr.
<br>Bei dem Versuch Abschlussobjekte mit TypedRacket zu verwenden kam es zu einigen Komplikationen, weswegen hier nicht so viel dazu gesagt werden kann, 
außer dass es TypedRacket mit Abschlussobjekten sehr kompliziert werden kann.
- **Funktionales Paradigma**: Racket selbst ist eine funktionale Sprache, was sich auch bei der erzeugung der Objekte wieder spiegelt. 
Es ist zwar möglich, jedoch deutlich komplexer als in anderen Sprachen. 

Zusätzlich sollte erwähnt werden, wenn man das funktionale Paradigma nutzen möchte, ist es oft sinnvoll die Nutzung von Objekten zu minimieren, da vor allem Seiteneffekte dem funktionalen Paradigma wiederstreben.
<br>An manchen stellen kann es Sinnvoll sein, objektorientierte Struktureden in dem funktionalen Paradigma umzusetzen, um z.B. Leistung zu sprachen oder mit der Umwelt zu interagieren. 
Ist es jedoch geplant große komplexe objektorientierte Struktureden umzusetzen, ist es wahrscheinlich schlauer direkt eine Sprache zu wählen, welche darauf ausgelegt ist.

## Aufgabe 3 
Implementieren Sie eine Funktion mk-mp3-control, die ein Objekt zurückliefert, das die Kontrolleinheit eines MP3-Spielers repräsentiert. 
(Es sollen aber nicht wirklich Dateien abgespielt werden.) 
Das Objekt soll folgende Informationen speichern/zurückliefern: 
- Eine Liste, der gespeicherten MP3-Dateien (mit Dateiname und Dauer des Stücks), 
- die Anzahl der Titel
- der aktuelle Titel, der gerade abgespielt wird oder vorgewählt ist
- den Abspielstatus, also ob derzeit ein Titel (welcher?) abgespielt wird oder stop, wenn kein Titel abgespielt wird. 

Folgende Botschaften soll das Objekt verstehen: 
- laden: Hinzufügen einer neuen MP3-Datei an das Ende der Titelliste, 
- loeschen: Löschen einer Datei aus der Liste, Übergabe der Nummer des zu löschenden Titels
- abspielen/stop: Ändern des Abspielstatus, 
- vor/zurück: Titel erhöhen oder erniedrigen, sind keine Titel vorhanden oder ist das Ende oder 1 erreicht, so wird die Nachricht ignoriert, 
- unbekannte Nachrichten werden ignoriert.

### Code
```
#lang racket

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
```

### Anmerkungen
- Dieser Code ist ein perfektes Beispiel dafür, 
dass die Objektorientierung in Racket sehr schnell sehr umfangreichen code erzeugen kann, 
selbst wenn nur wenig funktionalität umgesetzt werden soll, welche keinen hohen Komplexitätsgrad hat.

## Aufgabe 4
Recherchieren Sie, ob Sie Programmiersprachen finden, die `dynamic scope` umsetzen und nennen Sie diese. 
Falls Sie keine finden, wie kann man `dynamic scope` in anderen Programmiersprachen nachbauen?

### Antwort


## 5 Todo!
## 6 Todo!
## 7 Todo!
## 8 Todo!

## Aufgabe 9
Nennen und erläutern Sie mindestens drei Gründe, warum es sinnvoll sein kann, ein Typsystem zu verwenden. Erläutern Sie anschließend den Unterschied zwischen einer dynamischen Typprüfung und einer statischen Typprüfung. Sie können für Ihre Antwort neben Racket auch noch auf andere Ihnen bekannte Programmiersprachen zurückgreifen.

### Gründe für Typisierung:
- Typisierung regelt das Speicherlayout:
<br>Wenn der Compiler auf eine typisierte Variable trifft (entweder über annotation oder den Typ über Inferenz herausfindet), kann er die passende Menge an Bytes für den Datentyp im Speicher Belegen und somit möglichst effizient ausnutzen.
<br>In nicht typisierten Sprachen kann es z.B. passieren, das für eine Variable standardmäßig 32 Bit belegt werden jedoch eigentlich nur 16 Bit genutzt werden, was zu einer Speicherverschwendung fürt. Ebenso könnte es passieren, dass es im Nachhinein auf 64 Bit erweitert werden muss was zusätzlichen Rechenaufwand bedeutet.
<br>z.B. in Java Weiß der Compiler direkt, ob es sich bei einer nummerischen Zahl um eine `short` (16 Bit), `int` (32 Bit) oder `long` (64 Bit) -Variable handelt und reserviert entsprechend große Speicherblöcke, welche im Nachhinein nicht erweitert werden müssen, ohne unnötige mengen an speicher zu belegen.
<br> **Anmerkung**: Bei Objekten und Datenstrukturen ist dieser Vorteil nicht direkt der Fall, da diese (z.B. in Racket und Java) mit Speicheradressen arbeiten. Für die primitiven-Bestandteile der Objekte und Datenstrukturen ist das Argument weiterhin gültig. 
- Typisierung erlaubt effizientere Ausführung eines Programms:
<br> Wenn der Compiler bereits weiß, dass eine Operation mit Variablen immer mit Ganzzahlen ist, kann er den Code dahingehend optimieren, dass dann die Ganzzahloperation des Rechners verwendet wird, welche Potenziell schneller ist, als wenn zur Laufzeit erst entschieden werden muss, welche arithmetische Funktion tatsächlich benötigt wird.
- Typisierung erhöht die Lesbarkeit eines Programms:
<br> Wenn man ein Typsystem verwendet, kann man trotz potenziell ungünstig gewählten Argumentnamen leichter Nachvollziehen welche Argumente die Funktion benötigt und besser verstehen, was die Funktion macht.
- Automatisches Finden von logischen Fehlern in einem Programm
<br>Durch die Typennotationen fügt man Redundante Informationen dem Programm zu. Diese Redundanz kann der Compiler dann nutzen, um während eines statischen Codes analyse zu prüfen, ob die Variablen-Belegungen mit der zusätzlich angegeben Information zu jedem Zeitpunkt sichergestellt ist. Wenn dem nicht so ist, wirft der Compiler einen Typfehler und weist den Entwickler auf den entsprechenden Logikfehler hin. 
- Erlaubt Modularisierung (wenn ein Typ zugleich eine Schnittstelle/Interface ausrückt)
<br>Die Typsignatur kann gleichzeitig ein Interface ausdrücken, welche Modulentwickler Angeben können um spätere Nutzer bei der nutzung, bzw. ansteuerung des Moduls zu unterstützen.

### Unterschied zwischen dynamischer und statischer Typprüfung
Die statische Typprüfung findet zur Compilezeit statt und weist somit während einer Statischen Code Analyse die Typ korrektheit nach, ohne dass das Programm läuft.
Die dynamische Typprüfung findet erst während der Laufzeit, bei der Variablenzuweisung, statt. Also wenn das Programm an die entsprechende Stelle angekommen ist, wird geprüft ob die Typ-Inn-Variante eingehalten wurde. Wenn nicht, wird an dieser Stelle ein Laufzeitfehler geworfen. Ein typisches Beispiel hierfür ist die `NullPointerException` in Java wenn zur Laufzeit ein Objekt erwartet wird, jedoch nur `null` ankommt und das System darauf nicht reagieren kann.

## Aufgabe 10
Erklären Sie den Unterschied zwischen der Reduktionsstrategie `normal order` und `applicative order`. Führen beide immer zum gleichen Ergebnis? Begründen Sie Ihre Antwort an einem selbst erstellten Beispiel.

### Antwort
Die Reduktionsstrategie bestimmt, welcher Teil eines Programms zu welchem Zeitpunkt und somit in welcher Reihenfolge ausgewertet wird. 
Solange die aufzurufenden Funktionen also keine Seiteneffekte haben und bei gleichem Input stets gleiches Output liefern, führen beide Strategien stets zum gleichen Ergebnis.

Bei `normal order` findet die Auswertung der Argumente erst dann statt, wenn sie tatsächlich benötigt werden. Das kann den vorteil haben, dass die Berechnung für im Funktionsrumpf nicht benötigte Argumente auch nicht durchgeführt wird und so leistung gespart werden kann.
```
(* (square 2) (square 3))
(* (* 2 2) (square 3))
(* 4 (square 3))
(* 4 (* 3 3))
(* 4 9)
36
```

Bei `applicative order` werden alle Argumente einer Funktion ausgewertet, bevor die Funktion aufgerufen wird.
```
(* (square 2) (square 3))
(* (* 2 2) (* 3 3))
(* 4 9)
36
```

Hat eine Funktion jedoch Nebeneffekte, kann die Ausführungsreihenfolge relevant sein, btw. kann es wichtig sein, dass das Argument erst ausgewertet wird, wenn die Funktion an der entsprechenden Stelle ankommt. 
In diesen Fällen kann es hinderlich sein, dass bei `applicative order` die Argumente bereits bei Funktionsaufruf ausgewertet werden.
<br>Beispiele hierfür wurden in Aufgabenblock I: Aufgabe 7 und 8 behandelt.  

