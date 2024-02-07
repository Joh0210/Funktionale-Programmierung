# Aufgabenblock II

## Aufgabe 1 
Wenn Sie eine neue Sprache erlernen, erwarten Sie eher `applicative order` oder `normal order`? Zeigen Sie die Vorzüge Ihrer Erwartung auf. Welche Auswertungsstrategie ist für Sie natürlicher? Begründen Sie Ihre Antwort. (Ergänzen Sie Ihre Antwort im Laufe des Semesters ggf. noch einmal, nachdem Sie den Lambda-Kalkül kennengelernt haben.)

### Wiederholung
Hierfür ist es wichtig die beiden Begriffe klar einzuordnen:
- **Applicative Order** wertet die Argumente einer Funktion vor Aufruf des Funktionskörpers aus. Das entspricht einer "normalen" Reihenfolge, in der zuerst die Teile und dann das Ganze betrachtet wird.
- **Normal Order/Lazy Evaluation** hingegen wertet die Argumente erst dann aus, wenn sie tatsächlich benötigt werden. Das entspricht einer "faulen" Reihenfolge, bei der nur das ausgewertet wird, was unbedingt erforderlich ist.

### Antwort
Diese Frage lässt sich für mich nicht pauschal beantworten, sondern ist abhängig von dem gewählten Paradigma.

#### Imperativ und Objektorientiert
Erwartung von **Applicative Order**.

Bei der imperativen und objektorientierten Programmierung wird oft gebrauch von Zustandsänderungen, Objektanpassungen und allgemeinen Seiteneffekten in Funktionen, Prozeduren und Methoden gemacht.
<br>Aufgrund von Übersichtlichkeit sollten Funktionen und Prozeduren Seiteneffekte zwar primär vermeiden, jedoch sind sie für das Paradigma erlaubt und auch vorgesehen.

Hierfür ist es entsprechend wichtig, dass die Argumente einer Funktion bereits vollständig ausgewertet sind, bevor der Funktionsrumpf ausgewertet wird, da sonst potenziell Seiteneffekte die Argumente beeinflussen könnten.

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
- **Normal Order/Lazy Evaluation**: Das Argument `original` wird innerhalb des Funktionsrumpfes von dem Compiler durch `myTestInt.getValue()` ersetzt, 
wodurch es durch den Seiteneffekt `myInt.add(1)` ebenfalls beeinflusst wird und es zu folgender Ausgabe kommt:
    ```
    Plus1: 2
    Original: 2
    ```

Auch wenn das Beispiel sehr künstlich ist, zeigt es das Problem welches **Normal Order/Lazy Evaluation** in imperativen und objektorientierten Programmiersprachen zur Folge hätte.

Zu erwarten ist daher **Applicative Order**. Entsprechend muss der Entwickler auf Seiteneffekte, Exception-Handling, etc. bereits bei Aufruf der Funktion achten.

#### Funktional
Erwartung von **Lazy Evaluation**, jedoch ist **Applicative Order** oft vertreten.

Funktionale Programmiersprachen unterstützen zwar auch teilweise Objektmanipulation und Zustandsänderungen, jedoch beschreibt das grundlegende Paradigma, dass das Ergebnis einer Funktion nur von den Eingabedaten abhängig ist.
Somit sollte der Wert der Argumente unabhängig von der Position des Aufrufs sein.

Funktionen, die doch Seiteneffekte haben, z.B. das Löschen aller Dateien einer Datenbank, sollen in der Regel nur unter gewissen Konditionen aufgerufen werden.
Damit diese Funktionen als Argument übergeben werden können und nicht direkt bei dem Funktionsaufruf ausgewertet werden, ist ebenfalls **Normal Order/Lazy Evaluation** notwendig.

Durch das Lambda-Kalkül lässt sich auch beweisen, dass **Normal Order/Lazy Evaluation** öfters auf ein Ergebnis kommt, da sofern **Applicative Order** zum Ergebnis führt auch **Normal Order/Lazy Evaluation** zwangsläufig terminiert. 
Dieser Zusammenhang gilt jedoch nicht andersherum.

In der Praxis wird jedoch auch in funktionalen Sprachen häufiger **Applicative Order** verwendet, da **Normal Order** tendenziell weniger effizient ist.

Als Alternative kann **Lazy Evaluation** statt **Normal Order** erwartet werden, da bei einer **Lazy Evaluation** das Argument maximal einmal berechnet wird und so bei erneuter Abfrage im Funktionsrumpf nicht erneut berechnet werden muss, sondern den entsprechenden Wert übergeben bekommt. 
Bei **Normal Order** würde die Berechnung der Funktion bei jeder Auswertung des Arguments stattfinden.
Mit **Lazy Evaluation** lassen sich somit Ressourcen sparen, da wenn ein Argument nicht benötigt wird nicht berechnet wird und sonst maximal einmal. 
Diese Methode hat jedoch einen zusätzlichen Speicherverbrauch als Folge.    

#### Logisch
Aufgrund von mangelnder Erfahrung mit dem Paradigma, erwarte ich keine der Auswertungsstrategien, sondern setze mich erst mit dem Paradigma auseinander.

#### Welche Auswertungsstrategie ist für mich natürlicher?
Da ich primär mit objektorientierten Sprachen gearbeitet habe, erscheint mir **Applicative Order** natürlicher. 
Jedoch halte ich es für wahrscheinlich, dass es anders wäre, wenn ich primär mit Sprachen gearbeitet hätte, welche **Normal Order/Lazy Evaluation** verwenden.

## Aufgabe 2
Gegeben ist folgender Racket-Code, der ein simples Bankkonto realisieren soll. Es soll möglich sein, ein neues Bankkonto einzurichten und Geld ein- und auszuzahlen.
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
die Implementierung von objektorientierten Konzepten.

Abschlussobjekte (Closures) sind Funktionen, mit einem Verweis auf ihre Definitionsumgebung. 
Also ist das "Objekt", das durch die Funktion `(make-account 10)` erstellt wird also eigentlich auch nur eine Funktion, was auch in der REPL-Ausgabe sichtbar ist:
```
> a
#<procedure>
```

Da diese Funktion Zugriff auf ihre Definitionsumgebung hat, können dort Daten hinterlegt werden, welche von dem Abschlussobjekt genutzt werden können. 
Diese Daten repräsentieren also die Attribute des Objektes. In diesem Beispiel haben "Objekte" die durch `make-account` erstellt werden nur ein Attribut `money`.

Abschlussobjekte haben zusätzlich auch noch Funktionen, also Methoden, welche das Objekt ausführen kann. 
In diesem Fall haben die Objekte nur eine einzige Funktion, welche einen gewissen Betrag auf das Konto überweisen oder abheben kann. 
Da das Objekt nur eine Methode besitzt, ist diese auch gleichzeitig die Funktion, welche das Objekt darstellt. 

Möchte man die Methode aufrufen, nutzt man das Objekt als Funktion des Klammerausdrucks und den Parameter (`movement`) als erstes Argument:
```
; 10 (€) einzahlen:
(a 10)

; 10 (€) abheben:
(a -10)

; Kontostand prüfen:
(a 0)
```

Für gewöhnlich unterstützt ein Abschlussobjekt mehrere Funktionen. In den Fällen werden diese innerhalb der erzeugenden Funktion definiert. 
Zusätzlich wird noch eine Funktion definiert, welches als Argument eine "Message" übergeben bekommt, 
anhand derer die richtige Funktion in einer Sprungtabelle ausgewählt und mit den restlichen Parametern aufgerufen wird. 
Diese Auswahlmethode ist anschließend die Funktion, welche das Objekt darstellt.

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

Auf diese Weise gibt es auch Polymorphie oder sogenanntes "Duck-Typing" (wie in Python),
bei dem es der Funktion egal ist, um welche Klasse von Objekt es sich handelt, solange es die eingegebene Funktion unterstützt.
"If it walks like a duck, swims like a duck, and quacks like a duck, then it probably is a duck." [Ursprung der Redewendung ist nicht eindeutig auf eine bestimmte Person zurückzuführen]

Abschlussobjekte allein würden jedoch nicht ausreichen, um Objektorientierung nachstellen zu können, denn sie können erstmal nur Nachrichten empfangen, Nachrichten an andere Objekte senden, Auskunft über den eigenen Zustand geben und neue Objekte erzeugen.
Erst mit der Zustandsorientierung können die Attribute der Objekte manipuliert werden. Funktionen, welche die Attribute des Objektes anpassen können und so den Zustand des Objektes manipulieren, lassen sich mit `set!` implementieren.
<br>In dem gegebenen Beispiel findet z.B. eine Einzahlung statt, wodurch das Attribut, welche den Kontostand repräsentiert, verändert wird. 
```
> (b 10)
110
> (b 10)
120
```

### Grenzen und Schwächen
- **Vererbung**: Vererbung ist etwas umständlicher. Es ist möglich das Parent-Objekt, als Attribut im Child-Objekt zu speichern und unbekannte Massages des Childes an den Parent weiterzuleiten, jedoch ist das verhältnismäßig unübersichtlich und schwer mit mehreren Parents. 
- **Typprüfung**: Objektorientierte Programmiersprachen, wie Java, nutzen in der Regel ein statisches Typsystem welches auch selbsterstellte Klassen als Typ wahrnimmt. 
Die einzige Typprüfung, die hier jedoch existiert, ist das, oben erwähnte "Duck-Typing", welches dazu führen kann, dass falsche Objekte einer Funktion übergeben werden können. 
In sehr ungünstigen Situationen kann es sogar passieren, dass es nie zu einem tatsächlichen Error kommt, sondern lediglich falsche Ausgaben stattfinden. Dieses Problem haben allerdings auch einige objektorientierte Sprachen, wie z.B. Python.
<br>Bei dem Versuch Abschlussobjekte mit TypedRacket zu verwenden, kam es zu einigen Komplikationen, weswegen hier nicht genauer darauf eingegangen werden kann. 
Es lässt sich jedoch sagen, dass TypedRacket mit Abschlussobjekten sehr kompliziert werden kann.
- **Funktionales Paradigma**: Racket selbst ist eine funktionale Sprache, was sich auch bei der Erzeugung der Objekte wieder spiegelt. 
Es ist zwar möglich, jedoch deutlich komplexer als in anderen Sprachen. 

Zusätzlich sollte erwähnt werden, wenn man das funktionale Paradigma nutzen möchte, ist es oft sinnvoll die Nutzung von Objekten zu minimieren, da vor allem Seiteneffekte dem funktionalen Paradigma widerstreben.
<br>An manchen Stellen kann es sinnvoll sein, objektorientierte Strukturen in dem funktionalen Paradigma umzusetzen, um z.B. Rechenaufwand zu minimieren oder mit der Umwelt zu interagieren. 
Ist es jedoch geplant große komplexe objektorientierte Strukturen umzusetzen, ist es empfehlenswert direkt eine Sprache zu wählen, welche darauf ausgelegt ist.

## Aufgabe 3 
Implementieren Sie eine Funktion `mk-mp3-control`, die ein Objekt zurückliefert, das die Kontrolleinheit eines MP3-Spielers repräsentiert. 
(Es sollen aber nicht wirklich Dateien abgespielt werden.) 
Das Objekt soll folgende Informationen speichern/zurückliefern: 
- Eine Liste, der gespeicherten MP3-Dateien (mit Dateinamen und Dauer des Stücks), 
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
      ((and (exact-positive-integer? n) (<= n (length lieder)))
       (set! lieder (loeschen-inner lieder null (- n 1)))
       (string-append "Das " (number->string n) ". lied wurde entfernt" (auswahl-anpassen)))
      (else "Dieses Element existiert nicht")))

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
dass die Objektorientierung in Racket sehr schnell sehr umfangreichen Code erzeugen kann, 
selbst wenn nur wenig Funktionalität umgesetzt werden soll, welche keinen hohen Komplexitätsgrad hat.

## Aufgabe 4
Recherchieren Sie, ob Sie Programmiersprachen finden, die `dynamic scope` umsetzen und nennen Sie diese. 
Falls Sie keine finden, wie kann man `dynamic scope` in anderen Programmiersprachen nachbauen?

### Antwort
- Perl [[3]](http://www.cpan.org/authors/id/E/EI/EIKEG/doc/perl-tutorial-DE_2.07.pdf)
- LISP [[4]](https://link.springer.com/book/10.1007/978-3-642-71455-9)

### Quellen
- [3] **Eike Grote**: [Die Programmiersprache Perl](http://www.cpan.org/authors/id/E/EI/EIKEG/doc/perl-tutorial-DE_2.07.pdf) Version 2.07, S. 150-158, 2019
- [4] **Herbert Stoyan, Günter Görz**: [LISP: Eine Einführung in die Programmierung](https://link.springer.com/book/10.1007/978-3-642-71455-9) Springer, Berlin, S175-177; S182-183, 1984

## Aufgabe 5
Wie wird in Racket/Scheme `lexical scope` umgesetzt und warum ist `dynamic scope` eine schlechte Idee? 
Illustrieren Sie Ihr Argument an einem selbst entwickelten Beispiel.

### Umsetzung von lexical scope 
Bei `lexical scope` wird der Gültigkeitsbereich von Variablen durch deren Umgebung definiert. 
Eine Variable gehört hier zu der Umgebung einer Funktion, wenn sie:
1. Als Argument der Funktion übergeben wurde.
2. Im Funktionskörper der Funktion definiert wurde. (Definitionen im Funktionskörper einer Unterfunktion zählen nicht dazu!)
3. In der Umgebung einer übergeordneten Funktion (oder der globalen Umgebung) enthalten ist. 

Wenn eine neue Variable mit demselben Bezeichner durch 1. oder 2. erstellt wird, wird in der Umgebung der neu zugewiesene verwendet und der höheren Umgebung ignoriert.

### Warum ist dynamic scope eine schlechte Idee?
Bei `lexical scope` wird der Gültigkeitsbereich von Variablen durch deren Umgebung definiert,
sodass Funktionen Zugriff auf Variablen haben, die als Argument übergeben wurden, in einer übergeordneten Umgebung oder in dieser Funktion definiert sind.

Im Gegensatz dazu ist bei `dynamic scope` die Gültigkeit und der Wert einer Variable durch die Aufrufreihenfolge bestimmt. 

`lexical scope` sorgt also für eine bessere:
- **Wartbarkeit**: Durch `lexical scope` ist das Nachvollziehen von Zuordnungen leichter, da lediglich Zuweisungen innerhalb der Funktion und der übergeordneten Umgebung nachvollzogen werden müssen, was die Wartbarkeit stark vereinfachen dürfte.
- **Fehlervermeidung**: Durch Funktionsaufrufe im `dynamic scope` können ungewollte Seiteneffekte eintreten, welche zu schwer find bare Fehler führen können, da die Funktionen für sich genommen das richtige Ergebnis liefern, jedoch nicht in Kombination.
- **Modularität**: Da im `lexical scope` Funktion nur die Variablen in ihrer Umgebung manipulieren können, können sie also ohne potenzielle Seiteneffekte in einer Bibliothek für andere Programme genutzt werden.

### Code
#### Dynamic
```
#lang racket

(define x 10)

(define (mach-was)
  (cond
    (x "Zweig 1")
    (else "Zweig 2")))

; Aus einer Library kopiert: 
(define (super-wichtige-rechnung)
  ; + 100 Zeilen Code
  (set! x #f) ; eigentlich define. x würde in den nachvollgenden Zeilen gebraucht werden
  ; + 70 Zeilen Code
  (define y "test") ; y würde in den nachvollgenden Zeilen gebraucht werden
  ; + 20 Zeilen Code
  (string-append "Super wichtige Rechnung!"))

(mach-was) ; -> "Zweig 1"
; +30 Zeilen
(super-wichtige-rechnung)
; +14 Zeilen
(mach-was) ; -> "Zweig 2"
```

Die Funktion `mach-was` liefert bei den beiden Aufrufen jeweils ein anderes Ergebnis, 
ohne dass eine bewusste Anpassung von `x` stattfand.

```
; +30 Zeilen
(super-wichtige-rechnung)
; +14 Zeilen
```
Innerhalb der 45 Zeilen liefert jede Funktion für sich getestet das richtige Ergebnis.
Nach einigen Tests stellt sich heraus, dass die Funktion `super-wichtige-rechnung` für den ungewollten Seiteneffekt sorgt.
<br>Folglich: Fehleranfällig und schlechte Modularität.

Zudem, wenn man in einer Funktion eine neue Variable hinzufügen will und `super-wichtige-rechnung` als Unterfunktion nutzen muss (oder `super-wichtige-rechnung` allgemein nutzt und eine globale Variable hinzufügen will),
muss der Nutzer wissen, dass neben `x` auch `y` nicht verwendet werden darf, da es sonst wieder ungewollt überschrieben werden würde.
Um das herauszufinden, müss jedoch die komplette Funktion `super-wichtige-rechnung` mit über 190 Zeilen untersucht werden.
<br>Folglich: schlechte Wartbarkeit und Modularität.

#### Lexical

```
#lang racket

; 2.5 Lexical

(define x 10)

(define (mach-was)
  (cond
    (x "Zweig 1")
    (else "Zweig 2")))

; Aus einer Library kopiert: 
(define (super-wichtige-rechnung)
  ; + 100 Zeilen Code
  (define x #f) ; x würde in den nachvollgenden Zeilen gebraucht werden
  ; + 70 Zeilen Code
  (define y "test") ; y würde in den nachvollgenden Zeilen gebraucht werden
  ; + 20 Zeilen Code
  (string-append "Super wichtige Rechnung!"))

(mach-was) ; -> "Zweig 1"
; +30 Zeilen
(super-wichtige-rechnung)
; +14 Zeilen
(mach-was) ; -> "Zweig 1"
```

Diese Lexical-Version liefert das erwartete Ergebnis.

### Anmerkungen
- Um `dynamic scope` zu simulieren, wurden alle Variablen-Erstellungen nach der 1. durch `!set` ersetzt.

## Aufgabe 6
Überlegen Sie ein Beispiel, in dem verzögerte Auswertung sinnvoll sein kann. 
Wie würden Sie die verzögerte Auswertung umsetzen? Begründen Sie Ihr Vorgehen.

### Antwort
Verzögerte Auswertung sollte verwendet werden, wenn eine Berechnung relativ aufwendig ist und nur unter gewissen Konditionen benötigt wird.

Hierfür gibt es die Wahl zwischen `thunk` und Promises:
- `thunk` ist eine eingewickelte Berechnung, also eine Funktion, welche in einem z.B. lambda-Ausdruck eingewickelt ist (z.B. `(lambda () (+ 2 3))`).
<br>Möchte man das Ergebnis der Funktion an einer Stelle nutzen, so muss das `thunk` lediglich ausgewertet werden.
<br>Wird `thunk` allerdings mehrfach ausgewertet, findet die Berechnung an jeder Stelle erneut statt.
- Promises werden durch `delay` erstellt. Sie beinhalten die Funktion, welche potenziell ausgeführt werden muss. 
<br> Um an das Ergebnis der Funktion zu gelangen, muss das Promise an die `force` Funktion übergeben werden, welche die Berechnung anschließend durchführt.
<br> Wird das Ergebnis des Promise erneut benötigt, muss die Berechnung nicht erneut durchgeführt werden, sondern es wird ein zwischengespeicherter Wert verwendet. Das hat einen gewissen Speicherverbrauch zur Folge.

Wenn das Ergebnis der Berechnung an vielen Stellen benötigt wird, und nur einen geringen Speicheraufwand hat, sollten also Promises verwendet werden. Wird das Ergebnis nur einmal benötigt, oder der Speicheraufwand ist im Verhältnis zum Rechenaufwand zu groß, sollte `thunk` verwendet werden.

Genauere Erklärungen hierzu auch in Aufgabenblock I: Aufgabe 16.

### Code

#### Normale Auswertung
```
#lang racket

(define (entscheidung kondition res)
  (cond
    (kondition res)
    (else "wird nicht benötigt")))

(define (aufwendige-berechnung)
  (sleep 3)
  "wird benötigt")

(entscheidung #t (aufwendige-berechnung)) ; Es wird 3 sec gewartet
(entscheidung #t (aufwendige-berechnung)) ; Es wird 3 sec gewartet
(entscheidung #f (aufwendige-berechnung)) ; Es wird 3 sec gewartet
```

#### Verzögerte Auswertung: thunk
```
#lang racket

(define (entscheidung kondition thunk)
  (cond
    (kondition (thunk))
    (else "wird nicht benötigt")))

(define aufwendige-berechnung (lambda ()
  (sleep 3)
  "wird benötigt"))

(entscheidung #t aufwendige-berechnung) ; Es wird 3 sec gewartet
(entscheidung #t aufwendige-berechnung) ; Es wird 3 sec gewartet
(entscheidung #f aufwendige-berechnung) ; Es wird nicht gewartet
```

#### Verzögerte Auswertung: promise
```
#lang racket

(define (entscheidung kondition promise)
  (cond
    (kondition (force promise))
    (else "wird nicht benötigt")))

(define aufwendige-berechnung (delay
  (sleep 3)
  "wird benötigt"))

(entscheidung #t aufwendige-berechnung) ; Es wird 3 sec gewartet
(entscheidung #t aufwendige-berechnung) ; Es wird nicht gewartet
(entscheidung #f aufwendige-berechnung) ; Es wird nicht gewartet
```

## Aufgabe 7
Nehmen Sie zum Zitat "Objects are a poor man’s closures. Closures are a poor man’s objects." Stellung 
und vergleichen die vorgestellten Möglichkeiten der Objektorientierung mit einer Ihnen bekannten objektorientierten Programmiersprache.

### Stellungnahme 
Das Zitat vermittelt, dass es eine gewisse Gemeinsamkeit zwischen Abschlussobjekten und normalen Objekten besteht und sie mehr oder weniger das gleiche Repräsentieren und austauschbar sind.

Durch den Abschnitt "a poor man’s" wird jedoch zusätzlich impliziert, dass normale Objekte und Abschlussobjekte keineswegs gleich sind. 
So wird beschrieben, dass ein Entwickler welche Objekte nutzen möchte, jedoch nur Abschlussobjekte hat, diese zwar als alternative nutzen kann, es jedoch einige Unterschiede gibt, welche zu Anpassungen bei der Implementierung führen können.
Dasselbe gilt für Objekte statt Abschlussobjekte.

### Vergleich
Objekte in Java sind Instanzen von Klassen, welche Daten (Attribute) bündeln und zugehörige Methoden, die auf diesen Daten operieren. 
Durch die jeweiligen Konzepte der Sprache unterstützen Objekte Datenkapselung, Vererbung und Polymorphismus.

Abschlussobjekte in Racket hingegen sind eigentlich nur Funktionen, die auf einen bestimmten Kontext bezugnehmen.
Durch den Bezug auf ihr Umfeld können sie lokale Variablen erfassen und diese innerhalb ihres Gültigkeitsbereichs verwenden und mittels Zustandsorientierung auch manipulieren. 

Wie das Zitat vermittelt, kann man Abschlussobjekte als "Objekte" nutzen, dessen lokale Variablen die Attribute repräsentieren und dessen Unterfunktionen die Methoden.
<br>Beide Konzepte ermöglichen somit Organisation und Strukturierung von Code.

Jedoch gibt es auch ein paar Unterschiede:

Objekte in Java dienen meist dazu, Daten und Methoden zu kapseln um Entitäten (der realen Welt) und deren Beziehungen untereinander zu modellieren.

Abschlussobjekte in Racket sind allerdings darauf ausgelegt, Funktionalitäten mit Kontextbezug zu schaffen. 
Entitäten zu modellieren ist damit zwar möglich, doch vor allem bei größeren Strukturen auch deutlich aufwendiger.
<br>Da sie allerdings ihren kompletten Kontext erfassen, ermöglichen sie teilweise flexiblere und abstraktere Lösungen als es mit Objekten möglich wäre.

## Aufgabe 8
Überlegen Sie sich einen Anwendungsfall für Ströme (streams) und illustrieren Ihren Anwendungsfall an einer Implementierung in Racket. 
Die Implementierung muss nicht voll funktionsfähig sein, es reicht ein Prototyp oder Pseudo-Code.

### Antwort
Den Haupteinsatz von Streams in Racket sehe ich in der Verarbeitung von endlos-kontinuierlichen oder extrem großen Datenmengen mit lazy-streams. 
Bei kleinen, finiten Datenmengen ist es in der Regel ohne Probleme möglich, diese als Liste zu speichern und bei Funktionen alle Elemente der Liste mit einzubeziehen. 

Bei endlosen oder extrem großen Mengen ist das jedoch tendenziell nicht möglich. Lässt diese Menge sich jedoch (mathematisch) Beschreiben oder kontinuierlich auslesen, 
lässt sie sich als lazy-stream darstellen, welcher für einige Verarbeitungen genutzt werden kann.
<br>**Anmerkung**: Verarbeitungen, welche den ganzen Stream benötigen würden, funktionieren weiterhin nicht.

Eine Beispielfunktion wäre also das Finden des ersten Elements des endlosen Streams für das eine gewisse Bedingung gilt. 

### Code
```
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
; findet das erste Element des streams für das "bedingung" gilt
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
```

## Aufgabe 9
Nennen und erläutern Sie mindestens drei Gründe, warum es sinnvoll sein kann, ein Typsystem zu verwenden. 
Erläutern Sie anschließend den Unterschied zwischen einer dynamischen Typprüfung und einer statischen Typprüfung. 
Sie können für Ihre Antwort neben Racket auch noch auf andere Ihnen bekannte Programmiersprachen zurückgreifen.

### Gründe für Typisierung:
- Effizienteres Speicherlayout:
<br>Wenn der Compiler auf eine typisierte Variable trifft (entweder über Annotation oder über Inferenz identifiziert), 
kann er die passende Menge an Bytes für den Datentyp im Speicher belegen und somit möglichst effizient ausnutzen.
<br>In nicht typisierten Sprachen kann es z.B. passieren, dass für eine Variable standardmäßig 32 Bit belegt werden, 
jedoch eigentlich nur 16 Bit genutzt werden, was zu einer Speicherverschwendung führt. 
Ebenso könnte es passieren, dass es im Nachhinein auf 64 Bit erweitert werden muss, was zusätzlichen Rechenaufwand bedeutet.
<br>Beispiel: in Java weiß der Compiler direkt, ob es sich bei einer nummerischen Zahl um eine `short` (16 Bit), `int` (32 Bit) oder `long` (64 Bit) Variable handelt 
und reserviert entsprechend große Speicherblöcke, welche im Nachhinein nicht erweitert werden müssen, ohne unnötige Mengen an Speicher zu belegen.
<br> **Anmerkung**: Bei Objekten und Datenstrukturen ist dieser Vorteil nicht direkt der Fall, da diese (z.B. in Racket und Java) mit Speicheradressen arbeiten. 
Für die primitiven-Bestandteile der Objekte und Datenstrukturen ist das Argument weiterhin gültig. 
- Bessere Lesbarkeit:
<br> Wenn man ein Typsystem verwendet, kann man trotz potenziell ungünstig gewählten Argumentnamen leichter Nachvollziehen 
welche Argumente die Funktion benötigt und besser verstehen, was die Funktion macht.
- Automatisches Finden von logischen Fehlern:
<br>Durch die Typennotationen fügt man redundante Informationen dem Programm zu. 
Diese Redundanz kann der Compiler nutzen, um während einer statischen Code-Analyse zu prüfen, 
ob die Variablen-Belegung mit der zusätzlich angegeben Information zu jedem Zeitpunkt sichergestellt ist. 
Wenn dem nicht so ist, wirft der Compiler einen Typfehler und weist den Entwickler auf den entsprechenden Logikfehler hin. 
- Modularisierung (wenn ein Typ zugleich eine Schnittstelle/Interface ausdrückt):
<br>Die Typsignatur kann gleichzeitig ein Interface ausdrücken, welche Modulentwickler angeben können um spätere Nutzer bei der Nutzung, bzw. Ansteuerung des Moduls zu unterstützen.

### Unterschied zwischen dynamischer und statischer Typprüfung
Die statische Typprüfung findet zur Compilezeit statt und weist somit während einer statischen Code-Analyse die Typ Korrektheit nach, ohne dass das Programm ausgeführt wird.


Die dynamische Typprüfung findet erst während der Laufzeit, bei der Variablenzuweisung, statt. Also wenn das Programm an der entsprechenden Stelle ankommt, wird geprüft, ob die Typ-Inn-Variante eingehalten wurde.  
Wenn nicht, wird an dieser Stelle ein Laufzeitfehler geworfen. 
<br>Ein typisches Beispiel hierfür ist die `NullPointerException` in Java, bei welcher zur Laufzeit ein Objekt erwartet wird, 
jedoch `null` erhält und das System darauf nicht reagieren kann.

## Aufgabe 10
Erklären Sie den Unterschied zwischen der Reduktionsstrategie `normal order` und `applicative order`. Führen beide immer zum gleichen Ergebnis? Begründen Sie Ihre Antwort an einem selbst erstellten Beispiel.

### Antwort
Die Reduktionsstrategie bestimmt, welcher Teil eines Programms zu welchem Zeitpunkt und somit in welcher Reihenfolge ausgewertet wird. 
Solange die aufzurufenden Funktionen also keine Seiteneffekte haben und bei gleichem Input stets gleiches Output liefern, führen beide Strategien stets zum gleichen Ergebnis.

Bei `normal order` findet die Auswertung der Argumente erst dann statt, wenn sie tatsächlich benötigt werden. Das kann den Vorteil haben, dass die Berechnung für im Funktionsrumpf nicht benötigte Argumente auch nicht durchgeführt wird und so Leistung gespart werden kann.
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

Hat eine Funktion jedoch Nebeneffekte, kann die Ausführungsreihenfolge relevant sein, bzw. kann es wichtig sein, dass das Argument erst ausgewertet wird, wenn die Funktion an der entsprechenden Stelle ankommt. 
In diesen Fällen kann es hinderlich sein, dass bei `applicative order` die Argumente bereits bei Funktionsaufruf ausgewertet werden.
<br>Beispiele hierfür wurden in Aufgabenblock I: Aufgabe 7 und 8 behandelt.  
