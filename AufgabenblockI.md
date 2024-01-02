# Aufgabenblock I

## Aufgabe 1
Schreiben Sie eine Funktion, die eine natürliche Zahl `n` erhält und eine Liste der Zahlen von `n` bis `1` erzeugt.

### Code

``` 
#lang racket

(define (sequence n)
  (define (sequence-inner n acc)
    (cond
      ((<= n 1) acc)
      (else (sequence-inner (- n 1) (append acc (list(- n 1)))))))
  (cond
    ((or (not (integer? n)) (< n 1)) null)  
    (else (sequence-inner n (list n)))))
```

### Anmerkungen
- Es wird angenommen, der mathematische Ansatz der natürlichen Zahlen verwendet:
<br>Die natürlichen Zahlen sind die Menge aller positiven ganzen Zahlen, also ohne die 0.
<br>&#8469; = {1;2;3;...}
- Bei fehlerhaften Eingaben wird die Leere Liste (`null`) zurück gegeben 
- Die Funktion wurde Endrekursiv gebaut 

### Typed Racket
Die Aufgabe wurde auch einmal in Typed Racket verfasst, um mit Zahlen mit Positiver Typsignatur in kombination mit Rechenoperationen zu arbeiten.

<br>Wie erwartet kam es hierbei zu einer Komplikation:

``` 
#lang typed/racket

(: sequence (-> Positive-Exact-Rational (Listof Exact-Rational)))
(define (sequence n)
  (: sequence-inner (-> Exact-Rational (Listof Exact-Rational) (Listof Exact-Rational)))
  (define (sequence-inner n acc)
    (cond
      ((<= n 1) acc)
      (else (sequence-inner (- n 1) (append acc (list(- n 1)))))))

  (sequence-inner n (list n)))
```

Als Rückgabewert `(Listof Exact-Rational)`, anstelle von `(Listof Positive-Exact-Rational)` gewählt, 
da der Racket-Compiler erwartet das aus `(- n 1)` 0 entstehen kann, wenn n = 1 entspricht. 
Jedoch kann es dazu nie kommen, da für den Fall n=1 der anderer Zweig der If-Abfrage ausgewählt wird: `((<= n 1) acc)` 
Der Compiler kann das jedoch nicht erkennen und prüft lediglich die Typsignatur der `-` Operation, welche für diesen Aufruf wie folgt aussieht`(: - (-> Positive-Integer One Nonnegative-Integer))`.
<br><br>Um diesen Fehler zu umgehen wurde `Exact-Rational` statt `Positive-Exact-Rational` an Stellen außerhalb des Eingabeparameters verwendet. Das bedeutet jedoch, das Informationen über die Elemente der Liste verloren gehen, was für weitere Verarbeitungen unpraktisch sein kann. 

## Aufgabe 2
Schreiben Sie eine Funktion `my-reverse`, die eine Liste erhält und die Elemente umdreht, also bspw. `(my-reverse ’(1 2 3))` führt zu `(3 2 1)`.

### Code
```
#lang racket

(define (my-reverse lst)
  (foldl cons '() lst))
```

### Anmerkungen
- Bei erstellung der Funktion wurde der Fokus auf eine möglichst leichte Lösung gelegt, entsprechend wurde nicht auf ein möglichst performantes Design geachtet. 
- Für die Umsetzung wurde ein Inhalt aus der Vorlesung, Currying, eingesetzt. Eine Technik in der funktionalen Programmierung, welche aus einer n-Stelligen Funktion eine n-m Stellige Funktion macht, mit 0 < m &#8804; n  
- Tendenziell sind Funktionen die direkt von der Programmiersprache zur verfügung gestellt werden, auf Performanz optimiert (z.B. die meisten "Aufwendigen" Funktion die von Python zur verfügung gestellt werden sind in C/C++ Code verfasst). So sollte also trotz keiner direkten Performance-Optimierung die Performance durchaus akzeptabel bis gut sein.

## Aufgabe 3
Schreiben Sie eine Funktion `my-find`, die eine Liste `lst` und ein Argument `x` erhält und mittels `equal?` prüft, ob das Element in der Liste enthalten ist. Die Funktion soll `#t` antworten, wenn dem so ist, oder mit `#f` wenn nicht.

### Code
```
#lang racket

(define (my-find lst x)
  (ormap (lambda (a) (equal? a x)) lst))
```

### Anmerkungen
- Wie auch bei Aufgabe 2. kann man hier eine existierende Funktion höherer Ordnung nutzen und in die neue Funktion "Einbacken"
- Es wäre auch denkbar die Aufgabe mit der `foldl` Funktion zu lösen, jedoch gibt es 2 große Unterschiede:
  1. Nach persönlichem Empfinden ist die version mit `ormap` deutlich übersichtlicher.
  2. Die Version mit `foldl` arbeitet in jedem Fall die komplette Liste durch, selbst wenn das Element bereits gefunden wurde. Bei `ormap` hingegen wird die auswertung beendet, sobald das erste Element gefunden, für welches die Kondition `true` ergibt. <br>**Beispiel:** Eine Liste mit 10.000 Elementen, bei dem jedes Element dem gesuchten Element entspricht, benötigt bei der `ormap` version nur einen Verarbeitungsschritt um `#t` auszugeben. Bei der `foldl` Version 10.000.
  

### foldl-Version:
```
(define (my-find lst x)
  (foldl
   (lambda (a b) (or (equal? a x) b))
   #f lst))
```

## Aufgabe 4
Schreiben SIe eine Funktion `my-max`, die die höchste Zahl einer Liste von Zahlen ermittelt und nutzen Sie dazu eine `fold`-Funktion.

### Code
```
#lang racket

(define (my-max lst)
  (define (max-of-two a b)
    (cond
      ((>= a b) a)
      (else b)))

  (cond
    ((or (empty? lst) (not (andmap number? lst))) (error"ungueltige eingabe für die Funktion \"my-max\". Benötigt nicht leeren Liste von Zahlen"))
    (else (foldl max-of-two (first lst) (rest lst)))))
```

### Anmerkungen
- Die funktion wird wieder "Eingebacken"
- Statt eine anonyme Funktion `max-of-two` anzulegen, hätte man es auch genauso gut mit einer Lambda-Funktion umsetzen können, jedoch ist es auf diese weise Übersichtlicher
- Bei ungültigen Eingaben wird ein Error, mit entsprechendem Hinweistext, geworfen, da es keine sinnvolle Antwort auf eine solche Eingabe gäbe. 

## Aufgabe 5
Schreiben Sie ein weiteres `my-map` unter Benutzung von einer `fold`-Funktion (entscheiden Sie selbst welches).

### Code
```
#lang racket

(define (my-map f lst)
  (cond
    ((empty? lst) empty)
    (else
     (foldl
      (lambda (a b) (append b (list (f a))))
      (list (f (first lst))) (rest lst)))))
```
### Anmerkungen
- Die funktion wird wieder "Eingebacken"

## Aufgabe 6
Schreiben Sie eine Funktion repeated, die eine Funktion `f` und eine natürliche Zahl `n` erhält und dann `f` n-mal ausführt. Beispiel:
```
((repeated sqr 2) 3)
81

((repeated add1 10) 1)
11
```

### Code
```
#lang racket

(define (repeated f n)
  (cond
    ((or (not (integer? n)) (<= n 1)) f) 
    (else (compose f (repeated f (- n 1))))))
```

### Anmerkungen
- Durch die `compose` Funktion und dem rekursiven Aufruf von `repeated`, wird die übergebene Funktion mit sich selbst verknüpft.
- Bei ungültigen Eingaben wird angenommen, das 1 eingegeben wurde, und so die übergebene Funktion 1:1 zurück gegeben 

## Aufgabe 7
Gegeben folgender Code:
```
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
```
Was erhält der Nutzer, wenn er `(test 0 (p))` eingibt, bei applicative order, bei normal order?
### Antwort:
- normal order: 0
- applicative order: Endlose Rekursion, und somit kein Ergebniss

### Begründung:
Die Funktion `p` ruft sich endlos rekursiv selbst auf, sobald sie einmal ausgewertet wird. Ist das der Fall, hängt das Programm in einer Endlosschleife fest, und es muss manuell gestoppt werden.
<br>Somit gilt, sobald `(p)` ausgeführt wird, kommt es zu einer endlosen Rekursion und das Programm muss manuel beendet werden.
<br>**Anmerkung**: Für gewöhnlich benötigt Rekursion eine gewisse Menge an Speicher um die Rücksprungadresse zu hinterlegen. Auf diese Weise kommt es bei einer Endlosen Rekursion in den meisten Fällen zu einem absturz. 
Da diese Funktion jedoch Endrekursiv ist (der rekursive Aufruf der einzige, und somit letzte Operation in der Funktion), optimiert Racket diesen Teil, wodurch der Speicherplatz für den Funktionsaufruf freigegeben wird.
Auf diese Weise stützt das Programm nie ab, sondern verweilt in der Endlosschleife

Ob das Programm abstützt oder nicht, ist jedoch abhängig von der "Evaluation-Strategy", 
denn bie eingabe von `(test 0 (p))` ist die Kondition `(= x 0)` stets erfüllt, so wird der Abschnitt `y` nie erreicht, also der Abschnitt in welchem Rückgabewert von `(p)` erst relevant wird.

Bei **normal order**, werden die Funktionsargumente erst ausgewertet, wenn sie tatsächlich im Funktionskörper aufgerufen werden. Bei diesem Verfahren wird `(p)` aber nie aufgerufen, wodurch das Programm mit `0` als Ergebnisse terminiert.

Bei **applicative order**, werden die Funktionsargumente bereits ausgewertet, wenn die Funktion aufgerufen wird. Auf diese Weise wird die Funktion `(p)` aufgerufen und es kommt zur endlosen Rekursion, trotz dass ihr theoretischer Rückgabewert im späteren Funktionskörper nicht benötigt wird.

## Aufgabe 8
Ein Nutzer schreibt sich ein `if` neu, da es ja `cond` gibt:
```
(define (new-if predicate then-clause else-clause)
(cond (predicate then-clause)
(else else-clause)))
```
Erste Versuche zeigen: Das funktioniert auch.
```
(new-if (= 2 3) 0 5) 
5
(new-if (= 1 1) 0 5) 
0
```
Funktioniert `new-if` nun wie `if` oder gibt es noch ein Problem? Wenn ja, welches?

### Antwort:
Nein, es funktioniert nicht `new-if` exact wie `if`. 

In Racket werden sind von Nutzer erstelle Funktionen immer **applicative order**, also die Funktionsargumente bereits bei Funktionsaufruf ausgewertet, also die Funktionen werden ausgeführt, trotz dass ihr Rückgabewert erst später im Funktionskörper relevant ist, wenn überhaupt.
Dieses Verhalten ist entsprechend auch bei `new-if` der Fall, es handelt sich also um eine strikte Funktion. Bei `if` hingegen wird immer nur der Zweig ausgewertet, der durch die Kondition bestimmt wurde.

In vielen Fällen bei funktionalen Sprachen ist das zwar egal sein, aber wenn eine Funktion z.B. für einen Error sorgt, mit der Umwelt/System interagiert oder einen Zustand ändert, ist es wichrig das die Funktion nur aufgerufen wird, wenn die Kondition dafür tatsächlich erfüllt ist!

### Beispiel:
Eine theoretische Funktion die alle Dateien einer Datenbank löschen soll:
```
(define (deleate-all-flies)
  (print "Everything has been deleted")
  #f)
```

mit new-if: `(new-if (= 0 0) #t (deleate-all-flies))`
<br> Rückgabe: `"Everything has been deleted!"#t`

mit if: `(if (= 0 0) #t (deleate-all-flies))`
<br> Rückgabe: `#t`

## Aufgabe 9
Schreiben Sie endrekursive Fassungen von zwei beliebigen Funktionen, die auf Listen operieren (bspw. eine Funktion, die jedes Element einer Liste quadriert).

### Code
```
; 1
(define (my-map f lst)
  (define (my-map-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-map-inner f (rest lst) (append acc (list (f (first lst))))))))

  (my-map-inner f lst '()))

; 2
(define (my-foldl f start lst)
  (define (my-foldl-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-foldl-inner f (rest lst) (f (first lst) acc)))))

  (my-foldl-inner f lst start))
```

## Aufgabe 10
Endrekursive Funktionen sind Funktionen, bei denen der rekursive Aufruf die letzte Operation in der Funktion ist.
Häufig wird dies durch die nutzung eines Akkumulators bewerkstelligt welcher das Ergebnis der einzelnen Berechnungsschritte erhält, und am Ende das finale Ergebnis der Funktion darstellt und zurückgegeben wird. 

Für gewöhnlich benötigt Rekursion eine gewisse Menge an Speicher, um die Rücksprungadresse zu hinterlegen, jedoch
erkennt Racket solche endrekursiven Funktionen und optimiert diese dahingehend, der Speicherplatz für den Funktionsaufruf freigegeben wird, und der Stack somit nicht weiter gefüllt wird und somit Stack Overflow verhindert.

## Aufgabe 11
Wann liefern `foldr` und `foldl` das gleiche Ergebnis? Geben Sie jeweils ein selbst gewähltes Beispiel für beide Fälle (ergibt das gleiche, ergibt etwas anderes) an. Wie unterscheiden sich beide Funktionen in ihrem Laufzeitverhalten (Geschwindigkeit, Speicherbedarf etc.)?

### Antwort
In der Funktionalität unterscheiden sich `foldr` und `foldl` darin, in welcher Richtung die Liste durchlaufen wird.
- `foldl`: 1. Element -> Letztes Element
- `foldr`: Letztes Element -> 1. Element

Somit ist das Ergebnis bei Funktionen welche dem Kommutativgesetz folgen, also die Reihenfolge der Argumente kein einfluss auf das Ergebnis haben, identisch bei `foldr` und `foldl`.
- Beispiel für kommutative Funktionen: Addition
- Beispiel bei dem die Reihenfolge relevant ist: Subtraktion

```
(define (my+ x y) (+ x y))
(define (my- x y) (- x y))

; identisch
(foldl my+ 0 (list 1 2 3 4))
(foldr my+ 0 (list 1 2 3 4))

; verschieden
(foldl my- 0 (list 1 2 3 4))
(foldr my- 0 (list 1 2 3 4))
```

Beim Speicherbedarf unterscheiden sich `foldr` und `foldl` stark, denn `foldl` benötigt eine Konstante Menge an Speicher für die Bearbeitung, wodurch es auch bei unendlichen Listen nicht zu einem Stack Overflow kommen kann.
hingegen benötigt `foldr` eine Menge an Speicher proportional zur Länge der verarbeitenden Liste. In beiden gibt es zusätzlich den Speicherbedarf für den Aufruf der Prozedur. Quelle: [Racket foldl](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29), [Racket foldl](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29)

Bei der Geschwindigkeit lässt sich schwer eine eindeutige Aussage bilden. Tendenziell ist es aufwendiger auf das letzte Element einer Liste zuzugreifen, als auf das erste, wodurch `foldl` in kombination mit dem Konstanten Speicherverbrauch tendenziell besser ist, 
jedoch kommt es stark auf die gewünschte Funktion an. Manche Funktionalitäten lassen sich besser mit `foldr`, manche mit `foldl` implementieren.

## Aufgabe 12
Zeigen Sie an zwei selbst erstellten Beispielen die Nutzung von Funktionen höherer Ordnung auf.

### Beispiel 1
Man hat eine Liste an Messdaten eines Sensors. Da der Sensor manchmal werte über 100 ausgibt, welche das System nicht weiter verarbeiten kann, müssen alle Werte über 100 auf 100 gesetzt werden: 
``` 
#lang racket

(define (adjust_limit lst)
  (map (lambda (a) (cond ((<= a 100) a) (else 100))) lst))
```

### Beispiel 2
Man hat eine Liste von dem monatlichen Brutto Gehalt der Mitarbeiter, und will herausfinden wie viele über 4.000 € verdienen:

``` 
#lang racket

(define (count-over-4000 lst)
  (length (filter (lambda (a) (cond ((> a 4000) #t) (else #f))) lst)))
```

### Anmerkungen
- Im Laufe dieses Portfolios wurden schon einige Anwendungsfälle von Funktionen höherer Ordnung verwendet. Die beiden hier gezeigten Funktionen dienen als konkrete Beispiele, für fiktive Szenarien, doch ist die Notwendigkeit der Funktionen auch bei anderen Aufgaben zu sehen.
- Ein großes Anwendungsgebiet für Funktionen höherer Ordnung ist im Umgang mit Listen.

## Aufgabe 13
Überführen Sie die Funktion `sum` in eine endrekursive Fassung. Die Funktion wurde wie folgt definiert:
```
(define (sum term a next b)
  (cond
    ((> a b) 0)
    (else (+ (term a)
             (sum term (next a) next b)))))
```

### Code
```
#lang racket

(define (sum term a next b)
  (define (sum-inner term a next b acc)
    (cond
      ((> a b) acc)
      (else (sum-inner term (next a) next b (+ acc (term a))))))

  (sum-inner term a next b 0))
```

### Anmerkungen
- Für die endrekursive Fassung wurde eine Anonyme-Hilfsfunktion angelegt, 
welche einen Akkumulator besetzt, welcher das Ergebnis jeder Funktionsiteration speichert und aufsummiert. 
So beinhaltet er nach der letzten Rekursion das finale Ergebnis und wird einfach zurück gegeben
- Die Anonyme Funktion wird initial beim Funktionsaufruf aufgerufen 
und anschließend endrekursiv als letzter Funktionsaufruf von sich selbst. 

## Aufgabe 14
Schreiben Sie eine Funktion insert-sort, die eine übergebene Liste von Zahlen mittels Sortieren durch Einfügen sortiert und das Ergebnis zurückliefert. Beispiele:
```
(insert-sort '(2 4 1 6 4))
'(1 2 4 4 6)
(insert-sort '())
'()
```

### Code
```
#lang racket

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
```

### Anmerkungen
- Bei ungültigen Eingaben wird die leere Liste zurückgegeben.
- Durch Anpassen der Konstanten `DIRECTION` lässt sich zwischen Max-Sort (`>=`) und Min-Sort (`<=`) wechseln. 
Theoretisch hätte man das auch als Funktionsargument übergeben können, aber die Aufgabenstellung verlangte folgende Form: `(insert-sort '(2 4 1 6 4))`

## Aufgabe 15
Schreiben Sie eine Funktion flatten, die eine beliebig tief verschachtelte Listen-Struktur in eine Liste "glättet":
```
(flatten '((a) b (c (d) e) ()))
'(a b c d e)
```

### Code
```
#lang racket

(define (flatten lst)
  (define (flatten-by-1 x lst)
    (cond
      ((list? x) (append x lst))
      ((empty? x) lst)
      (else (cons x lst))
    ))

  (cond
    ((ormap pair? lst) (flatten (foldr flatten-by-1 null lst)))
    (else lst)))
```

### Anmerkungen
- Es handelt sich hier wieder um eine Endrekursive Form
- Es wird gleichermaßen nach Paaren wie Listen gesucht, auch wenn im folgenden von Listen die Rede ist
- Die Funktion ist unabhängig vom Typ der Liste 
- Kernkomponente ist die Funktion `(flatten (foldr flatten-by-1 null lst))`, welche die Tiefe der Liste bei jedem Aufruf um 1 Reduziert, 
in dem sie für jedes Element von rechts nach links Prüft, ob es sich um eine Liste handelt?
  - Ja: Füge die Liste mit der Restliste zusammen
  - Nein: Füge es an den Anfang der Restliste
- Diese Funktion wird so lange aufgerufen, bis jedes Element der Liste keine Liste mehr ist.


## Aufgabe 16
Beschreiben Sie den Unterschied zwischen der Nutzung von `thunk` und den Promises (`delay` und `force`). 
Wann ist das Ergebnis gleich und wann unterscheidet es sich? 
Für welchen Anwendungszweck würden Sie welche Version einsetzen? 
Warum kann man delay nicht als Funktion schreiben?

### Antwort
Beide stellen Mechanismen da welche dazu dienen die Auswertung von Ausdrücken zu verzögern.

`thunk` ist hierbei eine eingewickelte Berechnung, also eine Funktion, welche in z.B. einem lambda-Ausdruck eingewickelt ist (z.B. `(lambda () (+ 2 3))`). 
Wird das `thunk`, also der lambda-Ausdruck nun als Argument an eine Funktion übergeben, so wird die eingewickelte Funktion trotz applicative order nicht direkt ausgewertet, 
sondern erst, wenn die Funktion tatsächlich im Funktionskörper benötigt wird. Wird das Ergebnis im Funktionskörper allerdings mehrfach benötigt, 
wird an jeder stelle an welcher das `thunk` benötigt wird, die Berechnung neu durchgeführt, also es findet keine Zwischenspeicherung des Ergebnisses statt.
<br>Diese Auswertungsstrategie bezeichnet man als `normal order`.

Bei einem Promise wird die Auswertung der Berechnung zwar ebenfalls verzögert, bis an den Punkt, an dem sie eigentlich benötigt wird. 
Allerdings wird das Ergebnis der Berechnung in dem Promise zwischengespeichert. Sollte das Ergebnis also erneut benötigt werden, 
muss die Berechnung also nicht erneut durchgeführt werden, sondern kann aus dem Speicher gelesen werden. 
<br>Diese Auswertungsstrategie bezeichnet man als `lazy evaluation`.

#### Wann ist das Ergebnis gleich und wann unterscheidet es sich?
Die reinen Ergebnisse der Auswertungen sind so lange identisch, solange es bei der Auswertung der Berechnung zu keinen Seiteneffekten oder Zustandsänderungen kommt, oder die Berechnung nur einmal stattfindet.
<br>Sollte die Funktion nämlich einen Seiteneffekt haben, oder eine Zustandsänderung auslösen, so würde eine erneute Auswertung ein anderes Ergebnis auslösen, oder den Zustand noch weiter verändern.
Da bei `lazy evaluation` die Auswertung maximal einmal stattfindet, und bei `normal order` auch mehrfach geschehen kann, würde das also zu verschiedene Resultaten führen.

Bei ihrem Ressourcenverbrauch können sich die beiden Verfahren jedoch unterscheiden, da Promises entsprechend Speicher benötigen, und das Ergebnis zwischenzuspeichern, jedoch wird bei einem erneuten Abfrangen keine weitere Berechnung druchgeführt.
<br>Das `thunk` braucht zwar keinen zusätzlichen Speicher, jedoch würde ein erneuter Aufruf wieder eine potenziell aufwendige Berechnung durchführen.

#### Für welchen Anwendungszweck würden Sie welche Version einsetzen?
Beide Funktionen sollten also verwendet werden, wenn eine Berechnung relativ aufwendig ist und nur unter gewissen Konditionen benötigt wird.
Bei der Wahl zwischen `thunk` und Promises kann als grobe Orienteering gesagt werden, 
wenn das Ergebnis der Berechnung an vielen Stellen benötigt wird, und nur einen geringen Speicheraufwand hat, sollten Promises verwendet werden. Wird das Ergebnis nur einmal benötigt, oder der Speicheraufwand ist im Verhältnis zum Rechenaufwand zu groß, sollte `thunk` verwendet werden.

#### Warum kann man delay nicht als Funktion schreiben?
In Racket arbeiten alle Funktionen nach `applicative order`, also die Argumente der Funktion werden direkt bei Aufruf der Funktion ausgewertet. 
Somit würde die Funktion, die man an das `delay` übergeben möchte, direkt ausgewertet werden und das Ziel von wäre verfehlt. 

Das eigentliche `delay` ist daher als special form implementiert, welches seine Argumente nicht auswertet. Auf diese Weise findet die auswertung erst durch den Aufruf von `force` statt. 
<br>(Special Forms sind in der Lage ihre Argumente nicht, oder auch nur teilweise auszuwerten, wie z.B. `and`)

**Anmerkung**: Wenn die Funktion, die man an eine selbstgebaute `delay`-Funktion übergeben will, in als `thunk` eingewickelt ist, 
würde die Auswertung auch verzögert werden und man könnte die selbstgebaute `delay`-Funktion nutzen.  
```
(define (my-delay thunk)
  (mcons #f thunk))
```


## Aufgabe 17
Implementierten Sie eine Funktion `fib-stream`, die einen Strom von zwei-elementigen Listen `(n (fib(n)))` erzeugt, wobei `n` die natürlichen Zahlen durchläuft. 
Die Funktion soll nicht jede Fibonacci-Zahl unabhängig berechnen.
```
> (define a (fib-stream))
> a
((0 0) . #<promise>)
> (tail a)
((1 1) . #<promise>)
> (tail (tail a))
((2 1) . #<promise>)
> (tail (tail (tail a)))
((3 2) . #<promise>)
> (tail (tail (tail (tail a))))
((4 3) . #<promise>)
> (tail (tail (tail (tail (tail a)))))
((5 5) . #<promise>)
```

### Anmerkungen
- Es wurden 2 versionen des Programms erstellt, 
wobei die Variante 1 eine relativ schöne wirkt, jedoch sehr inperformant ist, 
da es zum einen viel Speicher aber auch eine große Menge an Rechenleistung benötigt 
<br> so kann z.B. die Funktion `(get-at a 100)` an meinem Testgerät nicht zu Ende ausgewertet werden, wobei `a` der Stream ist und `100`, die stelle, die bei dem Stream erfragt wird.
- Die Variante 2 ist hierbei bei weiten performanter, so kann auch `(get-at a 100000)` nach kurzer verzögerung ausgewertet werden. Damit das so schnell funktioniert werden die ergebnisse der `fib` funktion in einer Hash-Table gespeichert.   

### Code 
#### Variante 1
```
#lang racket

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
```

#### Variante 2
```
#lang racket

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
```

#### Für Tests:
```
#lang racket

(define (get-at s n)
  (cond
    ((<= n 0) (head s))
    (else (get-at (tail s) (- n 1)))))
```


## Aufgabe 18
Gegeben sei die folgende Funktion in Typed Racket:
```
(: bar (-> (U Integer Boolean String) Integer))
(define (bar x)
  (cond
    ((number? x) (string-length (number->string x)))
    (else (string-length x))))
```
Wenn man diese Funktion versucht zu kompilieren, erhält man eine Fehlermeldung vom Type Checker. Erläutern Sie die Fehlermeldung und korrigieren Sie die Funktion.

### Fehlermeldung
```
Type Checker: type mismatch
expected: String
given: (U Boolean String) in: x
```

### Erklärung
Die Typsignatur der Funktion gibt an, dass es sich bei `x` um eine Union-type handelt, als `x` einen `Integer`, `Boolean` oder `String` handeln kann, 
die Funktion `string-length` kann jedoch nur Strings verarbeiten. 
Deshalb wurde eine verzweigung eingebaut, in welche sich um `Integer` kümmert, der Funktion verarbeitet werden können.
Jedoch kommen auf diese Weise im Else-Zweig das `x` immer noch ein `Boolean` oder `String` sein. Die Funktion verlangt jedoch eindeutig den `String` Typ.

Add können wir wie Folgt ablesen: 
- `Type Checker: type mismatch [...] in: x` -> Die Typsignatur simmer bei `x` an der Markierten nicht überein.
- `expected: String`: Die Funktion erwartet ausschließlich den Typ `String`, welcher `x` nicht oder nicht ausschließlich hat. 
- `given: (U Boolean String)`: `x` ist ein Union-type und hat zwar u.a. den Typ `String`, kann jedoch auch ein `Boolean` sein.

### Lösung
Es muss ein weiterer Konditionszweig für `Boolean` einfügt werden.
<br> Da der String `"#t"` und `"#f"` je 2 Zeichen lang sind, kann man für den Zweig also 2 zurückgeben.

```
#lang typed/racket

(: bar (-> (U Integer Boolean String) Integer))
(define (bar x)
  (cond
    ((number? x) (string-length (number->string x)))
    ((boolean? x) 2) ; string-length("#t") = string-length("#f") = 2
    (else (string-length x))))
```

## Aufgabe 19
Schreiben Sie ein `map` und ein `foldr` in Typed Racket.

### Code
```
#lang typed/racket

; 1
(: my-map (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (my-map f lst)
  (: my-map-inner (All (A B) (-> (-> A B) (Listof A) (Listof B) (Listof B))))
  (define (my-map-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-map-inner f (rest lst) (append acc (list (f (first lst))))))))

  (my-map-inner f lst '()))

; 2
(: my-foldr (All (A B) (-> (-> A B B) B (Listof A) B)))
(define (my-foldr f start lst)
  (: my-foldl-inner (All (A B) (-> (-> A B B) (Listof A) B B)))
  (define (my-foldl-inner f lst acc)
    (cond
      ((empty? lst) acc)
      (else (my-foldl-inner f (rest lst) (f (first lst) acc)))))

  (my-foldl-inner f (reverse lst) start))
```

### Anmerkungen
- Für die Implementierung wurde eine Implementierung von `foldr` wurde die Implementierung von `my-foldl-inner` (Aufgabe 9) übernommen 
und die Liste wurde mit `reversed` invertiert. 
- Auf diese Weise muss zwar bei der Rekursion nicht jedes Mal aufwendig an das Ende der Liste gesprungen werden, dafür findet allerdings eine Aufwendige Invertierung der Liste statt. 

## Aufgabe 20
Schreiben Sie ein Makro `infix`, mit dem Sie die Addition zweier Zahlen in Infix-Schreibweise errechnen lassen können. Anwendungsbeispiel: `(infix 1 + 2)`

### Code
```
#lang racket

(define-syntax-rule (infix num1 op num2)
  (op num1 num2))
```

### Anmerkungen
- Das Makro ermöglicht die Infix-Notation für arithmetische Operationen, statt die übliche Präfix-Schreibweise