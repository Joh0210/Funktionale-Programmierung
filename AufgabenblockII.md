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

## 2 Todo!
## 3 Todo!
## 4 Todo!
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

