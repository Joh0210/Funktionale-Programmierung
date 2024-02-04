# Aufgabenblock IV

## Aufgabe 1
Haben Sie bereits mit einer funktionalen Programmiersprache Kontakt gehabt? Privat, im Betrieb? Wenn ja, welche war es? Wie hat diese Sprache die vorgestellten Konzepte (statische vs. dynamische Bindung, eager vs. lazy evaluation, strikt vs. nicht strikt) umgesetzt?

### Antwort:
Bislang hatte ich gewusst, dass es das funktionale Paradigma gibt, jedoch hatte ich es vor diesem Semester noch nicht angewendet, und hatte entsprechend zuvor noch keinen Kontakt mit einer funktionalen Programmiersprache.

## Aufgabe 2
Mit welchen Programmiersprachen und Paradigmen hatten Sie bisher Kontakt? 
Auf welche Schwierigkeiten (wenn überhaupt) stoßen Sie beim Erlernen des Programmierparadigmas der funktionalen Programmierung? Worin sehen Sie die Ursachen?

### Antwort 
An Programmiersprachen nutzte ich die folgenden:
- C/C++
- Java
- JavaScript
- Python
- Racket
- SQL
- Xtend

Es handelt sich hierbei hauptsächlich um objektorientierte Programmiersprachen.

Meine größte Schwierigkeit bestand drain, meine Denkweise auf die funktionale Programmierung anzupassen, 
da ich gewohnt bin Objekte und Kontrollstrukturen zu nutzen, welche in der funktionalen Programmierung nur sehr geringfügig vorkommen, wenn überhaupt.

## Aufgabe 3
Sehen Sie in Typed Racket Vorteile oder Nachteile gegenüber dem "normalen" Racket? Wenn ja, welche?

### Antwort
Den erwähnten Gründen für Typed Racket aus Aufgabenblock II, Aufgabe 9 stimme ich sehr zu. 
Vor allem die Übersichtlichkeit und das automatische Finden von logischen Fehlern in einem Programm sind sehr praktisch.

Jedoch stellte ich fest, dass ich bei der Entwicklung deutlich mehr Zeit benötige, wenn ich die Typisierung verwende. 
Gleichermaßen kommt es oftmals zu Komplikationen, wie ich in Aufgabenblock I, Aufgabe 1 erwähnte. 

Insgesamt erschien es mir also zu Aufwändig, um es für das gesamte Portfolio zu verwenden.

## Aufgabe 4
Vergleichen Sie das Typsystem von Typed Racket mit einem anderen Ihnen bekannten Typsystem. Welche Stärken und Schwächen der beiden Systeme können Sie ausmachen?

### Antwort
Mir erscheint das Typsystem an einigen stellen deutlich detaillierter zu sein, als z.B. in Java. z.B. 1 ist nicht nur eine `Number`, sondern auch eine `Positive Number` und genauer sogar eine `One`.
Dieser Detailgrad wird anschließend auch von einigen Funktionen unterstützt, so kann bei der Addition zweier Positiven Zahlen nur eine Positive Zahl hervorkommen.

Das sorgt jedoch auch für ein paar Probleme. 
1. Wenn dieser Detailgrad unterstützt werden soll, müssen Entwickler ihre Funktionen auch entsprechend umfangreich annotieren. Andernfalls gehen die zusätzlichen Informationen verloren.
2. Die Statische Codeanalyse prüft die einzelnen Typen ausschließlich bei der Funktion selbst, oder bei `type?`-Abfragen.
<br>Aufgrund dessen kann es passieren, dass ein gewisser Fall eigentlich nicht eintreten kann, da er von einem anderen Konditionszweig abfangen wird, ohne dass das Typsystem es erkennt. (vgl. Aufgabenblock I, Aufgabe 1)

## Aufgabe 5
Halten Sie Programmiersprachen mit dynamischer oder statischer Typprüfung für "besser"? Für welchen Einsatzzweck würden Sie die eine oder andere Typprüfung empfehlen?

### Antowrt
Statische Typprüfung, da auf diese Weise bereits vor Laufzeit des Programmes fehler erkannt werden können. 
Denn bei reiner dynamischen Typprüfung kann es bei umfangreichen programmen passieren, das manuelle Tests gewisse Codeabschnitte oder Grenzfälle nicht abdecken. 
So kommt es erst deutlich später, potenziell während laufendem Betrieb, zu dem Fehler der potenziell durch statische Typprüfung schon früher hätte erkannt werden können.

Bei kritischen Systemen halte ich statische Typprüfung für besonders wichtig. Bei kleinen Projekten oder "Spaß-Projekte" ist dynamischen Typprüfung auch mehr als ausreichend.

## Aufgabe 6
Zum Ende des Semesters:
- Welche Inhalte haben Sie besonders interessiert?
- Welche Inhalte haben Einfluss auf Ihr zukünftiges Programmieren?
- Ich hatte Ihnen am Anfang des Semesters das Zitat von Alan Perlis präsentiert: "A language that doesn’t affect the way you think about programming is not worth knowing." Hat das Modul und Racket Ihre Art über Programmieren zu denken verändert? Wenn ja, in welcher Weise? Wenn nein, warum denken Sie, dass es das nicht getan hat?
- Wie schätzen Sie den Aufwand für die Anfertigung des Portfolios ein? Größer oder kleiner als den Lernaufwand für eine Klaur?
- Halten Sie den Umfang der Aufgaben für zu groß, gerade richtig oder zu gering?

### Welche Inhalte haben Sie besonders interessiert?
Das Nutzen und Erstellen von Funktionen höherer Ordnung fand ich besonders interessiert.

### Welche Inhalte haben Einfluss auf Ihr zukünftiges Programmieren?
Ich werde eher darauf achten, ob es sich lohnen könnte Funktionen höherer Ordnung zu erstellen, und potenziell umsetzen. (z.B. mit Lambdafunktionen in Java)

### Ich hatte Ihnen am Anfang des Semesters das Zitat von Alan Perlis präsentiert: "A language that doesn’t affect the way you think about programming is not worth knowing." Hat das Modul und Racket Ihre Art über Programmieren zu denken verändert? Wenn ja, in welcher Weise? Wenn nein, warum denken Sie, dass es das nicht getan hat?
Das funktionale Paradigma hat definitiv meine Herangehensweise an die Programmierung beeinflusst, da ich für die Bearbeitung keine Objektorientierung nutzen konnte, und mir andere Verfahren überlegen musste.
Diese neuen Verfahren werde ich auch für zukünftige Projekte nutzen, jedoch werde ich sie wahrscheinlich auf die Objektorientierung anpassen.

### Wie schätzen Sie den Aufwand für die Anfertigung des Portfolios ein? Größer oder kleiner als den Lernaufwand für eine Klausur?
Der Aufwand war mindestens genauso groß, wenn nicht noch größer. Denn wie auch bei Klausuren schrieb ich mir während den Vorlesungen einen Lernzettel, welchen ich für die Bearbeitung des Portfolios nutze. 
Da die erstellung des Lernzettels ein großteil des Aufwandes bei der Prüfungsvorbereitung ist, ist das Zeitersparnis des Lernaufwands also etwas geringer.
Betrachtet man nun den sehr großen Umfang des Portfolios, ist der Zeitaufwand, den ich für die Klausurvorbereitung benötigen würde, weit überschritten.

### Halten Sie den Umfang der Aufgaben für zu groß, gerade richtig oder zu gering?
Meiner persönlichen Meinung nach, sind die Aufgaben deutlich zu groß für die entsprechenden Punkte.

Der Aufgabenblock I bringt lediglich 25%, hat jedoch 20 Aufgaben. Der Umfang variate zwar,
jedoch fehlte grade zu Beginn die Übung und eine Aufgabe konnte sich auch mal über Student ziehen. Wenn eine solche Aufgabe nur ungefähr 1% der Note ausmacht, ist ein wenig "verwirrend". 

## Aufgabe 7
Wenn Sie das Modul noch einmal wählen könnten, würden Sie es gerne in Präsenz oder wie durchgeführt online belegen?

### Antwort
Weiterhin Online.