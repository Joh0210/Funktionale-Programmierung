# Aufgabenblock III

## Aufgabe 1 
Berichten Sie aus Ihrer bisherigen Programmierpraxis (sei es im Studium oder im Betrieb): 
Mit welchen Sprachen haben Sie bisher gearbeitet und welche Aufgaben-/Aufgabentypen haben Sie bearbeitet?

### Programmiersprachen

- C/C++
- Java
- JavaScript
- Python
- Racket
- SQL
- Xtend

### Aufgabentypen
- Algorithmus-Entwicklung
- Automatisierung 
- Benutzeroberflächen Entwicklung
- Datenbankprogrammierung
- Mobile App-Entwicklung
- Qualitätssicherung/Testing 
- Spieleentwicklung/-erweiterung

## Aufgabe 2
Welche Programmierprojekte haben Sie in letzter Zeit umgesetzt? Erläutern Sie mindestens zwei bis drei.

### Anmerkung
Aus Vertraulichkeitsgründen werden hier keine Projekte der vergangenen Praxisphasen bei der Continental Automotive Technologies GmbH erläutert, sondern lediglich Projekte des Studiums und der privaten Implementierung.

### Minecraft Mod
Minecraft-Mods sind Erweiterungen, die von der Minecraft-Community in Java oder Kotlin erstellt wurden, um das ursprüngliche Spiel zu verändern oder neue Gegenstände und Funktionen hinzuzufügen. 

Minecraft selbst ist ein beliebtes Sandbox-Spiel, bei dem Spieler in einer offenen Welt mit Blöcken bauen und erkunden können. 
Mods ermöglichen es, diese Erfahrung zu personalisieren und auf die verschiedensten Arten zu erweitern.

Seit einem Jahr arbeite ich iterativ-inkrementell an zwei solcher Erweiterungen für das Spiel. Die Mods [Dragon Magic And Relics](https://github.com/Joh0210/DragonMagicAndRelics) und [Factions And Curiosities](https://github.com/Joh0210/FactionsAndCuriosities) fügen dem Spiel eine große Menge an neuen Gegenständen, Ritualen und Zauber hinzu.
<br>Zusätzlich fügen sie komplett neue Spielmechaniken hinzu, welche den Kern der Erweiterungen darstellt. Diese ermöglichen es dem Spieler Magie auf eine neue, personalisierte Weise zu nutzen.

### Android Algorithmen-Lern App: Heap Sort
Für das Modul "Mobile Technologien", des 4. Semesters der Softwaretechnologie mit Prof. Dr. Schultes, musste eine bereits existierende Lern-App erweitert werden.

Die App dient als Lernmaterial für das Modul "Algorithmen und Datenstrukturen", des 2. Semesters der Softwaretechnologie, in welchem der Begriff des Algorithmus vertieft und theoretisch untersucht wird. 
Hierfür kommen einige Beispielalgorithmen zum Einsatz, welche ebenfalls verstanden werden müssen. Um den Studierenden das Verständnis zu erleichtern und beim Lernen zu helfen, 
kommt diese Lern-App zum Einsatz, welche einige dieser Algorithmen schrittweise erklärt. 

Grundlegend besteht die App aus 2 Ansichten, eine für den Spielmacher und eine für den Spieler. In beiden Ansichten gibt es anschließend ein Menüfeld,
in welchen man einen Algorithmus aussuchen kann. Aus Spieleransicht wird einem der entsprechende Algorithmus schrittweise erklärt und man bekommt eine Handvoll Informationen über den Algorithmus, wie z.B. über die Komplexität.
Zusätzlich kann der Spieler den Algorithmus manuell durchführen, um zu prüfen, ob er ihn richtig verstanden hat. 

Als Spielmacher besitzt man die Möglichkeit einen Wettbewerb zu öffnen, an welchem die Spieler teilnehmen können, um gegeneinander zu spielen. Hierfür müssen die Spieler angemeldet sein und Internetzugriff haben. 

Die Grundstruktur der Mobilen Android-Anwendung wurde von Prof. Dr. Schultes erstellt und iterativ-inkrementell in dem Modul "Mobile Technologien" erweitert.
<br>Die Studierenden sollen als Gruppe von bis zu fünf Personen je einen Menüpunkt hinzufügen, welcher die beschriebenen Funktionalitäten erfüllt. 
Für die Server-Kommunikation des Wettbewerbsmodus und das "Hauptmenü" stand eine Grundstruktur zur Verfügung, der Rest musste von den Studierenden in Java entwickelt werden. 

Als Gruppe, bestehend aus Tobias Kapitza, Josias Klaus und mir, entschieden wir uns für den Heap Sort Algorithmus.
Wir implementierten des Weiteren einige Zusatzziele, wie UI-Tests, ein Trophäen-System für den Wettbewerbsmodus und einer persistenten Speicherung der Übungsresultate, um den Lernerfolg zu protokollieren.

## Aufgabe 3
Erläutern Sie an Ihren Beispielen, welche Architektur und welches Paradigma hierbei zur Anwendung kamen.

### Minecraft Mod
Um eine Minecraft Mod zu schreiben, muss an eine der beiden Schnittstellen [Forge](https://files.minecraftforge.net/net/minecraftforge/forge/) oder [Fabric](https://fabricmc.net) angeknüpft werden, um das Grundspiel zu erweitern.
In beiden Fällen kann in Java oder auch Kotlin programmiert werden, deren objektorientiertes Paradigma in großen Maßen genutzt wird. 

Für meine Projekte entschied ich mich für Java, da es auch die hauptsächlich genutzte Sprache der Vorlesungen war. 

Die grundlegende Architektur ist bei beiden Schnittstellen ähnlich, muss jedoch penibel eingehalten werden.  

Es handelt sich hierbei um ein Gradle-Projekt, welches die eigentliche Logik der Mod und dessen Ressourcen strikt trennt.
Die Logik wird zwar durch die Erweiterung definiert, jedoch können Spieler einzelne Ressourcen nach Belieben anpassen, um so z.B. Texturen oder Rezepte zu verändern.

Beispiel Architektur für Forge:
- gradle/wrapper/ 
- src/main -> Der komplette Inhalt der Mod 
  - java/ -> In diesem Verzeichnis ist der Programmcode, welcher die Logik definiert
  - resources/ 
    - assets/ -> Texturen, Übersetzungen, etc. die je Client individuell sind
    - data/ -> Rezepte und andere Regeldefinitionen, welche von dem Spiel-Server festgelegt werden und bei jedem Client identisch sind
    - META-INF/ -> Informationen über die Mod, welche von der Schnittstelle benötigt werden
- build.gradle 
- gradle.properties 
- gradlew 
- gradlew.bat 

Die grundlegende Logik lässt sich stark vereinfacht in 3 Haupt-Punkte unterteilen:
- Neue Objekte Registrieren
- Auf Ereignisse Reagieren
- Komplett neue Spielmechaniken hinzufügen

#### Neue Objekte Registrieren
In Minecraft gibt es Items, Blöcke, Zaubertränke, Kreaturen und einiges mehr, welches ebenfalls durch Mods hinzugefügt werden kann.
<br>Möchte man das tun, muss man eine neue Instanz von der entsprechenden (davon erbenden) Klasse erstellen, mit einer ID versehen und registrieren. 

```java
public class ItemInit {
    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, DragonMagicAndRelics.MOD_ID);

    public static final RegistryObject<Item> DRAGON_CORE = ITEMS.register("dragon_core", () -> 
            new Item(new Item.Properties().fireResistant().rarity(Rarity.EPIC).tab(CreativeModeTab.CreativeModeTab)));
    public static final RegistryObject<Item> MANA_CAKE = ITEMS.register("mana_cake", ManaCakeItem::new);

    public static void register(IEventBus eventBus){
        ITEMS.register(eventBus);
    }
}
```

Soll das Objekt zusätzliche Funktionalitäten beinhalten, lassen sich viele davon umsetzen in dem die ursprüngliche Klasse erweitert wird und einige Parent- oder Interface-Methoden überschrieben werden. Eine Instanz dieser Klasse kann anschließend registriert werden:

```java
public class ManaCakeItem extends Item {
    private static final int NUTRITION = 4;
    
    private static final float SATURATION = 0.4f;

    public ManaCakeItem() {
        super(new Item.Properties().tab(CreativeModeTab.CreativeModeTab).stacksTo(1).rarity(Rarity.COMMON)
                .food((new FoodProperties.Builder()).nutrition(NUTRITION).saturationMod(SATURATION).alwaysEat().build()));
    }

  @Override
  public @NotNull ItemStack finishUsingItem(@NotNull ItemStack itemstack, @NotNull Level world, @NotNull LivingEntity entity) {
    super.finishUsingItem(itemstack, world, entity);
    return new ItemStack(ItemInit.MANA_CAKE.get());
  }
}
```

#### Auf Ereignisse Reagieren
Einige Funktionalitäten von neuen Objekten lassen sich nicht allein durch Überschreiben von Klassenmethoden realisieren. 
Ist das der Fall, muss in der Regel ein Event Handler erstellt werden.

Minecraft ist Event-Driven aufgebaut, also bei jedem Ereignis, das in der Welt passiert, wird ein entsprechendes Event generiert. 
Wenn z.B. ein Spieler springt, wird ein Objekt der Klasse `LivingJumpEvent` erzeugt.
Diese Events sind nach dem Observer-pattern aufgebaut, damit sich sogenannte Event Handler von Mods als Listener registrieren und auf das Event reagieren können. 

Hat sich z.B. ein Event Handler für das `LivingJumpEvent` registriert und ein Spieler springt, wird die entsprechende Funktion des Event Handler aufgerufen. 
Die Instanz des `LivingJumpEvent` beinhaltet dabei alle Informationen, die für die Verarbeitung notwendig sind. Durch Manipulation der einzelnen Attribute der Instanz lässt sich dann mit der Welt interagieren. 

```java
@Mod.EventBusSubscriber(modid = DragonMagicAndRelics.MOD_ID, bus = Mod.EventBusSubscriber.Bus.FORGE)
public class CommonEventHandler {
    @SubscribeEvent
    public static void onLivingJump(LivingEvent.LivingJumpEvent event) {
        if(event.getEntity() instanceof Player player && !player.getLevel().isClientSide()){
            if (player.isSprinting()) {
                player.push(player.getDeltaMovement().x * 0.2f, 0.6f, player.getDeltaMovement().z * 0.2f);
                player.hurtMarked = true;
            }
        }
    }
}
```

Auch eigene Events, auf die andere Modmacher reagieren können, lassen sich erstellen, aber das ist etwas zu weitgreifend für das Portfolio.

#### Komplett neue Spielmechaniken hinzufügen
Die beiden oberen Erweiterungsmöglichkeiten sind klar definiert, wie sie umzusetzen sind. 
Möchte man jedoch eine komplett neue Spielmechanik hinzufügen oder Hilfsklassen erstellen, ist der Entwickler sehr frei in der Umsetzung.

In den meisten Fällen gibt es hierfür eine Klasse, welche die eigentliche Spielmechaniken-Logik beinhaltet. 
Diese kann nach Belieben implementiert werden und wird standardmäßig über ein selbst definiertes Interface angesteuert. 
In einem zweiten Schritt wird das Interface mit den benötigten Events durch ein Event Handler verknüpft.

Dieser Abschnitt ist bei weitem der komplizierteste, bietet jedoch auch den meisten Freiraum in der Umsetzung.

### Android Algorithmen-Lern App: Heap Sort
Für die Android nativ Entwicklung können standardmäßig ebenfalls die beiden objektorientierten Sprachen Java und Kotlin verwendet werden.

Auch hier werden die Ressourcen (z.B. Übersetzungen, Layouts, Bilder, ...) streng von der Logik getrennt, sodass sich leicht, auch ohne Programmierkenntnisse, Anpassungen wie z.B. Übersetzungen vornehmen lassen.

Damit es wenig ungewollte Interferenzen zwischen den Teams gab, wurde innerhalb des Haupt-Packages ("Root Verzeichnis" für den Java Code), 
für jedes Team ein Sub-Package erstellt, in welchem sie ihre Logik implementieren sollten. Die erstellten Ressourcen wurden mit einem Präfix entsprechend der Team-ID versehen.
Auf diese Weise kommt es zu möglichst wenig Problemen, wenn mehrere Teams an demselben Programm arbeiten.

Eine typische Strategie, welche ebenfalls von uns umgesetzt wurde, ist die Frontend- und Backendlogik voneinander zu trennen.

#### Frontendlogik
Das, was der Nutzer einer Android-Nativ-App sieht, sind sogenannte `Activities`. Diese sind Instanzen einer entsprechenden Activity-Klasse.
Die Elemente und das Layout der Activity, also z.B. ein Knopf, mit besonderer Farbe, Text im Inneren und wo auf dem Bildschirm er sich befindet, 
wird durch eine verknüpfte Ressource definiert.
Was die einzelnen Elemente jedoch machen, z.B. was passiert, wenn der Nutzer einen Knopf drückt, oder der Livecycle der Activity wird durch die entsprechende Klasse definiert, 
in dem ebenfalls Parent-Methoden überschrieben und Objekte manipuliert werden. 

```java
public class MainActivity extends AppCompatActivity {

  private ActivityMainBinding binding;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    binding = ActivityMainBinding.inflate(getLayoutInflater());
    setContentView(binding.getRoot());

    AppBarConfiguration appBarConfiguration = new AppBarConfiguration.Builder(
            R.id.navigation_home, R.id.navigation_dashboard, R.id.navigation_notifications)
            .build();
    NavController navController = Navigation.findNavController(this, R.id.nav_host_fragment_activity_main);
    NavigationUI.setupActionBarWithNavController(this, navController, appBarConfiguration);
    NavigationUI.setupWithNavController(binding.navView, navController);
  }
}
```

Neben Activities können auch Fragments verwendet werden. Instanzen dieser Klasse werden dem Nutzer ebenfalls angezeigt, 
jedoch nehmen sie nicht den ganzen Bildschirm ein, sondern sind oftmals Teil einer Activity.

In beiden Fällen muss die Objektorientierung für die Logik des Frontends genutzt werden.

#### Backendlogik
Die Backendlogik ist auch in Java oder Kotlin verfasst, jedoch ist der Entwickler hier sehr frei in der Umsetzung. 
Er muss keine speziellen Design Patterns einhalten, kann sie jedoch nach Belieben implementieren oder andere Herangehensweisen nutzen.

## Aufgabe 4
Ließen sich die Projekte oder Aufgaben auch mit dem Paradigma der funktionalen Programmierung umsetzen? Begründen Sie Ihre Antwort.

### Antwort
Nein. In beiden Fällen handelt es bei den Projekten um eine Erweiterung eines bereits existierenden Systems. 
Auf diese Weise wird zum einen die Programmiersprache Java (oder Kotlin) vorgegeben, welche auf das Paradigma der Objektorientierung ausgelegt ist, 
jedoch ließe sich auch damit das funktionale Paradigma umsetzen.

Zum anderen wird jedoch auch die Architektur der Logik-Komponenten vorgegeben. 
So muss, wie in Aufgabe 3 erklärt, ein neues Objekt in Minecraft immer als Instanz einer Klasse registriert werden. 
Auch die Activities von Android sind ebenfalls Instanzen der entsprechenden Klassen. 
<br>Auch der Inhalt dieser Klassen kann nur sehr begrenzt funktional umgesetzt werden, 
da die Funktionalität hauptsächlich durch Überschreiben von Parent-Methoden und Objektmanipulation stattfindet.

Auch das Reagieren auf Ereignisse in Minecraft nutzt ebenfalls Objekte als Kommunikationsmedium 
und nur durch dessen Manipulation lässt sich mit dem Spiel interagieren.

Funktionale Programmierung lässt zwar bedingt auch die Nutzung von Objekten zu, jedoch geht es in diesen Fällen hauptsächlich um die bewusste Nutzung von Seiteneffekten, 
welche dem Paradigma der funktionalen Programmierung wieder strebt. 

Jedoch lassen sich potenziell einige Teile mithilfe des funktionalen Paradigmas umsetzen. 

So muss zwar an einem gewissen Punkt die Backendlogik einer Android-App wieder mit der Objektorientierung der Frontendlogik verbunden werden, bzw. 
die Logik der komplett neuen Spielmechaniken in Minecraft muss mit dem Event-System verbunden werden, jedoch gibt es keine Vorgaben, wie die interne Logik aufgebaut sein muss.
Diese Abschnitte müssten zwar weiterhin in Java (oder Kotlin) implementiert werden, jedoch ist es theoretisch möglich auch andere Herangehensweisen zu nutzen.

Hierbei ist allerdings zu beachten, dass Rekursionen, auch Endrekursive-Rekursionen, in Java nicht optimiert ist 
und im großen Umfang für ein ineffizientes Verhalten sorgen können.

### Alternative Frage:
Ließen sich die Basis-Projekte auch mit dem Paradigma der funktionalen Programmierung umsetzen? Begründen Sie Ihre Antwort.

#### Minecraft
Auch funktionale Programmiersprachen ermöglichen die Entwicklung von Spielen, jedoch ist Minecraft über die Jahre hinweg extrem umfangreich geworden und stark mit dem Paradigma der Objektorientierung verknüpft. 
Sich davon zu trennen wäre wahrscheinlich möglich, bedeutet jedoch, dass die komplette Architektur, die über viele Jahre hinweg gewachsen ist, von Grund auf überarbeitet werden muss. 
Wie das die Performance beeinflussen würde, wird nicht weiter untersucht.

Gleichermaßen ist ein großer Bestandteil für viele der Spieler, die Verfügbarkeit von Mods, welche von der Minecraft-Community bereitgestellt werden.
Ich stelle die Vermutung auf, dass diese Fülle an über 10.000 Mods [[5]](https://www.curseforge.com/minecraft) unter anderem deshalb möglich gewesen ist, 
weil eine weit verbreitete Programmiersprache für die Entwicklung verwendet werden kann. Da funktionale Sprachen deutlich weniger verbreitet sind 
hätte es wahrscheinlich deutlich weniger Entwickler in der Minecraft-Community gegeben, welche in der Lage gewesen wären, das Spiel, um die Inhalte zu erweitern. [[6]](https://de.statista.com/statistik/daten/studie/678732/umfrage/beliebteste-programmiersprachen-weltweit-laut-pypl-index/)
<br>Hierbei handelt es sich jedoch nur um eine begründete Vermutung und keinen Beleg.  

Zusammenfassend lässt sich vermuten, dass es rein funktional umsetzbar wäre, 
jedoch würde dies eine grundlegend andere Architektur bedeuten und hätte wahrscheinlich eine deutlich kleinere Modding-Community zur Folge, 
was auch für den Erfolg des Spiels hinderlich gewesen sein könnte.

#### Android Algorithmen-Lern App
Android Native Apps lassen sich auch mithilfe von funktionalen Programmiersprachen, wie Haskell, implementieren [[7]](https://stackoverflow.com/questions/5151858/running-a-haskell-program-on-the-android-os), [[8]](https://github.com/neurocyte/android-haskell-activity).

Bei dem Projekt kamen einige objektorientierte Design-Patterns zum Einsatz, jedoch in einer deutlich loseren Struktur als z.B. bei Minecraft. 
Auf diese Weise müssen zwar bestehende Konzepte neu geplant werden, 
jedoch müsste nicht grundlegend neu definiert werden was Activities sind und wie sie sich verhalten, 
da die Android-Umgebung nicht überarbeitet werden müsste, da z.B. Haskell hierfür als Schnittstelle dient.

Die Backendlogik wurde von Gruppe zu Gruppe unterschiedlich implementiert, jedoch handelt es sich hierbei hauptsächlich um Algorithmen, die entsprechend auch mit funktionalen Verfahren implementiert werden können.

Also Ja, das Projekt hätte, von Grund auf mit der funktionalen Programmierung umgesetzt werden können.

**Anmerkung**: Der hauptsächliche Grund, der dagegen spricht ist, dass die Programmiersprache Java den Studierenden bereits im 1. Semester beigebracht wurde. Eine funktionale Sprache hätte erst vermittelt werden müssen, wodurch andere Themen des Lehrplans gekürzt worden wären. 
Zusätzlich lässt sich auch sagen das Java zwar immer weniger für die Appentwicklung verwendet wird, aber immer noch deutlich mehr als funktionale Sprachen, wodurch es sinnvoll sein kann diesen Prozess eher mit Java zu üben. [[6]](https://de.statista.com/statistik/daten/studie/678732/umfrage/beliebteste-programmiersprachen-weltweit-laut-pypl-index/)

### Quellen
- [5] **Curseforge**: [Minecraft Mods](https://www.curseforge.com/minecraft) o.J., https://www.curseforge.com/minecraft, 29.12.2023
- [6] **Statista**: [Die beliebtesten Programmiersprachen weltweit laut PYPL](https://de.statista.com/statistik/daten/studie/678732/umfrage/beliebteste-programmiersprachen-weltweit-laut-pypl-index/) o.J., https://de.statista.com/statistik/daten/studie/678732/umfrage/beliebteste-programmiersprachen-weltweit-laut-pypl-index/, 29.12.2023
- [7] **Stackoverflow**: [Running a Haskell program on the Android OS](https://stackoverflow.com/questions/5151858/running-a-haskell-program-on-the-android-os) 2011, https://stackoverflow.com/questions/5151858/running-a-haskell-program-on-the-android-os, 29.12.2023
- [8] **Neurocyte**: [android-haskell-activity](https://github.com/neurocyte/android-haskell-activity) o.J., https://github.com/neurocyte/android-haskell-activity, 29.12.2023

## Aufgabe 5
Entwerfen Sie funktionale Implementierungsskizzen Ihrer Beispiele. 
Sollte sich keines Ihrer Beispiele dafür eigenen, so überlegen Sie ein fiktives Projekt, Programmieraufgabe, die sich gut mit funktionaler Programmierung umsetzen ließe.

### Minecraft Mod
Wie bereits in Aufgabenblock III: Aufgabe 4 erwähnt, lassen sich keine Mods rein funktional umsetzen, lediglich einzelne Teilausschnitte. 
Eine vollständige funktionale Architektur-anpassung und Reimplementierung von Minecraft überschreitet hier auch bei weitem den Umfang des Projektes.
<br>Das folgende Flussdiagramm zeigt daher eine funktionale Umsetzung einer komplett neuen Spielmechanik, wie sie in Aufgabenblock III: Aufgabe 3 beschrieben ist.

Bei dem dargestellten Prozess handelt es sich um ein Kernelement der von mir erstellten Erweiterung [Factions And Curiosities](https://github.com/Joh0210/FactionsAndCuriosities). 
Es handelt sich dabei um zufällige Ereignisse, sogenannte "Wild Magic", die eintreten, wenn der Spieler einen Zauber nutzt.  

Diese Ereignisse müssen als Instanz einer Klasse registriert werden und die Interaktion mit der Welt findet durch Objektmanipulation statt.
<br>Instanzen der `WildMagic` Klasse verfügen zusätzlich über Funktionen, welche beschreiben, wie gut das Ereignis für den Nutzer ist, wie häufig das Ereignis ist und ob alle Konditionen für das Eintreten erfüllt sind.
<br>Zusätzlich wird ein Event Handler erstellt, welcher beim Einsetzen eines Zaubers aktiv wird und mithilfe der `WildMagicHelper` einen zufällige, aber gewichtete und ausführbare "Wild Magic" auswählt und ausführt.

Die Instanzen der `WildMagic` Klasse und der Handler sind zwangsläufig objektorientiert zu erstellen, da sich der Architektur von Minecraft angepasst werden muss (vgl. Aufgabenblock III: Aufgabe 3).
Der `WildMagicHelper` ist jedoch an keine solche Restriktion gebunden und kann entsprechend auch funktional umgesetzt werden. Hierfür eigenen sich besonders Streams. 

Nicht für die Erklärung relevante Zwischenschritte sind nicht in der Darstellung enthalten.

#### Initialisierung
![minecraft-Initialisierung.png](https://github.com/Joh0210/Funktionale-Programmierung/blob/main/draw_io/minecraft-Initialisierung.png?raw=true)

#### Spell Cast Event
![minecraft-SpellCastEvent.png](https://github.com/Joh0210/Funktionale-Programmierung/blob/main/draw_io/minecraft-SpellCastEvent.png?raw=true)

### Android Algorithmen-Lern App: Heap Sort
Dieser Abschnitt versucht, wie in Aufgabenblock III: Aufgabe 4 beschreiben, nicht nur die tatsächliche Erweiterung des Endanwendung, sondern das grundlegende Programm auf funktionale Weise dazustellen. 
Ein gewisser Grad an Seiteneffekten und Zustandsorientierung lässt sich dabei trotzdem nicht vermeiden.

Einzelne Zwischenschritte sind nicht ausformuliert, lassen sich jedoch meistens funktional umsetzen, wie in Aufgabenblock III: Aufgabe 6 zu sehen ist.
Um das Paradigma auf die Architektur anzuwenden, wird möglichst viel über einen Informationsfluss gelöst. Dieser Fluss lässt sich anschließend durch Funktionen oder Promises beschreiben.

Der Nutzer hat jederzeit die Möglichkeit die Anwendung zu beenden.
<br>Nicht für die Erklärung relevante Zwischenschritte sind nicht in der Darstellung enthalten.

#### App Start
![Android-AppStart.png](https://github.com/Joh0210/Funktionale-Programmierung/blob/main/draw_io/Android-AppStart.png?raw=true)

#### Wettbewerb Erstellen
![Android-WettbewerbErstellen.png](https://github.com/Joh0210/Funktionale-Programmierung/blob/main/draw_io/Android-WettbewerbErstellen.png?raw=true)

#### Spiel Start
![Android-SpielStart.png](https://github.com/Joh0210/Funktionale-Programmierung/blob/main/draw_io/Android-SpielStart.png?raw=true)

## Aufgabe 6
Wählen Sie sich abschließend eines der Projekte aus und versuchen eine prototypenhafte Umsetzung, an der man die funktionale Umsetzung erkennen kann.

### Anmerkung:
- Da beide Projekte mehrere Wochen bis Monate Entwicklungszeit benötigten und sich nicht jeder Teil funktional umsetzen lässt (vgl. Aufgabeblock III: Aufgabe 4), wurde sich dazu entschieden, nur einen Teilausschnitt eines der Projekte zu implementieren, welcher sich als sehr komplex und umfangreich herausstellt.
- Es ist empfehlenswert sich diesen Code in einer Entwicklungsumgebung anzuschauen, welche über Syntax Highlighting verfügt.
- Bei der funktionalen Umsetzung wäre es auch möglich gewesen den Heap über verschachtelte Paare darzustellen, jedoch wurde sich dazu entschieden Listen zu verwenden, um dem originalen Projekt ähnlicher zu sein.
- Die Nutzung von Listen, vor allem durch die Nutzung von der `list-ref` Funktion ist potenziell etwas weniger effizient, allerdings wird die Funktion `getSteps()` im Original nur sehr selten aufgerufen, weshalb Optimierungen an oft genutzten Funktionen empfehlenswerter sind.
- Alle implementierten Versionen sind generisch und nicht auf einen Datentyp zugeschnitten 
- Für den Heap als Array Darstellung gilt, das 0. Element des ist die Root-Node. Die Positionen der Child-Nodes im Array sind `2*n+1` und `2*n+2` für `n = Position des Parrent`. Daraus folgt u.a. dass die Positionen aller linken Child-Nodes immer ungerade sind.

###  Android Algorithmen-Lern App: Heap Sort
Die Kernkomponente unserer Algorithmen Erklärung des Heap Sort verlangte, dass wir eine Liste mit jedem Schritt der Sortierung erhalten.

#### Original
```java
public class Main {
  public static void main(String[] args) {
    // Objektorientiert:
    System.out.println("Objektorientiert:");
    ObjektorientiertHeapSortAlgorithmJava<Integer> heapSort = new ObjektorientiertHeapSortAlgorithmJava<>(new Integer[]{6, 3, 5, 2, 4, 1, 7}, true);
    for (ArrayList<Integer> step : heapSort.getSteps()) {
      System.out.println(step);
    }
  }
}
```

```java
/**
 * detailed step calculation of HeapSort
 * @param <T> type of comparable entry to sort
 * @author Johannes Freund, Tobias Kapitza
 */
public class ObjektorientiertHeapSortAlgorithmJava<T extends Comparable<T>> {
  public final T[] orgArray;
  private final ArrayList<ArrayList<T>> steps;
  private int counter = -1;

  public ObjektorientiertHeapSortAlgorithmJava(T[] orgArray, boolean isMinHeap) {
    this.orgArray = orgArray;
    this.steps = heapSort(orgArray, isMinHeap);
  }

  /**
   * @return array of every step
   */
  public ArrayList<ArrayList<T>> getSteps() {
    return steps;
  }

  /**
   * iteration increases by one
   * @return get the next step in order or null if current step is last
   */
  public ArrayList<T> getNext(){
    return getNext(true);
  }

  /**
   * @param increment Should the next higher step be output when called again?
   * @return Next step or null if the current step is the last.
   */
  public ArrayList<T> getNext(boolean increment){
    if(steps.size() <= counter +1){
      return null;
    }

    if(increment){
      counter++;
      return steps.get(counter);
    }
    return steps.get(counter + 1);
  }

  /**
   * @return true if size of array changes in the next step (insert or delete)
   */
  public boolean isBigStepNext(){
    return counter == -1 || steps.size() <= counter + 1 || (steps.get(counter).size() != getNext(false).size());
  }

  private ArrayList<ArrayList<T>> heapSort(T[] orgArray, boolean isMinHeap){
    ArrayList<ArrayList<T>> ret = new ArrayList<>();
    ArrayList<T> currentVersion = new ArrayList<>();

    for(T element : orgArray){
      currentVersion.add(element);
      ret.add(new ArrayList<>(currentVersion));
      ret.addAll(heapifyInsert(currentVersion, isMinHeap));
    }

    for(int i = 0; i < orgArray.length; i++){
      currentVersion.set(0, currentVersion.get(currentVersion.size()-1));
      currentVersion.remove(currentVersion.size()-1);
      ret.add(new ArrayList<>(currentVersion));
      ret.addAll(heapifyDeleteFirst(currentVersion, isMinHeap));
    }
    return ret;
  }

  private ArrayList<ArrayList<T>> heapifyInsert(ArrayList<T> step, boolean isMinHeap) {
    ArrayList<ArrayList<T>> ret = new ArrayList<>();

    int posElement = step.size() - 1;
    //If the elements are the same, they are unnecessarily exchanged once
    while(getParentPos(posElement) >= 0 && (isMinHeap ? (step.get(getParentPos(posElement)).compareTo(step.get(posElement)) > 0) : (step.get(getParentPos(posElement)).compareTo(step.get(posElement)) < 0))){
      T temp = step.get(getParentPos(posElement));
      step.set(getParentPos(posElement), step.get(posElement));
      step.set(posElement, temp);
      posElement = getParentPos(posElement);
      ret.add(new ArrayList<>(step));
    }
    return ret;
  }

  private ArrayList<ArrayList<T>> heapifyDeleteFirst(ArrayList<T> step, boolean isMinHeap) {
    ArrayList<ArrayList<T>> ret = new ArrayList<>();

    int posElement = 0;
    while(hasSmallerChild(posElement, step, isMinHeap)){
      int smallerChildPos = getChildPos(posElement, isLeftSmaller(posElement, step, isMinHeap));
      T temp = step.get(smallerChildPos);

      step.set(smallerChildPos, step.get(posElement));
      step.set(posElement, temp);
      posElement = smallerChildPos;
      ret.add(new ArrayList<>(step));
    }
    return ret;
  }

  private int getParentPos(int childPos) {
    if (childPos != 0){
      return (int) Math.floor((childPos - 1) / 2.0);
    } else {
      return -1;
    }
  }

  private int getChildPos(int parentPos, boolean left) {
    return 2 * parentPos + (left ? 1 : 2);
  }

  /**
   * @param parentPos Position of the parent node to be examined
   * @param step Current structure of the heap
   * @param isMinHeap Is it a MinHeap? If Max Heap it is checked whether the child is LARGER!
   * @return true if the parent has at least one child smaller than then it.
   */
  private boolean hasSmallerChild(int parentPos, ArrayList<T> step, boolean isMinHeap){
    for(boolean left : new boolean[]{true, false}){
      if(getChildPos(parentPos, left) <= step.size() - 1 && (isMinHeap ? (step.get(parentPos).compareTo(step.get(getChildPos(parentPos, left))) > 0) : (step.get(parentPos).compareTo(step.get(getChildPos(parentPos, left))) < 0))){
        return true;
      }
    }
    return false;
  }

  /**
   * The node must have at least one child!
   * @param parentPos Position of the parent node to be examined
   * @param step Current structure of the heap
   * @param isMinHeap Is it a MinHeap? If Max Heap it is checked whether the child is LARGER!
   * @return true if the left child is smaller than the right one or if the right does not exist
   */
  private boolean isLeftSmaller(int parentPos, ArrayList<T> step, boolean isMinHeap){
    if(getChildPos(parentPos, false) > step.size() - 1){
      return true;
    }
    int comparison = step.get(getChildPos(parentPos, true)).compareTo(step.get(getChildPos(parentPos, false)));
    return isMinHeap ? (comparison < 0) : (comparison > 0);
  }
}
```

#### Funktional - Racket
Die Funktion `getSteps()` muss also funktional in Racket umgesetzt werden.

Die Standartfunktion `get-steps` unterstützt lediglich Strings und Numbers.

Als generische Version gibt es die Funktion `get-steps-generic`, welche auch mit anderen Datentypen genutzt werden kann.
Hierfür benötigt sie als Argument `translate-comparator` eine Funktion, welche Number-Comparators (`>=`, `<=`, `>`, `<`, `=`) in eine geeignete Form umwandelt.
<br>Soll z.B. die Länge der Listen verglichen werden, könnte folgende Funktion übergeben werden: `(lambda (comparator) (lambda (a b) (comparator (length a) (length b))))`

Zur Übung des funktionalen Paradigmas wurde auf die Nutzung von Mutables verzichtet. Es hätte sich jedoch `vector` angeboten.

```
#lang racket

; Generische Variante von "get-steps" um alle Datentypen zu unterstützen
; Es muss eine Funktion "translate-comparator" übergeben werden welche die Number-Comparators (>= <= > < =)
; für die jeweiligen Datentypen übersetzt
; Warnung: Inkonsistente Typen werden hier theoretisch unterstützt, jedoch muss die "translate-comparator"-Funktion damit klar kommen.
(define (get-steps-generic lst is-min-heap translate-comparator)

  ; Ermittelt die Position einer Parent-Node einer Node in einem binären Heap
  ; -1 falls die Node die Root Node ist 
  (define (get-parent-pos child-pos)
    (cond
      (( <= child-pos 0) -1)
      (else (inexact->exact(floor (/ (- child-pos 1) 2.0))))))

  ; Ermittelt ob die Heap-Condition des Binären Heaps gebrochen ist
  ; #f falls die Heap-Condtion nicht gebrochen ist
  ; Position der Child-Node an der die Heap-Condtion nicht erfüllt ist
  ; Sollte es 2 Kandidaten geben, die mit dem Parent getauscht werden können,
  ; wird der kleinere (minheap)/größere (maxheap)genommen.
  (define (heapify-broken? lst)
    (define (broken-at? lst pos)
      (cond
        ((empty? lst) #f)
        ((<= pos 0) #f)
        (((translate-comparator (cond (is-min-heap >=) (else <=))) (list-ref lst pos) (list-ref lst (get-parent-pos pos)))
         (broken-at? lst (- pos 1)))
        ; Fehlerfall:
        ((and (odd? pos) (= pos (- (length lst) 1))) pos)
        ; wähle das kleinere (minheap) der beiden Kinder (größere für dem Maxheap) 
        (((translate-comparator (cond (is-min-heap <=) (else >=)))
                  (list-ref lst pos) (list-ref lst ((cond ((odd? pos) +) (else -)) pos 1))) pos)
        ; die Position aller Linken Child-Nodes sind immer Ungerade
        (else ((cond ((odd? pos) +) (else -)) pos 1))))
    (broken-at? lst (- (length lst) 1)))

  ; Tauscht 2 Elemente einer Liste
  (define (swap pos1 pos2 lst)
    (define (swap-inner1 pre value1 pos2 lst)
      (cond
        ((and (<= pos2 0) (empty? lst)) (append pre (list value1)))
        ((<= pos2 0) (append pre (list value1) (rest lst)))
        (else (swap-inner1 (append pre (list (first lst))) value1 (- pos2 1) (rest lst)))
        ))

    (define (swap-inner2 pre pos1 pos2 lst)
      (cond
        ((<= pos1 0) (swap-inner1 (append pre (list (list-ref lst pos2))) (first lst) (- pos2 1) (rest lst)))
        (else (swap-inner2 (append pre (list (first lst))) (- pos1 1) (- pos2 1) (rest lst)))))

    (cond
      ((< pos1 0)(error "IndexOutOfBoundsException"))
      ((< pos2 0)(error "IndexOutOfBoundsException"))
      ((< pos1 pos2)(swap-inner2 null pos1 pos2 lst))
      ((> pos1 pos2)(swap-inner2 null pos2 pos1 lst))
      (else lst)))

  ; Wenn die Heap-Condition der Liste gebrochen wurde,
  ; fürt diese Funktion genau einen Schritt aus um sie stückweise wieder herzustellen.
  (define (heapify-step lst)
    (swap (get-parent-pos (heapify-broken? lst)) (heapify-broken? lst) lst))

  ; Baut den Heap Schrittweise auf
  (define (insert-step current ret lst)
    (cond
      ((heapify-broken? current) (insert-step (heapify-step current) (append ret (list current)) lst))
      ((empty? lst) (append ret (list current)))
      (else (insert-step (append current (list (first lst))) (cond ((empty? current) ret)(else (append ret (list current)))) (rest lst)))))

  ; Baut den Heap Schrittweise ab.
  ; Nutzt als ausgabgspunkt den letzen Schritt der insert-steps
  (define (delete-step insert-steps)
    (define (delete-step-inner current ret)
      (cond
        ((heapify-broken? current) (delete-step-inner (heapify-step current) (append ret (list current))))
        ((<= (length current) 1) (append ret (list current)))
        (else (delete-step-inner (cons (last current) (reverse (rest (reverse (rest current))))) (append ret (list current))))))

    (delete-step-inner (cons (last (last insert-steps)) (reverse (rest (reverse (rest (last insert-steps)))))) insert-steps))

  ; Interner Aufruf der Funktion
  (cond
    ((empty? lst) null) ; kann nicht sortiert werden
    ((<= (length lst) 1) (list lst)) ; ist bereits sortiert 
    (else (delete-step (insert-step null null lst))))
  )


; Die Standartfunktion unterstütz ausschließlich Numbers und Strings
; Alternative Datentypen können jedoch mit "get-steps-generic" verarbeitet werden.
(define (get-steps lst is-min-heap)

  ; Erstellt eine Funktion die number-comparator zu string-comparator überfürt,
  ; falls es sich bei der Liste um Strings handelt.
  (define (get-comparator-translater)
    (cond
      ((number? (first lst)) (lambda (comparator) comparator))
      ((string? (first lst))
       (lambda (comparator) (cond
         ((equal? comparator >=) string>=?)
         ((equal? comparator <=) string<=?)
         ((equal? comparator >) string>?)
         ((equal? comparator <) string<?)
         (else equal?))))
      (else (error "TypeException: Diese Funktion untersützt nur Listen aus nur Numbers oder nur Strings.\nVerwende statdessen die Funktion \"get-steps-generic\""))))

  (cond
    ((empty? lst) null) ; kann nicht sortiert werden
    ((<= (length lst) 1) (list lst)) ; ist bereits sortiert 
    ((nor (andmap number? lst) (andmap string? lst))
     (error "TypeException: Diese Funktion untersützt nur Listen aus nur Numbers oder nur Strings.\nVerwende statdessen die Funktion \"get-steps-generic\""))
    (else (get-steps-generic lst is-min-heap (get-comparator-translater)))))
```

#### Funktional - Java
Sofern die funktionale Version für das eigentliche Projekt eingesetzt werden sollte, 
muss die Funktion `getSteps()` wieder in Java übertragen werden.

Um das funktionale Schema zu erreichen, werden statische Funktionen verwendet,
welche keine Seiteneffekte auf die Übergabeparameter ausüben.

Des Weiteren wird bestmöglich auf die Objektmanipulation und Schleifen-Kontrollstrukturen verzichtet.
Lediglich die Hilfsfunktionen `add`, `cons`, `rest`, `removeLast` und `swap` greifen auf diese Technik zurück,
um eine signifikante Menge an Rechenleistung bei den Listenoperationen zu sparen.

Es sei auch angemerkt, dass in Java versucht werden sollte Rekursion zu vermeiden
und es daher an vielen Stellen sinnvoll wäre ein `while` Konstrukt zu bauen.
Für Lern- und Demonstrationszwecke wurde darauf jedoch verzichtet.

```java
public class Main {
  public static void main(String[] args) {
    // Funktional:
    System.out.println("\n\nFunktional:");
    for (ArrayList<Integer> step : Objects.requireNonNull(FunktionalHeapSortAlgorithmJava.getSteps(new ArrayList<>(List.of(6, 3, 5, 2, 4, 1, 7)), true))) {
      System.out.println(step);
    }
  }
}
```

```java
/**
 * Hilfsklasse die alle Schritte des Heap Sort für ein Array ermittelt.
 * <br> Die Klasse funktioniert für alle Objekte die {@link Comparable} implementieren.
 * @see Comparable
 * @author Johannes Freund
 */
public class FunktionalHeapSortAlgorithmJava {
    /**
     * Ermittelt alle Schritte die bei dem Heap Sort des Arrays benötigt werden.
     * @param array Array, dass durch Heapsort sortiert werden soll
     * @param isMinHeap Soll ein Min- (true) oder Max (false) Heap aufgebaut werden?
     * @return Array, mit allen Schritten des Heapsort
     * @param <E> Comparable Typ des Heap. Alle Objekte die {@link Comparable} implementieren können diese Funktion nutzen.
     * @see Comparable
     */
    public static <E extends Comparable<E>> ArrayList<ArrayList<E>> getSteps(ArrayList<E> array, boolean isMinHeap) {
        if(array.isEmpty()){
            return null;
        }
        else if(array.size() == 1){
            ArrayList<ArrayList<E>> ret = new ArrayList<>();
            ret.add(array);
            return ret;
        }
        else {
            return deleteStep(insertStep(new ArrayList<>(), new ArrayList<>(), array, isMinHeap), isMinHeap);
        }
    }

    /**
     * Baut den Heap Schrittweise auf
     * @param current Aktueller Schritt
     * @param ret Rückgabe
     * @param array einzufügende Werte
     * @param isMinHeap Handelt es sich um einen Min-Heap?
     * @return Alle Insert Steps
     * @param <E> Typ des Heap
     */
    private static <E extends Comparable<E>> ArrayList<ArrayList<E>> insertStep(ArrayList<E> current, ArrayList<ArrayList<E>> ret, ArrayList<E> array, boolean isMinHeap){
        if(isHeapifyBroken(current, isMinHeap) > -1){
            return insertStep(heapifyStep(current, isMinHeap), add(ret, current), array, isMinHeap);
        }
        else if(array.isEmpty()){
            return add(ret, current);
        }
        else {
            return insertStep(add(current, array.get(0)), (current.isEmpty())? ret : add(ret, current), rest(array), isMinHeap);
        }
    }

    /**
     * Baut den Heap Schrittweise ab
     * @param isMinHeap Handelt es sich um einen Min-Heap?
     * @return Alle Delete Steps
     * @param <E> Typ des Heap
     */
    private static <E extends Comparable<E>> ArrayList<ArrayList<E>> deleteStep(ArrayList<ArrayList<E>> insertSteps, boolean isMinHeap){
        return deleteStepHelper(cons(insertSteps.getLast().getLast(), removeLast(rest(insertSteps.getLast()))), insertSteps, isMinHeap);
    }

    /**
     * Hilfsfunktion von {@link FunktionalHeapSortAlgorithmJava#deleteStep}
     */
    private static <E extends Comparable<E>> ArrayList<ArrayList<E>> deleteStepHelper(ArrayList<E> current, ArrayList<ArrayList<E>> ret, boolean isMinHeap){
        if(isHeapifyBroken(current, isMinHeap) > -1){
            return deleteStepHelper(heapifyStep(current, isMinHeap), add(ret, current), isMinHeap);
        }
        else if(current.size() <= 1){
            return add(ret, current);
        }
        else {
            return deleteStepHelper(cons(current.getLast(), removeLast(rest(current))), add(ret, current), isMinHeap);
        }
    }

    /**
     * Ermittelt die Position einer Parent-Node einer Node in einem binären Heap
     * @param childPos Position der Child Node
     * @return Position des Parent, oder -1 Falls die Node die Wurzel ist
     */
    private static int getParentPos(int childPos){
        if(childPos <= 0){
            return -1;
        }
        else {
            return (int) Math.floor((childPos - 1) / 2.0f);
        }
    }

    /**
     * Ermittelt ob die Heap-Condition des Binären Heaps gebrochen ist
     * @param array Zu überprüfender Heap
     * @param isMinHeap Handelt es sich um einen Min-Heap?
     * @return -1 falls nicht gebrochen oder Position der Child-Node an der die Heap-Condtion nicht erfüllt ist
     * @param <E> Typ des Heap
     */
    private static <E extends Comparable<E>> int isHeapifyBroken(ArrayList<E> array, boolean isMinHeap){
        return (isHeapifyBrokenAt(array, array.size() - 1, isMinHeap));
    }

    /**
     * Hilfsfunktion von {@link FunktionalHeapSortAlgorithmJava#isHeapifyBroken}
     */
    private static <E extends Comparable<E>> int isHeapifyBrokenAt(ArrayList<E> array, int pos, boolean isMinHeap){
        if(array.isEmpty()){
            return -1;  // -> False
        }
        else if(pos <= 0){
            return -1; // -> False
        }
        else if(
                (isMinHeap && 0 <= array.get(pos).compareTo(array.get(getParentPos(pos)))) |    //Parent >= Child
                (!isMinHeap && 0 >= array.get(pos).compareTo(array.get(getParentPos(pos))))     //Parent <= Child
        ){
            return isHeapifyBrokenAt(array, pos-1, isMinHeap);
        }

        // Fehlerfall:
        else if((pos % 2 != 0) && (pos == array.size()-1)){
            return pos;
        }
        // wähle das kleinere (minheap) der beiden Kinder (größere für dem Maxheap)
        else if(
                (isMinHeap && 0 >= array.get(pos).compareTo(array.get(pos + ((pos % 2 != 0) ? +1 : -1)))) |
                (!isMinHeap && 0 <= array.get(pos).compareTo(array.get(pos + ((pos % 2 != 0) ? +1 : -1))))
        ){
            return pos;
        }
        else {
            return pos + ((pos % 2 != 0) ? +1 : -1);
        }
    }

    /**
     * Wenn die Heap-Condition der Liste gebrochen wurde,
     * fürt diese Funktion genau einen Schritt aus um sie stückweise wieder herzustellen.
     * @param array Heap
     * @param isMinHeap Handelt es sich um einen Min-Heap?
     * @return Nächster Schritt für den Heap um die Heap Kondition zu erfüllen.
     * @param <E> Typ des Heap
     */
    private static <E extends Comparable<E>> ArrayList<E> heapifyStep(ArrayList<E> array, boolean isMinHeap){
        return swap(array, getParentPos(isHeapifyBroken(array, isMinHeap)), isHeapifyBroken(array, isMinHeap));
    }

    /**
     * Tauscht 2 Elemente im Heap. (Nicht funktional, aber ohne Seiteneffekte)
     * @param array Array in dem Elemente getauscht werden sollen
     * @param pos1 1. Tauschelement
     * @param pos2 2. Tauschelement
     * @return Array in dem die Elemente getauscht wurden
     * @param <E> Typ des Heap
     */
    private static <E extends Comparable<E>> ArrayList<E> swap(ArrayList<E> array, int pos1, int pos2){
        ArrayList<E> copy = new ArrayList<>(array);

        E temp = copy.get(pos1);
        copy.set(pos1, copy.get(pos2));
        copy.set(pos2, temp);

        return copy;
    }

    /**
     * Fügt ein Element an das Ende des Arrays. (Nicht funktional, aber ohne Seiteneffekte)
     * @param array Haupt-Array
     * @param addtion Element das hinzugefügt wird
     * @return Zusammengefügtes Array
     * @param <T> Typ des Haupt-Arrays
     */
    private static <T> ArrayList<T> add(ArrayList<T> array, T addtion){
        ArrayList<T> copy = new ArrayList<>(array);
        copy.add(addtion);
        return copy;
    }

    /**
     * Fügt ein Element an den Anfang des Arrays. (Nicht funktional, aber ohne Seiteneffekte)
     * @param array Haupt-Array
     * @param addtion Element das hinzugefügt wird
     * @return Zusammengefügtes Array
     * @param <T> Typ des Haupt-Arrays
     */
    private static <T> ArrayList<T> cons(T addtion, ArrayList<T> array){
        ArrayList<T> copy = new ArrayList<>(array);
        copy.add(0, addtion);
        return copy;
    }

    /**
     * Entfernt das letzte Element des Arrays. (Nicht funktional, aber ohne Seiteneffekte)
     * @param array Zu nutzendes Array
     * @return Array ohne das letzte Element
     * @param <T> Typ des Arrays
     */
    private static <T> ArrayList<T> removeLast(ArrayList<T> array){
        ArrayList<T> copy = new ArrayList<>(array);
        copy.removeLast();
        return copy;
    }

    /**
     * Entfernt das 1. Element des Arrays. (Nicht funktional, aber ohne Seiteneffekte)
     * @param array Zu nutzendes Array
     * @return Array ohne das 1. Element
     * @param <T> Typ des Arrays
     */
    private static <T> ArrayList<T> rest(ArrayList<T> array){
        ArrayList<T> copy = new ArrayList<>(array);
        copy.removeFirst();
        return copy;
    }
}
```
