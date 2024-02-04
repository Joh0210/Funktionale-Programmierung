import java.util.ArrayList;

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
