import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Main {
    public static void main(String[] args) {
        // Objektorientiert:
        System.out.println("Objektorientiert:");
        ObjektorientiertHeapSortAlgorithmJava<Integer> heapSort = new ObjektorientiertHeapSortAlgorithmJava<>(new Integer[]{6, 3, 5, 2, 4, 1, 7}, true);
        for (ArrayList<Integer> step : heapSort.getSteps()) {
            System.out.println(step);
        }

        // Funktional:
        System.out.println("\n\nFunktional:");
        for (ArrayList<Integer> step : Objects.requireNonNull(FunktionalHeapSortAlgorithmJava.getSteps(new ArrayList<>(List.of(6, 3, 5, 2, 4, 1, 7)), true))) {
            System.out.println(step);
        }

        System.out.println("\n\nTest:");
        test(new Integer[]{6, 3, 5, 2, 4, 1, 7},true);
        test(new Integer[]{6, 3, 5, 2, 4, 1, 7},false);
        test(new Integer[]{2},true);
        test(new Integer[]{2},false);
        test(new String[]{"A", "L", "B", "D", "D", "E"},true);
        test(new String[]{"A", "L", "B", "D", "D", "E"},false);
        test(new String[]{"lölaöls", "asdfsa", "klmopk", "Dasd", "DEEE", "EFAD"},true);
        test(new String[]{"lölaöls", "asdfsa", "klmopk", "Dasd", "DEEE", "EFAD"},false);
    }

    /**
     * Testet, ob die objektorientierte und funktionale Version das identische Ergebnis für ein Test-Array ergeben
     * @param testArray Array für das getestet werden soll
     * @param isMinHeap Soll ein Min- (true) oder Max (false) Heap aufgebaut werden?
     * @param <E> Comparable Typ des Heap. Alle Objekte die {@link Comparable} implementieren können diese Funktion nutzen.
     * @see FunktionalHeapSortAlgorithmJava
     * @see ObjektorientiertHeapSortAlgorithmJava
     */
    public static <E extends Comparable<E>> void test(E[] testArray, boolean isMinHeap) {
        ObjektorientiertHeapSortAlgorithmJava<E> heapSort = new ObjektorientiertHeapSortAlgorithmJava<>(testArray, isMinHeap);
        ArrayList<ArrayList<E>> heapSteps = heapSort.getSteps();
        heapSort.getSteps().removeLast();   // Das Original hängt noch die Leere Liste an.
        System.out.println(Arrays.toString(testArray) + (isMinHeap ? ", min" : ", max") + ":\t" + isEqual(
                heapSteps,
                Objects.requireNonNull(FunktionalHeapSortAlgorithmJava.getSteps(new ArrayList<>(Arrays.asList(testArray)), isMinHeap))));
    }


    /**
     * Prüft, dass die beiden übergebenen Arrays einen identischen Inhalt haben
     * @return true, falls identisch.
     */
    private static <E extends Comparable<E>> boolean isEqual(ArrayList<ArrayList<E>> array1, ArrayList<ArrayList<E>> array2){
        if(array1.size() == array2.size()){
            for (int i = 0; i < array1.size(); i++) {
                if(array1.get(i).size() == array2.get(i).size()){
                    for(int j = 0; j < array1.get(i).size(); j++){
                        if(array1.get(i).get(j).compareTo(array2.get(i).get(j)) != 0){
                            return false;
                        }
                    }
                } else {
                    return false;
                }
            }
        } else {
            return false;
        }
        return true;
    }
}