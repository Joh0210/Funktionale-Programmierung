import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {
        HeapSortAlgorithmJava<Integer> heapSort = new HeapSortAlgorithmJava<>(new Integer[]{1, 2, 3, 4, 5}, true);
        for (ArrayList<Integer> step : heapSort.getSteps()) {
            System.out.println(step);
        }
    }
}