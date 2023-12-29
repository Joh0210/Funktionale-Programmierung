import java.util.ArrayList;

/**
 * detailed step calculation of HeapSort
 * @param <T> type of comparable entry to sort
 * @author Johannes Freund, Tobias Kapitza
 */
public class HeapSortAlgorithmJava<T extends Comparable<T>> {
    public final T[] orgArray;
    private final ArrayList<ArrayList<T>> steps;
    private int counter = -1;

    public HeapSortAlgorithmJava(T[] orgArray, boolean isMinHeap) {
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
        while(getParentPos(posElement) >= 0 && (step.get(getParentPos(posElement)).compareTo(step.get(posElement)) > 0 == isMinHeap)){
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
            if(getChildPos(parentPos, left) <= step.size() - 1 && (step.get(parentPos).compareTo(step.get(getChildPos(parentPos, left))) > 0) == isMinHeap){
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
        return (step.get(getChildPos(parentPos, true)).compareTo(step.get(getChildPos(parentPos, false))) < 0) == isMinHeap;
    }
}
