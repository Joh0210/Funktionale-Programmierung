import java.lang.Integer;

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
        private int value;

        public MyInt(int value){
            this.value = value;
        }

        public int getValue(){
            return this.value;
        }

        public void add (int i){
            this.value += i;
        }
    }
}