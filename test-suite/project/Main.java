
public class Main  {
    
    private int someAttr = 2; 
    private double someOtherAttr = 4.0; 

    public static void main (String [] argv) { 
        System.out.println("Hello, World!");
    }

    public int addOne() {
        someAttr = someAttr + 1;
        return someAttr;
    }
}
