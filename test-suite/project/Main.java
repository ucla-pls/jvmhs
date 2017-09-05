
public class Main implements Runnable {

    private int someAttr = 2;
    private double someOtherAttr = 4.0;

    public static void main (String [] argv) { 
        System.out.println("Hello, World!");
    }

    public int addOne() {
        someAttr = someAttr + 1;
        return someAttr;
    }

    public int aComplexExample(int a ) {
      // This should produce a tableswitch
      switch (a) {
      case 0: return 2;
      case 1: return 1;
      case 2: return 0;
      default:
        System.out.println("Reached Table Default");
      }
      // This should produce a LookupSwitch
      switch (a) {
      case 0: return 2;
      case 23232: return 0;
      case 23232233: return 1;
      default:
        System.out.println("Reached Lookup Default");
      }
      return a;
    }

    public void run () { }
}
