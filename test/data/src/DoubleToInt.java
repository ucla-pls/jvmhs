public class DoubleToInt {

  public static void main(String [] array) {
    DoubleToInt[] a = new DoubleToInt[1];
    Object[] b = a;
    b[0] = "I'm not a car";
    a[0].hello();
  }

  public void hello() {
    System.out.println("Hello, world!");
  }
}
