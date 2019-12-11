public class Simple {
  private Simple field = null;
  public static void main (String [] args) {
      System.out.println("Hello, world");
      for (int i = 0; i < 100; i++) { 
          int y = i;
          y += i;
          System.out.println(y);
      }
  }

  public void method2() {
  }

  public int method3 (int x) {
    return 0;
  }
}
