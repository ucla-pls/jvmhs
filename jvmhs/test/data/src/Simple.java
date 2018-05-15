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
}
