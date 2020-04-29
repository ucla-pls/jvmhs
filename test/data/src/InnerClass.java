public class InnerClass {

  public static void main (String [] args) { 
    new InnerClass().test();
  }
 
  public void test () {
    A a = new A();
  }

  public class A {
    private A () {};
  }
  
  public class B {
  }
}
