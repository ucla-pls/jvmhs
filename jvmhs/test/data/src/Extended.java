public class Extended extends Simple implements Interface {
  public int extField = 0;

  public void method1 () {
    this.method2();
  }

  @Override
  public int method3 (int x) {
    return x;
  }

  @Override
  public void method4 () {
  }
}
