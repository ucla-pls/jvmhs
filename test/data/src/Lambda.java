/** Testing lamdba expressions.
  */
import java.util.concurrent.Callable;
public class Lambda <T> {

  public static void main (String [] args) {
    Callable<Object> fn = () -> new Object();
  }
  
  public static <S> void thes (S s) {
    Callable<S> fn = () -> s;
    try { 
      S x = fn.call();
    } catch (Exception e) { 
    }
  }
  
  public void inmethod () {
    Callable<String> fn = () -> "Hello";
  }
}
