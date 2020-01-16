import java.lang.annotation.*;
import java.util.List;

public class DeepSignature<C extends List> {
  
  public static @Target({ElementType.PARAMETER}) @interface Ann {};

  public class Node<T extends Throwable> {
      final T a;
      public Node(@Ann T a) { 
        this.a = a;
      }
  }
}
