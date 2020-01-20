import java.lang.annotation.*;

public class Wierd {
  public static @Target({ElementType.TYPE_USE}) @interface Ann {}; 

  public static C. D . @Ann E <?> x;
  public static F. G . @Ann H <?> y;

  public <T> C.D.E m () { return null;}
  public <T> F.G.H n () { return null;}
  
  public class C {
    public class D {
      public class E<T> {
      }
    }
  }
  
  public static class F {
    public static class G {
      public class H <T> {
      }
    }
  }

}
