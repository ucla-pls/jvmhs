import java.lang.annotation.*;
import java.util.List;
import java.util.Set;
import java.io.IOException;

public class Annotations <O> {

  @interface TestNotation {
    String a();
    int b();
  };
  
  public static @Target({ElementType.FIELD, ElementType.TYPE, ElementType.TYPE_USE}) @interface TestTypes {
    TestType [] value ();
  }
  
  public static @Repeatable(TestTypes.class) @Target({ElementType.FIELD, ElementType.PARAMETER, ElementType.TYPE, ElementType.TYPE_USE}) @interface TestType {
    int value();
  };
  
  public static @Target({ElementType.FIELD, ElementType.TYPE, ElementType.PARAMETER, ElementType.TYPE_USE}) @interface B {
    int value();
  };

  @TestNotation(a = "hello", b = 2)
  abstract static class Annotated <@TestType(10) T> extends @B(22) Object implements List<@B(23) T>{

  }

  Annotated<@TestType(0) int @TestType(1) []> x;
  
  Annotated<? extends Object> z;

  @TestType(3) int y;

  @TestType(4) int m (@B(5) @TestType(3) @TestType(6) boolean b, @B(10) List<@B(9) Object> c) throws @TestType(7) Exception, Throwable, IOException {
    return 0;
  }
  
  <@B(11) T extends Throwable & List<Object[]>> int n () throws T, Exception{
    return 0;
  }

  <T extends Comparable & List<T>> T k () {
    return null;
  }
  
  <T extends List<T> & Comparable > T j () {
    return null;
  }
  
  void inside () {
    @TestType(12) Integer i = null;
    return null;
  }
  
}
