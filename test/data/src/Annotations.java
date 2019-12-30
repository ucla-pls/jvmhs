import java.lang.annotation.*;

public class Annotations {

  @interface TestNotation {
    String a();
    int b();
  };
  
  public static @Target({ElementType.FIELD, ElementType.TYPE, ElementType.TYPE_USE}) @interface TestType {
    int value();
  };

  @TestNotation(a = "hello", b = 2)
  static class Annotated <@TestType(10) T> {
  }

  Annotated<@TestType(0) int @TestType(1) []> x;

  @TestType(3) int y;

  @TestType(4) int m (@TestType(6) boolean b) throws @TestType(7) Exception{
    return 0;
  }
}
