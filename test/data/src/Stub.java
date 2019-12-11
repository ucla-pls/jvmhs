public class Stub {

    static {
        System.out.println("hello");
    }

    public static void main (String [] argv) { 
        new Stub().fn1(0.0, 2.0);
    }

    public Stub () {
        System.out.println("world");
    }

    public double fn1(double a, Double b) {
        return a + b;
    }
    
    public long fn2(long a, Long b) {
        return a + b;
    }
    
    public int fn3(int a, Integer b) {
        return a + b;
    }
    
    public String fn4(String a, String b, String c) {
        return a + b + c;
    }
    
    public void fn5(String a, String b, String c) {
        System.out.println(this.fn4(a, b, c));
        return;
    }
    
    public static void fn6() {
        return;
    }
    
    public static char fn7() {
        return 0;
    }

}
