public class SimpleI implements Itfc, Itfc2{
	static int x = 0;
	// Remove this vars
	static int y = 0;
//    Itfc sp = null;

	public static void main(String [] argv) {
	    new SimpleI().useX();
	}

	public int useX(){
        System.out.println(x);
        Itfc a = new SimpleI();
        ItfcParent p = new SimpleI();
        p.calculate0(false);
        return a.calculate(true);
    }

    public int calculate0(boolean a){
        return 2;
    }

    public int calculate(boolean a){
	    return 2;
    }

    public int calculate2(boolean a){
        return 2;
    }
}
