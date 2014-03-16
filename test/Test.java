public class Test {
	byte   bval   = 0x5;
	short  sval   = 0x555;
	int    ival   = 0x555555;
	long   lval   = 0x555555555555l;
	float  fval   = 0.123e10f;
	double dval   = 0.1234567890123456e250d;
	char   cval   = 'f';
	String strval = "oink";
	Object objval = new Object();

	byte[]   baval   = new byte[] { 0x1, 0x2, 0x3 };
	short[]  saval   = new short[] { 0x555, 0x666, 0x777 };
	int[]    iaval   = new int[] { 0x555555, 0x666666 };
	long[]   laval   = new long[] { 0x555555555555l };
	float[]  faval   = new float[] { 0.123e10f, 6.0f };
	double[] daval   = new double[] { 0.1234567890123456e250d };
	char[]   caval   = new char[] { 'f', 'g' };
	String[] straval = new String[] { "oink" };
	Object[] objaval = new Object[] { new Object() };	

	static void   sVoid()   {}
	static byte   sByte()   { return 1; }
	static short  sShort()  { return 1; }
	static int    sInt()    { return 1; }
	static long   sLong()   { return 1; }
	static float  sFloat()  { return 1.0f; }
	static double sDouble() { return 1.0; }
	static char   sChar()   { return 'c'; }
	static String sString() { return "string"; }
	static Object sObject() { return new Object(); }

	static void   sVoid(int a, double b, char[] c, long d, byte e)   {}
	static byte   sByte(int a, double b, char[] c, long d, byte e)   { return 1; }
	static short  sShort(int a, double b, char[] c, long d, byte e)  { return 1; }
	static int    sInt(int a, double b, char[] c, long d, byte e)    { return 1; }
	static long   sLong(int a, double b, char[] c, long d, byte e)   { return 1; }
	static float  sFloat(int a, double b, char[] c, long d, byte e)  { return 1.0f; }
	static double sDouble(int a, double b, char[] c, long d, byte e) { return 1.0; }
	static char   sChar(int a, double b, char[] c, long d, byte e)   { return 'c'; }
	static String sString(int a, double b, char[] c, long d, byte e) { return "string"; }
	static Object sObject(int a, double b, char[] c, long d, byte e) { return new Object(); }

	static native int snInt(int x, char y);
	static synchronized char ssChar(int x, char y) { return 'e'; }

	void   iVoid()   {}
	byte   iByte()   { return 1; }
	short  iShort()  { return 1; }
	int    iInt()    { return 1; }
	long   iLong()   { return 1; }
	float  iFloat()  { return 1.0f; }
	double iDouble() { return 1.0; }
	char   iChar()   { return 'c'; }
	String iString() { return "string"; }
	Object iObject() { return new Object(); }

	protected void   iVoid(int a, double b, char[] c, long d, byte e)   {}
	protected byte   iByte(int a, double b, char[] c, long d, byte e)   { return 1; }
	protected short  iShort(int a, double b, char[] c, long d, byte e)  { return 1; }
	protected int    iInt(int a, double b, char[] c, long d, byte e)    { return 1; }
	protected long   iLong(int a, double b, char[] c, long d, byte e)   { return 1; }
	protected float  iFloat(int a, double b, char[] c, long d, byte e)  { return 1.0f; }
	protected double iDouble(int a, double b, char[] c, long d, byte e) { return 1.0; }
	protected char   iChar(int a, double b, char[] c, long d, byte e)   { return 'c'; }
	protected String iString(int a, double b, char[] c, long d, byte e) { return "string"; }
	protected Object iObject(int a, double b, char[] c, long d, byte e) { return new Object(); }

	@Deprecated int dx = 5;



	private static int sPrivateX = 6;

	static class InnerStaticSuper {
		void bar() { System.out.println("bar"); }
	}

	static class InnerStaticSub extends InnerStaticSuper {
		@Override void bar() { System.out.println("foo"); }
		void baz() { sPrivateX++; }
	}


	private int privateX = 15;

	class InnerSuper {
		protected void foo() { System.out.println("foo"); }
	}

	class InnerSub extends InnerSuper {
		InnerSub() { privateX++; }

		@Override protected void foo() { System.out.println("bar"); }
	}


	void exceptionCatcher(int x, int y, double a, double b, long v) throws ArrayIndexOutOfBoundsException {
		try {
			System.out.println("hi!");

			while (true) {
				try {
					System.out.println("oink");
				} catch (Throwable t) {
					System.out.println("mooooooo");
					continue;
				} finally {
					System.out.println("foo");
					break;
				}
			}

			if (x == 5) return;
		} catch (Exception e) {
			e.printStackTrace();
			throw new ArrayIndexOutOfBoundsException(5);
		} finally {
			System.out.println("bye!");
		}
		System.out.println("bye!");
	}
}