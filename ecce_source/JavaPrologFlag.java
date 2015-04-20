package ecce.ecce_source;

public class JavaPrologFlag
{
    public static boolean bFlag = false;
    public static void setFlag(boolean bF) {bFlag = bF;}
    public static boolean getFlag() {return bFlag;}

    public static void main(String argv[]) {
	  System.out.println("current flag is "+bFlag);
    }
}
