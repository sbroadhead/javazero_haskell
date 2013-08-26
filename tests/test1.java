class Class1 {
    public static int[] a;
    int[] b;
    
    public int Test(int a, int b) {
        return 2*a + 5*b;
    }
}

class Class2 {
    public static void main(String[] args) {
        Class1 z;
        z = new Class1();
        z.b = new int[128];
        z.b[7] = 100;
        z.b[6] = 5;
        z.b[8] = 6;
        
        print(z.b[7]);
        print(z.Test(z.b[8], z.b[6]));
    }
}