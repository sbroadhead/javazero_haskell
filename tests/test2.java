class Matrix
{
    int[] a;
    int w;
    int h;

    void Init(int width, int height) {
        a = new int[width*height];
        w = width;
        h = height;
    }

    Matrix mult(Matrix other) {
        int i; int j; int k;
        Matrix product;

        product = new Matrix();
        product.Init(other.w, h);

        i = 0;
        while (i < h) {
            j = 0;
            while (j < other.w) {
                k = 0;
                while (k < w) {
                    product.a[i*product.w+j] = product.a[i*product.w+j] + a[i*w+k] * other.a[k*other.w+j];
                    k = k + 1;
                }
                j = j + 1;
            }
            i = i + 1;
        }

        return product;
    }

    void dump() {
        int i; int j;
        i = 0;
        while (i < h) {
            j = 0;
            while (j < w) {
                print(a[i*w+j]);
                j = j + 1;
            }
            i = i + 1;
            println();
        }
    }
}

class Program
{
    static void main(String[] args) {
        Matrix a; Matrix b; Matrix c;
        a = new Matrix();
        b = new Matrix();

        a.Init(3, 3); b.Init(3, 3);
        
        a.a[0] = 1; a.a[1] = 2; a.a[2] = 3;
        a.a[3] = 4; a.a[4] = 5; a.a[5] = 6;
        a.a[6] = 7; a.a[7] = 8; a.a[8] = 9;

        b.a[0] = 2; b.a[1] = 0; b.a[2] = 3;
        b.a[3] = 0; b.a[4] = 2; b.a[5] = 0;
        b.a[6] = 0; b.a[7] = 0; b.a[8] = 2;

        c = a.mult(b);

        c.dump();
    }
}
