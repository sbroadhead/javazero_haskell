class PQ {

  public int key;
  public PQ next;

  public void insert (int x) {
    if (next == null) {
      next = new PQ ();
      key = x;
    } else
      if (x > key) next.insert (x);
      else {
        next.insert (key); key = x;
      }
  }

  public int min () {
    return key;
  }

  public static void main (String[] args) {
    PQ l;
    int a;
    a = 5 + 6;
    l = new PQ ();
    l.insert (9);
    l.insert (3);
    l.insert (5);
    while (l.next != null) {
      print (l.key);
      l = l.next;
    }
  }
}
