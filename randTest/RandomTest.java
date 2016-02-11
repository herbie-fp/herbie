public class RandomTest {
	public static void main(String[] args) {
		if (args.length != 3) {
			System.out.println("Usage: java RandomTest size nVars nTests");
			System.exit(1);
		}
		int size = Integer.parseInt(args[0]);
		int nVars = Integer.parseInt(args[1]);
		int nTests = Integer.parseInt(args[2]);

    for(int i=0; i<nTests; i++) {
      System.out.println(new OperatorTree(size, nVars));
			System.out.println();
    }
  }
}
