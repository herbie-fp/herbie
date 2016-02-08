
public class RandomTest {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		if (args.length != 3) {
			System.out.println("Use 'java RandomTest treeSize numOfVars numOfTest'");
			System.exit(1);
		}
		int treeSize = Integer.parseInt(args[0]);
		int numOfVars = Integer.parseInt(args[1]);
		int numOfTest = Integer.parseInt(args[2]);
		
		OperatorTree opt = new OperatorTree(treeSize, numOfVars);
		printTest(opt, numOfTest);
//		opt.preOrder();
//		OperatorTree.printNode(opt.root);
		
	}
	
	public static void printTest(OperatorTree opt) {
		printTest(opt, 1);
	}
	
	public static void printTest(OperatorTree opt, int n) {
		for (int i  = 0; i < n; i++) {
			//initialize tree
			opt.createEmpty();
			opt.populate(opt.root);
			// print test
			System.out.print("(herbie-test (");
			String[] varArr = new String[OperatorTree.VARIABLES.size()];
			OperatorTree.VARIABLES.toArray(varArr);
			System.out.print(varArr[0]);
			for (int j = 1; j < varArr.length; j++) {
				System.out.print(" " + varArr[j]);
			}
			System.out.println(")");
			System.out.println("  \"Random test " + (i+1) + "\"");
			System.out.print("    " + opt.parse());
			System.out.println(")");
			System.out.println();
		}
	}

}

