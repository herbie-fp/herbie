
public class RandomTest {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		OperatorTree opt = new OperatorTree(6);
		opt.createEmpty();
		opt.populate(opt.root);
		printTest(opt);
//		opt.preOrder();
//		OperatorTree.printNode(opt.root);
		
	}
	
	public static void printTest(OperatorTree opt) {
		System.out.print("(herbie-test (");
		String[] varArr = new String[OperatorTree.VARIABLES.size()];
		OperatorTree.VARIABLES.toArray(varArr);
		System.out.print(varArr[0]);
		for (int i = 1; i < varArr.length; i++) {
			System.out.print(" " + varArr[i]);
		}
		System.out.println(")");
		System.out.println("  \"Random test\"");
		System.out.println("    " + opt.parse());
		System.out.println(" )");
	}

}
