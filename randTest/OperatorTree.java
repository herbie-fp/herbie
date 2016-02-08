import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.Stack;

import javax.annotation.PostConstruct;

public class OperatorTree {
	private int height;  //number of operators or the height of the tree
	public Node root;
	public final static Set<String> OPERATORS = new HashSet<String>();
	public final static Set<String> VARIABLES = new HashSet<String>();
	private Random r;
	
	static {
		OPERATORS.add("+");
		OPERATORS.add("-");
		OPERATORS.add("/");
		OPERATORS.add("*");
		VARIABLES.add("x");
		VARIABLES.add("y");
	}
	
	// @param size number of nodes in the tree
	public OperatorTree(int size) {
		this.height = size;
		root = new Node();
		r =  new Random();
	}
	
	// constructs an empty tree where each node either has zero or two children
	public void createEmpty() {
		if (height < 0) {
			throw new IllegalArgumentException();
		}
		Node curr = root;
		for (int i = 0; i < height; i++) {
			curr.left = new Node();
			curr.right = new Node();
			int whichWay = r.nextInt(2);
			if (whichWay == 0) {		
				curr = curr.left;
			} else {
				curr = curr.right;
			}
		}
	}
	
	public String parse() {
		StringBuilder sb = new StringBuilder(0);
		Stack<String> s = new Stack<String>();
		parse(sb, root, s);	
		return sb.toString();
	}
	
	private void parse(StringBuilder sb, Node node, Stack<String> s) {
		if (node != null) {
			if (OPERATORS.contains(node.data)) {
				sb.append("(" + node.data + " ");
				s.push(node.data);
			} else {
				sb.append(node.data + " ");
			}
			parse(sb, node.left, s);
			parse(sb, node.right, s);
			if (OPERATORS.contains(node.data)) {
				sb.append(") ");
				s.pop();
			}
		}
	}
	
//	public Node createEmpty(Node node, int size) {
//		if (size < 0) {
//			return null;
//		} else {
//			int buildOrNot = r.nextInt(2);
//			if (buildOrNot == 0) {
//				node = new Node();
//				node.left = createEmpty(node.left, size - 1);
//				if (node.left != null) { // if build left, has to build right
//					node.right = createEmpty(node.right, size - 2);
//					if (node.right == null) {
//						node.right = new Node();
//					}
//				}
//				return node;
//			} else {
//				return null;  //decide not to build
//			}
//		}
//			
//	}
	
	public void populate(Node n) {
		if (n.left == null && n.right == null) {
			if (r.nextInt(2) == 0) {
				n.data = r.nextDouble()* 1000+"";
				n.data = n.data.substring(0, 7);
			} else {
				String[] varArr = new String[VARIABLES.size()];
				VARIABLES.toArray(varArr);
				n.data = varArr[r.nextInt(varArr.length)];
			}
		} else {
			int opIndex = r.nextInt(4);
			String[] optArr = new String[OPERATORS.size()];
			OPERATORS.toArray(optArr);
			n.data = optArr[opIndex];
			populate(n.left);
			populate(n.right);
		}
	}
	
	public int size() {
		return height;
	}
	
	public void preOrder() {
		preOrder(root);
	}
	
	private void preOrder(Node n) {
		if (n == null) {
			return;
		} else {
			System.out.println(n.data);
			preOrder(n.left);
			preOrder(n.right);
		}
	}
	
	public class Node {
		private String data;
		private Node left;
		private Node right;
		
		public Node() {
			this(null, null, null);
		}
		
		public Node(String data, Node left, Node right) {
			this.data = data;
			this.left = left;
			this.right = right;
		}
	}
	
	
	// All code below are just printing functions.
	
	
	public static void printNode(Node root) {
        int maxLevel = maxLevel(root);

        printNodeInternal(Collections.singletonList(root), 1, maxLevel);
    }

    private static void printNodeInternal(List<Node> nodes, int level, int maxLevel) {
        if (nodes.isEmpty() || isAllElementsNull(nodes))
            return;

        int floor = maxLevel - level;
        int endgeLines = (int) Math.pow(2, (Math.max(floor - 1, 0)));
        int firstSpaces = (int) Math.pow(2, (floor)) - 1;
        int betweenSpaces = (int) Math.pow(2, (floor + 1)) - 1;

        printWhitespaces(firstSpaces);

        List<Node> newNodes = new ArrayList<Node>();
        for (Node node : nodes) {
            if (node != null) {
                System.out.print(node.data);
                newNodes.add(node.left);
                newNodes.add(node.right);
            } else {
                newNodes.add(null);
                newNodes.add(null);
                System.out.print(" ");
            }

            printWhitespaces(betweenSpaces);
        }
        System.out.println("");

        for (int i = 1; i <= endgeLines; i++) {
            for (int j = 0; j < nodes.size(); j++) {
                printWhitespaces(firstSpaces - i);
                if (nodes.get(j) == null) {
                    printWhitespaces(endgeLines + endgeLines + i + 1);
                    continue;
                }

                if (nodes.get(j).left != null)
                    System.out.print("/");
                else
                    printWhitespaces(1);

                printWhitespaces(i + i - 1);

                if (nodes.get(j).right != null)
                    System.out.print("\\");
                else
                    printWhitespaces(1);

                printWhitespaces(endgeLines + endgeLines - i);
            }

            System.out.println("");
        }

        printNodeInternal(newNodes, level + 1, maxLevel);
    }

    private static void printWhitespaces(int count) {
        for (int i = 0; i < count; i++)
            System.out.print(" ");
    }

    private static int maxLevel(Node node) {
        if (node == null)
            return 0;

        return Math.max(maxLevel(node.left), maxLevel(node.right)) + 1;
    }

    private static boolean isAllElementsNull(List list) {
        for (Object object : list) {
            if (object != null)
                return false;
        }

        return true;
    }
}
