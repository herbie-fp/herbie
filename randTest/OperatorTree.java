import java.util.Random;
import java.util.Arrays;
import java.nio.ByteBuffer;

public class OperatorTree {
  public static final String[] UNOP =
    { "-"
    , "sqrt"
    , "sqr"
    , "exp"
    , "log"
    , "sin"
    , "cos"
    , "tan"
    , "cotan"
    , "asin"
    , "acos"
    , "atan"
    , "sinh"
    , "cosh"
    , "tanh"
    , "abs"
    , "expm1"
    , "log1p"
    };

  public static final String[] BINOP =
    { "+"
    , "-"
    , "*"
    , "/"
    , "expt"
    , "atan2"
    , "mod"
    , "hypot"
    };

  public static final String[] VARS =
    "abcdefghijklmnopqrstuvwxyz".split("");

  private static Random rnd = new Random();

  private static String choose(String[] a) {
    return a[rnd.nextInt(a.length)];
  }

  private static String getDoubleStr() {
    byte[] bytes = new byte[8];
    rnd.nextBytes(bytes);
    double val = ByteBuffer.wrap(bytes).getDouble();
    return Double.toString(val);
  }

  private static int count = 0;

  private final String name;
  private final String[] vars;
  private final Node expr;

  public OperatorTree(int size, int nVars) {
    if(size < 1) throw new Error("size < 1");
    if(nVars > VARS.length) throw new Error("nVars > VARS.length");

    count++;
    this.name = String.format("\"Random Jason Test %03d\"", count);
    this.vars = Arrays.copyOf(VARS, nVars);
    this.expr = genExpr(size);
  }

  private Node genExpr(int fuel) {
    fuel--;

    Node n;
    if(fuel < 1) {
      if(vars.length == 0 || rnd.nextInt(5) == 0) {
          n = new Node(getDoubleStr(), null, null);
      } else {
        n = new Node(choose(vars), null, null);
      }
    } else if(fuel < 2 || rnd.nextInt(4) == 0) {
      n = new Node(choose(UNOP), genExpr(fuel), null);
    } else {
      int l = rnd.nextInt(fuel - 1) + 1;
      int r = fuel - l;
      n = new Node(choose(BINOP), genExpr(l), genExpr(r));
    }
    return n;
  }

  public String toString() {
    return String.format(
        "(herbie-test (%s)\n  %s\n  %s)"
        , String.join(" ", vars)
        , name
        , expr.toString());
  }

  public class Node {
    private final String label;
    private final Node l;
    private final Node r;

    public Node(String label, Node l, Node r) {
      this.label = label;
      this.l = l;
      this.r = r;
    }

    public String toString() {
      if(l == null && r == null) {
        return label;
      }
      if(l != null && r == null) {
        return String.format("(%s %s)", label, l.toString());
      }
      if(l != null && r != null) {
        return String.format("(%s %s %s)", label, l.toString(), r.toString());
      }
      throw new Error("l == null but r != null");
    }
  }
}
