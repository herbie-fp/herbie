public class RandomTest {
  private static int nOps   = 1;
  private static int nVars  = 1;
  private static int nTests = 1;

  private static final String usage =
    "Usage: java RandomTest --nops N --nvars N --ntests N";

  private static class BogusCL extends Exception {
    String msg;

    public BogusCL(String m) {
      msg = m;
    }
  }

  private static int parseArgInt(String[] args, int i) throws BogusCL {
    try {
      return Integer.parseInt(args[i]);
    }
    catch(ArrayIndexOutOfBoundsException e) {
      throw new BogusCL(
        String.format("ERROR: expected integer argument at position %d", i));
    }
    catch(NumberFormatException e) {
      throw new BogusCL(
        String.format("ERROR: could not parse '%s' as integer", args[i]));
    }
  }

  private static void parseArgs(String[] args) throws BogusCL {
    int i = 0;
    while(i < args.length) {
      switch(args[i]) {
        case "-h":
        case "--help":
          System.out.println(usage);
          System.exit(0);
        case "-o":
        case "--nops":
          i++;
          nOps = parseArgInt(args, i);
          break;
        case "-v":
        case "--nvars":
          i++;
          nVars = parseArgInt(args, i);
          break;
        case "-t":
        case "--ntests":
          i++;
          nTests = parseArgInt(args, i);
          break;
        default:
          throw new BogusCL(
            String.format("ERROR: invalid argument '%s'", args[i]));
      }
      i++;
    }
  }

  public static void main(String[] args) {
    try {
      parseArgs(args);
    }
    catch(BogusCL e) {
      System.err.println(e.msg);
      System.err.println();
      System.err.println(usage);
      System.exit(1);
    }

    for(int i=0; i<nTests; i++) {
      System.out.println(new OperatorTree(nOps, nVars));
      System.out.println();
    }
  }
}
