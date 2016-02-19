import java.util.Random;

public class RandomTest {
  private static int size   = 1;
  private static int nVars  = 1;
  private static int nTests = 1;

  private static int sizeWiggle  = 0;
  private static int nVarsWiggle = 0;

  private static final String usage =
    String.join("\n"
      , "Usage: java RandomTest OPTIONS"
      , ""
      , "where OPTIONS include:"
      , "  --help              print this usage info and exit"
      , "  --size N            size of expressions to generate (default 1)"
      , "  --size-wiggle N     how much to wiggle to randomly add to size (default 0)"
      , "  --nvars N           number of variables in generated expressions (default 1)"
      , "  --nvars-wiggle N    how much wiggle to randomly add to number of vars (default 0)"
      , "  --ntests N          number of tests to generate (default 1)"
      );

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
        String.format("expected integer argument at position %d", i));
    }
    catch(NumberFormatException e) {
      throw new BogusCL(
        String.format("could not parse '%s' as integer", args[i]));
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
        case "-s":
        case "--size":
          i++;
          size = parseArgInt(args, i);
          if(size < 1) {
            throw new BogusCL(
              String.format("size must be positive, but got %d", size));
          }
          break;
        case "-sw":
        case "--size-wiggle":
          i++;
          sizeWiggle = parseArgInt(args, i);
          if(sizeWiggle < 0) {
            throw new BogusCL(
              String.format("wiggle must be nonnegative, but got %d", sizeWiggle));
          }
          break;
        case "-v":
        case "--nvars":
          i++;
          nVars = parseArgInt(args, i);
          if(nVars < 0) {
            throw new BogusCL(
              String.format("number of vars must be nonnegative, but got %d", nVars));
          }
          if(nVars + nVarsWiggle > 26) {
            throw new BogusCL(
              String.format("number of vars plus wiggle must be <= 26, but got %d"
                           , nVars + nVarsWiggle));
          }
          break;
        case "-vw":
        case "--nvars-wiggle":
          i++;
          nVarsWiggle = parseArgInt(args, i);
          if(nVarsWiggle < 0) {
            throw new BogusCL(
              String.format("wiggle must be nonnegative, but got %d", nVarsWiggle));
          }
          if(nVars + nVarsWiggle > 26) {
            throw new BogusCL(
              String.format("number of vars plus wiggle must be <= 26, but got %d"
                           , nVars + nVarsWiggle));
          }
          break;
        case "-t":
        case "--ntests":
          i++;
          nTests = parseArgInt(args, i);
          if(size < 1) {
            throw new BogusCL(
              String.format("number of tests must be positive, but got %d", nTests));
          }
          break;
        default:
          throw new BogusCL(
            String.format("invalid argument '%s'", args[i]));
      }
      i++;
    }
  }

  public static void main(String[] args) {
    try {
      parseArgs(args);
    }
    catch(BogusCL e) {
      System.err.println("ERROR: " + e.msg);
      System.err.println();
      System.err.println(usage);
      System.exit(1);
    }

    Random rnd = new Random();

    for(int i=0; i<nTests; i++) {
      int s = size + rnd.nextInt(sizeWiggle + 1);
      int v = nVars + rnd.nextInt(nVarsWiggle + 1);
      System.out.println(new OperatorTree(s, v));
      System.out.println();
    }
  }
}
