![Herbie](logo.png)


Herbie automatically improves the error of floating point expressions.
Visit [our website](https://herbie.uwplse.org) for tutorials,
documentation, and an online demo. Herbie is a joint project of the
Universities of [Washington](https://uwplse.org) and
[Utah](https://cpu.cs.utah.edu).

## Installing

We recommend installing Herbie from the Racket Package Archive. To do
so, [install Racket](https://download.racket-lang.org/) and then run:

    raco pkg install --auto herbie

You can then run `racket -l herbie` to run Herbie. Herbie supports
Windows, Linux, and macOS on both x86 and AArch64. For full
instructions, see the
[documentation](https://herbie.uwplse.org/doc/latest/installing.html).

## Installing from Source

You can install Herbie from source if you want to participate in
Herbie development. This requires
[Racket](https://download.racket-lang.org/) (8.0 or later) and
[Rust](https://www.rust-lang.org/tools/install) (1.60.0 or later).
On Linux, avoid the Snap installer for Racket. Then, download the
this repository and run:

    make install

You can then run `racket -l herbie` to run Herbie, or run
`src/main.rkt` directly.

## Running Herbie

You can run Herbie's web interface with:

    $ racket -l herbie web

For more information on running Herbie, please see the
[tutorial](https://herbie.uwplse.org/doc/latest/using-web.html).

You can also use Herbie from the command line:

    $ racket -l herbie shell
    Herbie 1.3 with seed 1866513483
    Find help on https://herbie.uwplse.org/, exit with Ctrl-D
    herbie> (FPCore (x) (- (+ 1 x) x))
    (FPCore (x)
      ...
      1)

Here the input is the program `(1 + x) - x` and the output is `1`. The
input format is [FPCore](https://fpbench.org/spec/fpcore-1.2.html);
you can see more examples in `bench/`.

Besides `shell`, Herbie has batch the `improve` and `report` commands.
The [documentation](https://herbie.uwplse.org/doc/latest/options.html)
has more details.
