from subprocess import Popen, PIPE

from .runner import Runner
from .fpcore import FPCore

# Support operations in standard C
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'expm1', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'log1p', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'hypot', 'pow', 'remainder']
ternary_ops = ['fma']

# C lang
target_lang = 'c'
compiler = 'cc'
c_flags = ['-std=c11', '-O3']
ld_flags = ['-lm']

class CRunner(Runner):
    """`Runner` for standard C."""

    def __init__(self, working_dir: str, herbie_path: str):
        super().__init__(
            name='C',
            working_dir=working_dir,
            herbie_path=herbie_path,
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops
        )

    def compile(self) -> None:
        with Popen(
            args=['racket', str(self.herbie_path)],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            # need to do some munging of newlines
            for core in self.cores:
                print(f'compile c {core.core}', file=server.stdin, flush=True)
                output = server.stdout.readline()
                core.compiled = output.replace('\\n', '\n').strip()

            # terminate the server
            print('exit', file=server.stdin, flush=True)
            _ = server.stdout.readline()
        self.log(f'compiled {len(self.cores)} cores')

    def make_drivers(self) -> None:
        driver_dirs = self.make_driver_dirs()
        for core, driver_dir in zip(self.cores, driver_dirs):
            pass
        self.log(f'created drivers')
