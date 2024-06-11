from subprocess import Popen, PIPE
from typing import Optional, List
from pathlib import Path
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str, chunks

# Support operations with Intel MKL
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'expm1', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'log1p', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'hypot', 'pow', 'remainder']
ternary_ops = []

# C lang
target_lang = 'mkl'
compiler = 'cc'
c_flags = ['-g', '-O3', '-march=skylake']
ld_flags = ['-lmkl_intel_lp64', '-lmkl_sequential', '-lmkl_core', '-lpthread', '-lm']
driver_name = 'main.c'
time_unit = 'ms'

# Regex patterns
time_pat = re.compile(f'([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) {time_unit}')

class MKLRunner(Runner):
    """`Runner` for Intel MKL."""
    
    def __init__(self, **kwargs):
        super().__init__(
            name='mkl',
            lang='mkl',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops,
            time_unit='ms',
            **kwargs
        )

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            sample = self.cache.get_sample(core.key, self.seed)
            input_points, _ = sample
            with open(driver_path, 'w') as f:
                print('#include <math.h>', file=f)
                print('#include <stdio.h>', file=f)
                print('#include <stdlib.h>', file=f)
                print('#include <time.h>', file=f)
                print('#include <mkl.h>', file=f)
                print('#define TRUE 1', file=f)
                print('#define FALSE 0', file=f)

                print(core.compiled, file=f)

                for i, points in enumerate(input_points):
                    print(f'const double x{i}[{self.num_inputs}] = {{', file=f)
                    print(',\n'.join(map(double_to_c_str, points)), file=f)
                    print('};', file=f)

                print('int main() {', file=f)
                print(f'struct timespec ts1, ts2;', file=f)
                print(f'double res[{self.num_inputs}];', file=f)
                print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);', file=f)

                args = [f'{self.num_inputs}'] + list(map(lambda i: f'x{i}', range(core.argc))) + ['res']
                arg_str = ', '.join(args)
                print(f'foo({arg_str});', file=f)

                print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts2);', file=f)
                print(f'double diff = (1000.0 * ts2.tv_sec + 1e-6 * ts2.tv_nsec) - (1000.0 * ts1.tv_sec + 1e-6 * ts1.tv_nsec);', file=f)
                print(f'printf("%.17g {time_unit}", diff);', file=f)
                print('  return 0;', file=f)
                print('}', file=f)

        self.log(f'created drivers')

    def compile_drivers(self, driver_dirs: List[str]) -> None:
        # chunk over threads
        for driver_dirs in chunks(driver_dirs, self.threads):
            ps = []
            # fork subprocesses
            for driver_dir in driver_dirs:
                driver_path = Path(os.path.join(driver_dir, driver_name))
                out_path = driver_path.parent.joinpath(driver_path.stem)
                p = Popen([compiler] + c_flags + ['-o', out_path, driver_path] + ld_flags)
                ps.append(p)
            # join processes
            for p in ps:
                _, _ = p.communicate()
        self.log(f'compiled drivers')

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        # run processes sequentially
        times = [[] for _ in driver_dirs]
        for i, driver_dir in enumerate(driver_dirs):
            for _ in range(self.num_runs):
                driver_path = Path(os.path.join(driver_dir, driver_name))
                out_path = driver_path.parent.joinpath(driver_path.stem)
                p = Popen([out_path], stdout=PIPE)
                stdout, _ = p.communicate()
                output = stdout.decode('utf-8')
                time = re.match(time_pat, output)
                if time is None:
                    self.log("bad core: "+str(cores[i]))
                    raise RuntimeError(f'Unexpected error when running {out_path}: {output}')
                times[i].append(float(time.group(1)))

        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
