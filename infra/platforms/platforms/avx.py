from pathlib import Path
from subprocess import Popen, CalledProcessError
from typing import List
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str, chunks, run_subprocess

unary_ops = ['neg', 'floor', 'sqrt', 'round', 'ceil', 'fabs']
binary_ops = ['+', '-', '*', '/', 'fmax', 'fmin']
ternary_ops = ['fma', 'fmsub', 'fnmadd', 'fnmsub']

compiler = 'clang'
c_flags = ['-std=gnu11', '-ffp-contract=off', '-O2', '-mavx', '-mfma']
ld_flags = ['-lm']
driver_name = 'main.c'
time_unit = 'ms'

time_pat = re.compile(f'([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) {time_unit}')

class AVXRunner(Runner):
    def __init__(self, **kwargs):
        super().__init__(
            name='avx',
            lang='avx',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops,
            time_unit='ms',
            **kwargs,
        )

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            # pull sample from cache
            sample = self.cache.get_sample(core.key, self.seed)
            input_points, _ = sample
            with open(driver_path, 'w') as f:
                print('#include <immintrin.h>', file=f)
                print('#include <stdio.h>', file=f)
                print('#include <time.h>', file=f)

                print(f'static inline {core.compiled}', file=f)

                for i, points in enumerate(input_points):
                    print(f'__attribute__((aligned(32)))', file=f)
                    print(f'const double x{i}[{self.num_inputs}] = {{', file=f)
                    print(',\n'.join(map(double_to_c_str, points)), file=f)
                    print('};', file=f)

                print('int main() {', file=f)
                print(f'struct timespec ts1, ts2;', file=f)
                print(f'volatile __m256d res;', file=f)
                print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);', file=f)

                arg_str = ', '.join(map(lambda i: f'_mm256_load_pd(&x{i}[i])', range(core.argc)))
                # arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                app_str =  f'foo({arg_str})'
                print(f'for (long i = 0; i < {self.num_inputs}; i += 4) {{', file=f)
                print(f'  res = {app_str};', file=f)
                print('}', file=f)

                print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts2);', file=f)
                print(f'double diff = (1000.0 * ts2.tv_sec + 1e-6 * ts2.tv_nsec) - (1000.0 * ts1.tv_sec + 1e-6 * ts1.tv_nsec);', file=f)
                print(f'printf("%.17g {time_unit}", diff);', file=f)
                print('  return 0;', file=f)
                print('}', file=f)

        self.log(f'created drivers')

    def compile_drivers(self, driver_dirs: List[str]) -> None:
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
                rc = p.wait()
                if rc != 0:
                    raise CalledProcessError(rc, p.args, p.stdout, p.stderr)

        self.log(f'compiled drivers')

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        # run processes sequentially
        times = [[] for _ in driver_dirs]
        for i, driver_dir in enumerate(driver_dirs):
            for _ in range(self.num_runs):
                driver_path = Path(os.path.join(driver_dir, driver_name))
                out_path = driver_path.parent.joinpath(driver_path.stem)
                output = run_subprocess([out_path], capture_stdout=True)
                time = re.match(time_pat, output)
                if time is None:
                    self.log("bad core: "+str(cores[i]))
                    raise RuntimeError('Unexpected error when running {out_path}: {output}')
                times[i].append(float(time.group(1)))

        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
