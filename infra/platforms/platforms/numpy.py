from typing import List
from pathlib import Path
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str, run_subprocess

# Supported operations for NUMPY


unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'ceil', 'cos', 'cosh', 'exp', 'expm1', 'fabs', 'floor', 'log', 'log10', 'log2', 'log1p', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'trunc','recip','square','deg2rad','rad2deg','rint','round','exp2','cbrt']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fmax', 'fmin', 'fmod', 'pow', 'remainder','logaddexp','logaddexp2','hypot']

# Numpy lang
target = 'python3'
driver_name = 'main.py'
time_unit = 'ms'

# Regex patterns
time_pat = re.compile(f'([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) {time_unit}')

class NumpyRunner(Runner):
    """`Runner` for NUMPY"""
    
    def __init__(self, **kwargs):
        super().__init__(
            name='numpy',
            lang='numpy',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            time_unit='ms',
            **kwargs
        )

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            sample = self.cache.get_sample(core.key, self.seed)
            input_points, _ = sample
            with open(driver_path, 'w') as f:
                print('import time', file=f)
                print('import numpy', file=f)
                print('numpy.seterr(divide=\'ignore\',over=\'ignore\',invalid=\'ignore\')', file=f)
                print(f'{core.compiled}', file=f)

                for i, points in enumerate(input_points):
                    spoints = []
                    for pt in points:
                        s = double_to_c_str(pt)
                        if s == 'NAN':
                            spoints.append('numpy.nan')
                        else:
                            spoints.append(s)
        
                    print(f'x{i} = numpy.array([', file=f)
                    print(',\n'.join(spoints), file=f)
                    print('])', file=f)

                arg_str = ', '.join(map(lambda i: f'x{i}', range(core.argc)))
                print('if __name__ == "__main__":', file=f)
                print(f'\tfor _ in range({self.num_runs}):', file=f)
                print(f'\t\tstart = time.time_ns()', file=f)
                print(f'\t\tfoo({arg_str})', file=f)
                print(f'\t\tend = time.time_ns()', file=f)
                print(f'\t\tdiff = (10 ** -6) * (end - start)', file=f)
                print(f'\t\tprint(f\'{{diff}} ms\')', file=f)

        self.log(f'created drivers')
    
    def compile_drivers(self, driver_dirs: List[str]) -> None:
        self.log(f'drivers interpreted, skipping compilations')

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        # run processes sequentially
        times = [[] for _ in driver_dirs]
        for i, driver_dir in enumerate(driver_dirs):
            driver_path = Path(os.path.join(driver_dir, driver_name))
            log_prefix = f'[{i}/{len(driver_dirs)}] '
            print(log_prefix, end='', flush=True)

            output = run_subprocess([target, driver_path], capture_stdout=True)
            for line in output.strip().split('\n'):
                time = re.match(time_pat, line)
                if time is None:
                    self.log("bad core: "+str(cores[i]))
                    raise RuntimeError('Unexpected error when running {out_path}: {output}')
                times[i].append(float(time.group(1)))

            # Reset terminal
            print('\r', end='', flush=True)
            print(' ' * (len(log_prefix)), end='', flush=True)
            print('\r', end='', flush=True)
        
        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
