from subprocess import Popen, PIPE
from typing import Optional, List
from pathlib import Path
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str

# Supported operations for NUMPY


unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'ceil', 'cos', 'cosh', 'exp', 'expm1', 'fabs', 'floor', 'log', 'log10', 'log2', 'log1p', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'trunc','recip','square','deg2rad','rad2deg','rint','round','exp2','cbrt']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fmax', 'fmin', 'fmod', 'pow', 'remainder','logaddexp','logaddexp2','hypot']

#unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'ceil', 'cos', 'cosh', 'exp', 'expm1', 'fabs', 'floor', 'log', 'log10', 'log2', 'log1p', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'trunc','recip']
#binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fmax', 'fmin', 'fmod', 'pow', 'remainder']

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
        self.log(f'starting make_ drivers')

        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            _, sample = self.cache.get_core(core.key)
            input_points, _ = sample
            with open(driver_path, 'w') as f:
                print('import time', file=f)
                print('import numpy', file=f)
                print(f'{core.compiled}', file=f)

                spoints = []
                for i, points in enumerate(input_points):
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
                print(f'\ti = 0', file=f)
                print(f'\tstart = time.time_ns()', file=f)
                #print(f'\twhile i < {self.num_inputs}:', file=f)
                print(f'\twhile i < {self.num_inputs}:', file=f)
                print(f'\t\ttry:', file=f)
                print(f'\t\t\tfoo({arg_str})', file=f)
                print(f'\t\t\ti += 1', file=f)
                print(f'\t\texcept:', file=f)
                print(f'\t\t\ti += 1', file=f)
                print(f'\tend = time.time_ns()', file=f)
                print(f'\tdiff = (10 ** -6) * (end - start)', file=f)
                print(f'\tprint(f\'{{diff}} ms\')', file=f)

        self.log(f'created drivers')
    
    def compile_drivers(self, driver_dirs: List[str]) -> None:
        self.log(f'drivers interpreted, skipping compilations')

    def run_drivers(self, driver_dirs: List[str]) -> List[float]:
        print("starting run drivers")
        # run processes sequentially
        times = [[] for _ in driver_dirs]
        for i, driver_dir in enumerate(driver_dirs):
            for _ in range(self.num_runs):
                driver_path = Path(os.path.join(driver_dir, driver_name))
                p = Popen([target, driver_path], stdout=PIPE)
                stdout, _ = p.communicate()
                output = stdout.decode('utf-8')
                time = re.match(time_pat, output)
                if time is None:
                    raise RuntimeError('Unexpected error when running {out_path}: {output}')
                times[i].append(float(time.group(1)))
                print('.', end='', flush=True)
            print('x', end='', flush=True)
        print()
        
        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
