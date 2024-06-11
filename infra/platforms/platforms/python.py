from typing import List
from pathlib import Path
import os
import re

from .cache import SampleType
from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str, run_subprocess

# Supported operations for Python
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'expm1', 'fabs', 'floor', 'log', 'log10', 'log2', 'log1p', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'trunc']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fmax', 'fmin', 'fmod', 'hypot', 'pow', 'remainder']
ternary_ops = []
nary_ops = [(3, 'sum3'), (4, 'sum4')]

# Pyton lang
target = 'python3'
driver_name = 'main.py'
time_unit = 'ms'

# Regex patterns
time_pat = re.compile(f'([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) {time_unit}')
points_pat = re.compile('# points ([0-9]+)')

class PythonRunner(Runner):
    """`Runner` for Python 3.10"""
    
    def __init__(self, **kwargs):
        super().__init__(
            name='python',
            lang='python',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops,
            nary_ops=nary_ops,
            time_unit='ms',
            **kwargs
        )


    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        # extract samples
        samples = []
        for core in cores:
            samples.append(self.cache.get_sample(core.key, self.seed))

        # create a temporary driver to eliminate exception points
        for core, driver_dir, sample in zip(cores, driver_dirs, samples):
            driver_path = os.path.join(driver_dir, driver_name)
            input_points, _ = sample
            num_inputs = self.num_inputs
            with open(driver_path, 'w') as f:
                print('import math', file=f)
                print('import time', file=f)
                print(f'{core.compiled}', file=f)
    
                spoints = []
                for i, points in enumerate(input_points):
                    for pt in points:
                        s = double_to_c_str(pt)
                        if s == 'NAN':
                            spoints.append('math.nan')
                        else:
                            spoints.append(s)
    
                    print(f'x{i} = [', file=f)
                    print(',\n'.join(spoints), file=f)
                    print(']', file=f)
            
                arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                print('if __name__ == "__main__":', file=f)
                print(f'\tfor i in range(0, {num_inputs}):', file=f)
                print(f'\t\ttry:', file=f)
                print(f'\t\t\tfoo({arg_str})', file=f)
                print(f'\t\t\tprint(\'1 \', end=\'\')', file=f)
                print(f'\t\texcept:', file=f)
                print(f'\t\t\tprint(\'0 \', end=\'\')', file=f)

            # run temporary driver
            output = run_subprocess([target, driver_path], capture_stdout=True)
            no_excepts = list(map(lambda s: s == '1', output.strip().split()))

            # prune points that cause exceptions
            by_points = list(zip(*input_points))
            pruned_by_points = []
            for pt, no_except in zip(by_points, no_excepts):
                if no_except:
                    pruned_by_points.append(pt)
            
            input_points = list(zip(*pruned_by_points))
            num_inputs = len(pruned_by_points)
            self.log(f'kept {num_inputs}/{len(by_points)} sampled points for {core.name}')

            # create actual driver using pruned points
            with open(driver_path, 'w') as f:
                print(f'# points {num_inputs}', file=f)
                print('import math', file=f)
                print('import time', file=f)
                print(f'{core.compiled}', file=f)
    
                spoints = []
                for i, points in enumerate(input_points):
                    for pt in points:
                        s = double_to_c_str(pt)
                        if s == 'NAN':
                            spoints.append('math.nan')
                        else:
                            spoints.append(s)
    
                    print(f'x{i} = [', file=f)
                    print(',\n'.join(spoints), file=f)
                    print(']', file=f)
    
                arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                print('if __name__ == "__main__":', file=f)
                print(f'\tfor _ in range({self.num_runs}):', file=f)
                print(f'\t\tstart = time.time_ns()', file=f)
                print(f'\t\tfor i in range(0, {num_inputs}):', file=f)
                print(f'\t\t\tfoo({arg_str})', file=f)
                print(f'\t\tend = time.time_ns()', file=f)
                print(f'\t\tdiff = (10 ** -6) * (end - start)', file=f)
                print(f'\t\tprint(f\'{{diff}} ms\')', file=f)

        self.log(f'created drivers')
    
    def compile_drivers(self, driver_dirs: List[str]) -> None:
        self.log(f'drivers interpreted, skipping compilations')

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        # read number of points back in
        num_pointss = []
        for driver_dir in driver_dirs:
            driver_path = Path(os.path.join(driver_dir, driver_name))
            with open(driver_path, 'r') as f:
                matches = re.match(points_pat, f.readline())
                num_pointss.append(int(matches.group(1)))

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
                    self.log("bad core: " + str(cores[i]))
                    raise RuntimeError(f'Unexpected error when running {driver_path}: {output}')
                
                time = float(time.group(1))
                times[i].append(time * (self.num_inputs / num_pointss[i]))

            # Reset terminal
            print('\r', end='', flush=True)
            print(' ' * (len(log_prefix) + self.num_runs), end='', flush=True)
            print('\r', end='', flush=True)
        
        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
