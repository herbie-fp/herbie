from subprocess import Popen, PIPE
from typing import List
from pathlib import Path
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str, run_subprocess

# Supported operations for Python
unary_ops = [
    'neg', 'sin', 'cos', 'tan', 'sinpi', 'cospi',
    'sind', 'cosd', 'tand',  'sinh', 'cosh', 'tanh', 'asin', 'acos', 'atan',
    'asind', 'acosd', 'atand', 'sec', 'csc', 'cot', 'secd', 'cscd', 'cotd', 'asec', 'acsc',
    'acot', 'asecd', 'acscd', 'acotd', 'sech', 'csch', 'coth', 'asinh', 'acosh', 'atanh',
    'asech', 'acsch', 'acoth', 'deg2rad', 'rad2deg', 'log', 'log2', 'log10', 'log1p',
    'exp', 'exp2', 'exp10', 'expm1', 'fabs', 'abs2', 'sqrt', 'cbrt']
binary_ops = ['+', '-', '*', '/', 'hypot', 'fmin', 'fmax', 'pow']
ternary_ops = ['fma']

# Julia lang
target = 'julia'
target_flags = ['--history-file=no', '--compile=min', '-O0']
driver_name = 'test.jl'
time_unit = 'ms'

# Regex patterns
points_pat = re.compile('# points ([0-9]+)')

class JuliaRunner(Runner):
    """`Runner` for Julia"""
    
    def __init__(self, **kwargs):
        super().__init__(
            name='julia',
            lang='julia',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops,
            time_unit='ms',
            **kwargs
        )

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        # extract samples
        samples = []
        for core in cores:
            sample = self.cache.get_sample(core.key, self.seed)
            samples.append(sample)

        # create a temporary driver to eliminate exception points
        for core, driver_dir, sample in zip(cores, driver_dirs, samples):
            driver_path = os.path.join(driver_dir, driver_name)
            input_points, _ = sample
            with open(driver_path, 'w') as f:
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
                    print('] :: Vector{Float64}', file=f)

                arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                arg0_str = ', '.join(map(lambda i: f'x{i}[1]', range(core.argc)))
                
                print(f'function main()', file=f)
                print(f'\tfor i = 1:{self.num_inputs}', file=f)
                print(f'\t\ttry', file=f)
                print(f'\t\t\tfoo({arg_str})', file=f)
                print(f'\t\t\tprint("1 ")', file=f)
                print('\t\tcatch', file=f)
                print(f'\t\t\tprint("0 ")', file=f)
                print('\t\tend', file=f)
                print('\tend', file=f)
                print('end', file=f)

                print(f'try', file=f)
                print(f'\tfoo({arg0_str})', file=f) # pre-compile
                print(f'catch', file=f)
                print(f'end', file=f)
                print('main()', file=f)

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
            self.log(f'kept {num_inputs}/{len(by_points)} sampled points')

            # create actual driver using pruned points
            with open(driver_path, 'w') as f:
                print(f'# points {num_inputs}', file=f)
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
                    print('] :: Vector{Float64}', file=f)

                arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                arg0_str = ', '.join(map(lambda i: f'x{i}[1]', range(core.argc)))
                
                print(f'function main()', file=f)
                print('\tv = 0', file=f)
                print(f'\tt0 = time_ns()', file=f)
                print(f'\tfor i = 1:{num_inputs}', file=f)
                print(f'\t\tv = foo({arg_str})', file=f)
                print('\tend', file=f)
                print('\tt1 = time_ns()', file=f)
                print('\tprintln(t1 - t0)', file=f)
                print('\treturn v', file=f)
                print('end', file=f)

                print(f'foo({arg0_str})', file=f) # pre-compile
                print(f'for i = 1:{self.num_runs}', file=f)
                print('\tmain()', file=f)
                print('end', file=f)

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
            log_prefix = f'[{i}/{len(driver_dirs)}]'
            print(log_prefix, end='', flush=True)
    
            output = run_subprocess([target] + target_flags + ['--', driver_path], capture_stdout=True)  
            for time_str in output.strip().split('\n'):
                time = float(time_str) / 1e6
                times[i].append(time * (self.num_inputs / num_pointss[i]))

            # Reset terminal
            print('\r', end='', flush=True)
            print(' ' * (len(log_prefix)), end='', flush=True)
            print('\r', end='', flush=True)
        
        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
