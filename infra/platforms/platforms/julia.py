from subprocess import Popen, PIPE
from typing import List
from pathlib import Path
import os

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str

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
target_flags = ['--history-file=no']
driver_name = 'test.jl'
time_unit = 'ms'

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
        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            sample = self.cache.get_sample(core.key, self.seed)
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
                    print(']', file=f)

                arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                arg0_str = ', '.join(map(lambda i: f'x{i}[1]', range(core.argc)))
                
                print(f'function main()', file=f)
                print(f'\tv = 0', file=f)
                print(f'\tt0 = time_ns()', file=f)
                print(f'\tfor i = 1:{self.num_inputs}', file=f)
                print(f'\t\ttry', file=f)
                print(f'\t\t\tv = foo({arg_str})', file=f)
                print('\t\tcatch', file=f)
                print('\t\tend', file=f)
                print('\tend', file=f)
                print('\tt1 = time_ns()', file=f)
                print('\tprintln(t1 - t0)', file=f)
                print('end', file=f)

                print(f'try', file=f)
                print(f'\tfoo({arg0_str})', file=f) # pre-compile
                print(f'catch', file=f)
                print(f'end', file=f)
                print('main()', file=f)

        self.log(f'created drivers')
    
    def compile_drivers(self, driver_dirs: List[str]) -> None:
        self.log(f'drivers interpreted, skipping compilations')

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        # run processes sequentially
        times = [[] for _ in driver_dirs]
        for i, driver_dir in enumerate(driver_dirs):
            log_prefix = f'[{i}/{len(driver_dirs)}] '
            print(log_prefix, end='', flush=True)

            for _ in range(self.num_runs):
                driver_path = Path(os.path.join(driver_dir, driver_name))
                p = Popen([target] + target_flags + ['--', driver_path], stdout=PIPE)
                stdout, _ = p.communicate()
                output = stdout.decode('utf-8').strip()
                time_ns = float(output)
                times[i].append(time_ns / 1e6)
                print('.', end='', flush=True)

            # Reset terminal
            print('\r', end='', flush=True)
            print(' ' * (len(log_prefix) + self.num_runs), end='', flush=True)
            print('\r', end='', flush=True)
        
        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times
