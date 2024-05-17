from subprocess import Popen, PIPE
from typing import Optional, List
from pathlib import Path
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str

# Supported operations for Python
unary_ops = ('sind', 'cosd', 'tand', 'sinpi', 'cospi', 'sinh', 'cosh', 'tanh', 'asin', 'acos', 'atan', 'asind', 'acosd', 'atand', 'sec', 'csc', 'cot', 'secd', 'cscd', 'cotd', 'asec', 'acsc', 'acot', 'asecd', 'acscd', 'acotd', 'sech', 'csch', 'coth', 'asinh', 'acosh', 'atanh', 'asech', 'acsch', 'acoth', 'deg2rad', 'rad2deg', 'hypot', 'log', 'log2', 'log10', 'log1p', 'exp', 'exp2', 'exp10', 'ldexp', 'expm1', 'abs', 'abs2', 'sqrt', 'cbrt', 'powermod',
             )

binary_ops = ['+', '-', '*', '/', 'ldexp' 'expm1']

# Pyton lang
target = 'julia'
driver_name = 'main.jl'
time_unit = 'ms'

# Regex patterns
time_pat = re.compile(f'([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) {time_unit}')

class JuliaRunner(Runner):
    """`Runner` for Python 3.10"""
    
    def __init__(self, **kwargs):
        super().__init__(
            name='julia',
            lang='julia',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            time_unit='ms',
            **kwargs
        )

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            _, sample = self.cache.get_core(core.key)
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

                arg_str = ', '.join(map(lambda i: f'x{i}[j]', range(core.argc)))
                print(f'\ti = 0', file=f)
                print(f'\tstart = time.time_ns()', file=f)
                print(f'\twhile i < {self.num_inputs}:', file=f)
                print(f'\t\ttry:', file=f)
                print(f'\t\t\tfor j in range(i, {self.num_inputs}):', file=f)
                print(f'\t\t\t\tfoo({arg_str})', file=f)
                print(f'\t\t\t\ti += 1', file=f)
                print(f'\t\t\tend', file=f)
                print(f'\t\tcatch', file=f)
                print(f'\t\t\ti += 1', file=f)
                print(f'\t\tend', file=f)
                print(f'\tend', file=f)
                print(f'\tend = time.time_ns()', file=f)
                print(f'\tdiff = (10 ^ -6) * (end - start)', file=f)
                print(f'\tprintln(f\"$diff ms\")', file=f)

        self.log(f'created drivers')
    
    def compile_drivers(self, driver_dirs: List[str]) -> None:
        self.log(f'drivers interpreted, skipping compilations')

    def run_drivers(self, driver_dirs: List[str]) -> List[float]:
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
