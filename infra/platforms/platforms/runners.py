""" Constructs Runners """

from .runner import Runner

from .arith import ArithRunner
from .avx import AVXRunner
from .c import CRunner
from .math import MathRunner
from .mkl import MKLRunner
from .python import PythonRunner

def make_runner(
    platform: str,
    working_dir: str,
    herbie_path: str,
    num_inputs: int,
    num_runs: int,
    threads: int,
    key: str,
    seed: int
) -> Runner:
    kwargs = {
        'working_dir': working_dir,
        'herbie_path': herbie_path,
        'num_inputs': num_inputs,
        'num_runs': num_runs,
        'threads': threads,
        'key': key,
        'seed': seed
    }

    if platform == 'arith':
        return ArithRunner(**kwargs)
    elif platform == 'c':
        return CRunner(**kwargs)
    elif platform == 'math':
        return MathRunner(**kwargs)
    elif platform == 'mkl':
        return MKLRunner(**kwargs)
    elif platform == 'python':
        return PythonRunner(**kwargs)
    elif platform == 'avx':
        return AVXRunner(**kwargs)
    else:
        raise ValueError(f'Unsupported output platform: {platform}')