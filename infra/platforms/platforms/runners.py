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
    key: str
) -> Runner:
    if platform == 'arith':
        return ArithRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif platform == 'c':
        return CRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif platform == 'math':
        return MathRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif platform == 'mkl':
        return MKLRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif platform == 'python':
        return PythonRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif platform == 'avx':
        return AVXRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads
        )
    else:
        raise ValueError(f'Unsupported output platformuage: {platform}')
