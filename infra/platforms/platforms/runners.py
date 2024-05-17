""" Constructs Runners """

from .runner import Runner

from .arith import ArithRunner
from .avx import AVXRunner
from .c import CRunner
from .math import MathRunner
from .mkl import MKLRunner
from .python import PythonRunner

def make_runner(
    lang: str,
    working_dir: str,
    herbie_path: str,
    num_inputs: int,
    num_runs: int,
    threads: int,
    key: str
) -> Runner:
    if lang == 'arith':
        return ArithRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'c':
        return CRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'math':
        return MathRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'mkl':
        return MKLRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'python':
        return PythonRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'avx':
        return AVXRunner(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads
        )
    else:
        raise ValueError(f'Unsupported output language: {lang}')
