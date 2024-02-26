from typing import List
from subprocess import Popen, PIPE
from pathlib import Path

import matplotlib.pyplot as plt
import shutil
import re

from .fpcore import FPCore

fpcore_pat = re.compile('\(FPCore \(([^\(\)]*)\)')

def synthesize1(op: str, argc: int) -> FPCore:
        """Creates a single FPCore for an operation with an arity."""
        op_ = '-' if op == 'neg' else op
        vars = [f'x{i}' for i in range(argc)]
        arg_str = ' '.join(vars)
        app_str = '(' + ' '.join([op_] + vars) + ')'
        core = f'(FPCore ({arg_str}) :name "{op}" {app_str})'
        return FPCore(core, name=op, argc=argc)

class Runner(object):
    """Representing a runner for a given platform"""

    def __init__(
        self,
        lang: str,
        working_dir: str,
        herbie_path: str,
        time_unit: str,
        num_inputs: int = 10_000,
        num_runs: int = 100,
        threads: int = 1,
        unary_ops: List[str] = [],
        binary_ops: List[str] = [],
        ternary_ops: List[str] = []
    ):
        # configuration data
        self.lang = lang
        self.num_inputs = num_inputs
        self.num_runs = num_runs
        self.threads = threads
        self.working_dir = Path(working_dir)
        self.herbie_path = Path(herbie_path)
        self.unary_ops = unary_ops
        self.binary_ops = binary_ops
        self.ternary_ops = ternary_ops
        self.time_unit = time_unit
        # mutable data
        self.cores: List[FPCore] = []
        self.costs: List[float] = []
        self.driver_dirs: List[str] = []
        self.times: List[float] = []

        # if the working directory does not exist, create it
        if not self.working_dir.exists(): 
            self.working_dir.mkdir(parents=True)
            self.log('created working directory at `' + str(self.working_dir) + '`')

    def log(self, msg: str, *args):
        """Logging routine for this runner."""
        print(f'[Runner:{self.lang}]:', msg, *args)

    def synthesize(self):
        """For each operator, create an FPCore and append to `self.cores`."""
        for op in self.unary_ops:
            self.cores.append(synthesize1(op, 1))
        for op in self.binary_ops:
            self.cores.append(synthesize1(op, 2))
        for op in self.ternary_ops:
             self.cores.append(synthesize1(op, 3))

    def herbie_compile(self):
        """Compiles each FPCore in `self.cores` to the target language.
        This requires the target language to be supported by the
        \"compile\" command in the Racket script."""
        with Popen(
            args=['racket', str(self.herbie_path)],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            for core in self.cores:
                print(f'(compile {self.lang} {core.core})', file=server.stdin, flush=True)
                output = server.stdout.readline()
                core.compiled = output.replace('\\n', '\n').strip()

            # terminate the server
            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()
        self.log(f'compiled {len(self.cores)} cores')

    def herbie_cost(self):
        """Estimates the cost of an expression using Herbie's cost model."""
        with Popen(
            args=['racket', str(self.herbie_path)],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            for core in self.cores:
                print(f'(cost {core.core})', file=server.stdin, flush=True)
                output = server.stdout.readline()
                self.costs.append(float(output))

            # terminate the server
            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()
        self.log(f'computed cost esimates')

    def herbie_improve(self, path: str, threads: int = 1):
        """Runs Herbie improvement on benchmarks under `path`
        appending all resulting FPCores to `self.cores`."""
        with Popen(
            args=['racket', str(self.herbie_path)],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            print(f'(improve \"{path}\" {threads}) (exit)', file=server.stdin, flush=True)
            output = server.stdout.read()

        for line in output.split('\n'):
            core = line.strip()
            if len(core) > 0:
                core_match = re.match(fpcore_pat, core)
                arg_str = core_match.group(1).strip()
                argc = len(arg_str.split())
                self.cores.append(FPCore(core=core, argc=argc))

    def make_driver_dirs(self) -> List[str]:
        """Creates the subdirectories for each driver: one subdirectory
        per FPCore in `self.cores`. Returns the list of subdirectories.
        Likely a utility function for `make_drivers()`."""
        for i, _ in enumerate(self.cores):
            subdir = self.working_dir.joinpath(Path(str(i)))
            if subdir.exists():
                shutil.rmtree(subdir)
            subdir.mkdir()
            self.driver_dirs.append(subdir)
        self.log(f'prepared driver subdirectories')

    def make_drivers(self) -> None:
        """Creates drivers for each compiled FPCore.
        Assumes `compile()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def compile_drivers(self) -> None:
        """Compiles all drivers for each compiled FPCore.
        Assumes `make_drivers()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def run_drivers(self) -> None:
        """Runs all drivers for each compiled FPCore.
        Assumes `compile_drivers()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def print_times(self):
        """Prints driver times in a table."""
        print('op | time (ms)')
        table = [(core.name, time) for core, time in zip(self.cores, self.times)]
        table = sorted(table, key=lambda row: str.lower(row[0]))
        for name, time in table:
            print(f'[{name} {time}]')

    def plot_times(self):
        """Prints cost vs. runtime on a graph."""
        plt.scatter(self.costs, self.times)
        plt.title('Estimated cost vs. actual run time')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Run time ({self.time_unit})')
        plt.show()
