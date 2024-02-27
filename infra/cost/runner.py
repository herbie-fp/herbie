from typing import List, Tuple
from subprocess import Popen, PIPE
from pathlib import Path

import multiprocessing as mp
import matplotlib.pyplot as plt
import shutil
import re

from .fpcore import FPCore
from .util import sample_repr

fpcore_pat = re.compile('\(FPCore \(([^\(\)]*)\)')
fpcore_prop_name_pat = re.compile('^.*:name "([^\"]*)"')
fpcore_name_pat = re.compile('(.*) variant (.*)')

def baseline() -> FPCore:
    return FPCore(core='(FPCore () :name "baseline" 0)', name='baseline', argc=0)

def synthesize1(op: str, argc: int) -> FPCore:
    """Creates a single FPCore for an operation with an arity."""
    op_ = '-' if op == 'neg' else op
    vars = [f'x{i}' for i in range(argc)]
    arg_str = ' '.join(vars)
    app_str = '(' + ' '.join([op_] + vars) + ')'
    core = f'(FPCore ({arg_str}) :name "{op}" {app_str})'
    py_sample = op == 'lgamma' or op == 'tgamma'  # Rival struggles with these
    return FPCore(core, name=op, argc=argc, py_sample=py_sample)

def sample1(config: Tuple[FPCore, int, str, str]) -> List[float]:
    core, num_inputs, herbie_path, platform = config
    if core.argc == 0:
        return []
    elif core.py_sample:
        # sample using Python
        return [sample_repr('double', num_inputs) for _ in range(core.argc)]
    else:
        # sample using Herbie
        with Popen(
                args=['racket', str(herbie_path), "--platform", platform],
                stdin=PIPE,
                stdout=PIPE,
                universal_newlines=True) as server:
            
            print(f'(sample {num_inputs} {core.core})', file=server.stdin, flush=True)
            output = server.stdout.readline()
            inputs = output.split('|')
            if len(inputs) != num_inputs:
                print('(exit)', file=server.stdin, flush=True)
                raise RuntimeError(f'did not sample expected number of points: {len(inputs)} != {num_inputs}')

            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()
    
            points = [[] for _ in range(core.argc)]
            for input in inputs:
                for i, val in enumerate(input.split(',')):
                    points[i].append(float(val.strip()))
            return points


class Runner(object):
    """Representing a runner for a given platform"""

    def __init__(
        self,
        name: str,
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
        self.name = name
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
        self.samples: List[Tuple[FPCore, List[float]]] = []

        # if the working directory does not exist, create it
        if not self.working_dir.exists(): 
            self.working_dir.mkdir(parents=True)
            self.log('created working directory at `' + str(self.working_dir) + '`')

    def log(self, msg: str, *args):
        """Logging routine for this runner."""
        print(f'[Runner:{self.name}]:', msg, *args)

    def synthesize(self):
        """For each operator, create an FPCore and append to `self.cores`."""
        self.cores.append(baseline())
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
            args=['racket', str(self.herbie_path), "--platform", self.name],
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
            args=['racket', str(self.herbie_path), "--platform", self.name],
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
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            print(f'(improve \"{path}\" {threads}) (exit)', file=server.stdin, flush=True)
            output = server.stdout.read()

        for line in output.split('\n'):
            core = line.strip()
            if len(core) > 0:
                # extract argument count
                core_match = re.match(fpcore_pat, core)
                arg_str = core_match.group(1).strip()
                argc = len(arg_str.split())
                # optionally extract name
                name_match = re.match(fpcore_prop_name_pat, core)
                name = None if name_match is None else name_match.group(1).strip()
                self.cores.append(FPCore(core=core, name=name, argc=argc))
        self.log(f'extracted Herbie output')

    def herbie_sample(self):
        """Runs Herbie's sampler for each FPCore in `self.cores`."""
        # organize fpcores by sample first
        # this is using a heuristic that names are formatted a certain way!
        named_cores = dict()
        unnamed_cores = []
        for core in self.cores:
            if core.name is None:
                unnamed_cores.append(core)
            else:
                m = re.match(fpcore_name_pat, core.name)
                if m is None:
                    if core.name in named_cores:
                        raise RuntimeError(f'Duplicate FPCore name found: {core.name}')
                    named_cores[core.name] = [core]
                else:
                    base_name = m.group(1)
                    if base_name in named_cores:
                        named_cores[base_name].append(core)
                    else:
                        named_cores[base_name] = [core]
        cores_by_sample = unnamed_cores + list(named_cores.values())
        
        # actually run the sample
        config_gen = map(lambda cs: (cs[0], self.num_inputs, self.herbie_path, self.name), cores_by_sample)
        with mp.Pool(processes=self.threads) as pool:
            samples = pool.map(sample1, config_gen)

        for cores, sample in zip(cores_by_sample, samples):
            for core in cores:
                self.samples.append((core, sample))
        self.log(f'sampled input points')

    def get_sample(self, core: FPCore) -> List[float]:
        """Extracts a cached sample for a given FPCore."""
        for core_, sample in self.samples:
            if core == core_:
                return sample
        raise RuntimeError(f'Sample not cached for {core}')

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
        # Print baseline
        for name, time in table:
            if name == 'baseline':
                print(f'baseline: {time}')
        # Print table
        for name, time in table:
            if name != 'baseline':
                print(f'[{name} {time}]')

    def plot_times(self):
        """Prints cost vs. runtime on a graph."""
        plt.scatter(self.costs, self.times)
        plt.title('Estimated cost vs. actual run time')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Run time ({self.time_unit})')
        plt.show()
