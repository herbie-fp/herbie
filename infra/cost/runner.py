from typing import List, Tuple, Optional
from subprocess import Popen, PIPE
from pathlib import Path

import multiprocessing as mp
import matplotlib.pyplot as plt
import shutil
import re

from .fpcore import FPCore
from .util import sample_repr, chunks

fpcore_pat = re.compile('\(FPCore \(([^\(\)]*)\)')
fpcore_prop_name_pat = re.compile('^.*:name "([^\"]*)"')
fpcore_prop_descr_pat = re.compile('^.*:description "([^\"]*)"')
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

def parse_core(s: str) -> FPCore:
    """Parses a string as an FPCore."""
    core_match = re.match(fpcore_pat, s)
    if core_match is None:
        raise RuntimeError('Failed to parse FPCore from', s)
    arg_str = core_match.group(1).strip()
    argc = len(arg_str.split())
    # optionally extract name
    name_match = re.match(fpcore_prop_name_pat, s)
    name = None if name_match is None else name_match.group(1).strip()
    # optionally extract description
    descr_match = re.match(fpcore_prop_descr_pat, s)
    descr = None if descr_match is None else descr_match.group(1).strip()
    return FPCore(core=s, name=name, descr=descr, argc=argc)

def sample1(config: Tuple[FPCore, int, str, str]) -> List[float]:
    core, num_inputs, herbie_path, platform, py_sample = config
    if core.argc == 0:
        return []
    elif core.py_sample or py_sample:
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
            _ = server.stdout.read()
    
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
        ternary_ops: List[str] = [],
        nary_ops: List[Tuple[int, str]] = []
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
        self.nary_ops = nary_ops
        self.time_unit = time_unit
        # if the working directory does not exist, create it
        if not self.working_dir.exists(): 
            self.working_dir.mkdir(parents=True)
            self.log('created working directory at `' + str(self.working_dir) + '`')

    def log(self, msg: str, *args):
        """Logging routine for this runner."""
        print(f'[Runner:{self.name}]:', msg, *args)

    def synthesize(self):
        """Return an FPCore for all operators."""
        cores = [baseline()]
        for op in self.unary_ops:
            cores.append(synthesize1(op, 1))
        for op in self.binary_ops:
            cores.append(synthesize1(op, 2))
        for op in self.ternary_ops:
            cores.append(synthesize1(op, 3))
        for n, op in self.nary_ops:
            cores.append(synthesize1(op, n))
        return cores
    
    def herbie_read(self, path: str) -> List[FPCore]:
        """Reads a benchmark suite from `path` returning all FPCores found."""
        with Popen(
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            print(f'(read \"{path}\") (exit)', file=server.stdin, flush=True)
            output = server.stdout.read()

        cores = []
        for line in output.split('\n'):
            if len(line) > 0:
                cores.append(parse_core(line.strip()))
        return cores

    def herbie_compile(self, cores: List[FPCore]):
        """Compiles each FPCore in `cores` to the target language.
        This requires the target language to be supported by the
        \"compile\" command in the Racket script."""
        with Popen(
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            for core in cores:
                print(f'(compile {self.lang} {core.core})', file=server.stdin, flush=True)
                output = server.stdout.readline()
                core.compiled = output.replace('\\n', '\n').strip()

            # terminate the server
            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()
        self.log(f'compiled {len(cores)} cores')

    def herbie_cost(self, cores: List[FPCore]) -> None:
        """Computes the cost of each FPCore, overriding the `cost` variable of each FPCore."""
        with Popen(
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            for core in cores:
                print(f'(cost {core.core})', file=server.stdin, flush=True)
                output = server.stdout.readline()
                core.cost = float(output.strip())

            # terminate the server
            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()
        self.log(f'recomputed cost of {len(cores)} cores')

    def herbie_desugar(self, cores: List[FPCore]) -> List[FPCore]:
        """Attempts to desugar an FPCore generated in another platform into the platform
        represented by this `Runner`. If desugaring fails, the FPCore is removed."""
        desugared = []
        with Popen(
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            for core in cores:
                print(f'(desugar {core.core})', file=server.stdin, flush=True)
                output = server.stdout.readline()

            # terminate the server
            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()

        self.log(f'desugared {len(cores)} cores')
        return desugared

    def herbie_improve(
            self,
            cores: List[FPCore],
            threads: int = 1,
            platform: Optional[str] = None
    ):
        """Runs Herbie improvement on benchmarks under `path`
        appending all resulting FPCores to `self.cores`."""
        if platform is None:
            platform = self.name

        with Popen(
            args=['racket', str(self.herbie_path), "--platform", platform],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            core_strs = ' '.join(map(lambda c: c.core, cores))
            print(f'(improve ({core_strs}) {threads}) (exit)', file=server.stdin, flush=True)
            output = server.stdout.read()

        cores = []
        for group in chunks(output.split('\n'), 3):
            if len(group) == 3:
                core = parse_core(group[0].strip())
                core.cost = float(group[1].strip())
                core.err = float(group[2].strip())
                cores.append(core)

        self.log(f'generated FPCores with Herbie')
        return cores

    def herbie_pareto(self, cores: List[FPCore]) -> List[Tuple[float, float]]:
        """Runs Herbie's pareto frontier algorithm."""
        # assuming all FPCores have names at this point
        cores_by_group = dict()
        for core in cores:
            if core.name is None:
                raise RuntimeError('FPCore does not have name', core)
            if core.name in cores_by_group:
                cores_by_group[core.name].append(core)
            else:
                cores_by_group[core.name] = [core]

        with Popen(
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            frontiers = []
            for key in cores_by_group:
                cores = cores_by_group[key]
                frontier = ' '.join(list(map(lambda c: f'({c.cost} {c.err})', cores)))
                frontiers.append(f'({frontier})')

            # call out to server
            args = ' '.join(frontiers)
            print(f'(pareto {args})', file=server.stdin, flush=True)
            output = server.stdout.readline()

            # shutdown server
            print(f'(exit)', file=server.stdin, flush=True)
            _ = server.stdout.read()

        frontier = []
        for line in output.split('|'):
            datum = line.split(' ')
            if len(datum) != 2:
                raise RuntimeError('Pareto frontier malformed:', datum)

            cost = float(datum[0])
            err = float(datum[1])
            frontier.append((cost, err))

        self.log(f'computed Pareto frontier')
        return frontier
    

    def herbie_sample(self, cores: List[FPCore], py_sample: bool = False) -> dict:
        """Runs Herbie's sampler for each FPCore in `self.cores`."""
        config_gen = map(lambda c: (c, self.num_inputs, self.herbie_path, self.name, py_sample), cores)
        with mp.Pool(processes=self.threads) as pool:
            sample_data = pool.map(sample1, config_gen)

        samples = dict()
        for core, data in zip(cores, sample_data):
            samples[core.name] = data
        self.log(f'sampled input points')
        return samples

    def make_driver_dirs(self, cores: List[FPCore]) -> List[str]:
        """Creates the subdirectories for each driver: one subdirectory
        per FPCore in `cores`. Returns the list of subdirectories.
        Likely a utility function for `make_drivers()`."""
        driver_dirs = []
        for i, _ in enumerate(cores):
            subdir = self.working_dir.joinpath(Path(str(i)))
            if subdir.exists():
                shutil.rmtree(subdir)
            subdir.mkdir()
            driver_dirs.append(subdir)
        self.log(f'prepared driver subdirectories')
        return driver_dirs

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> None:
        """Creates drivers for each compiled FPCore.
        Assumes `compile()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def compile_drivers(self, driver_dirs: List[str]) -> None:
        """Compiles all drivers for each compiled FPCore.
        Assumes `make_drivers()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def run_drivers(self, driver_dirs: List[str]) -> List[float]:
        """Runs all drivers for each compiled FPCore.
        Assumes `compile_drivers()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def print_times(self, cores: List[FPCore], times: List[float]):
        """Prints driver times in a table."""
        print('op | time (ms)')
        table = [(core.name, time) for core, time in zip(cores, times)]
        table = sorted(table, key=lambda row: str.lower(row[0]))
        # Print baseline
        for name, time in table:
            if name == 'baseline':
                print(f'baseline: {time}')
        # Print table
        for name, time in table:
            if name != 'baseline':
                print(f'[{name} {time}]')

    def plot_times(self, cores: List[FPCore], times: List[float]):
        """Plots Herbie cost estimate vs. actual run time."""
        costs = list(map(lambda c: c.cost, cores))
        plt.scatter(costs, times)
        plt.title('Estimated cost vs. actual run time')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Run time ({self.time_unit})')
        plt.show()

    def plot_pareto(self, frontier: List[Tuple[float, float]]):
        """Plots cost vs. accuracy Pareto frontier."""
        costs = []
        errs = []
        for cost, err in frontier:
            costs.append(cost)
            errs.append(err)

        plt.plot(costs, errs, label='Points')
        plt.title('Estimated cost vs. cumulative average error (bits)')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Cumulative average error')
        plt.show()

    def plot_pareto_comparison(self, *frontiers):
        """Plots two cost vs. accuracy Pareto frontiers"""
        for name, frontier in frontiers:
            costs = list(map(lambda p: p[0], frontier))
            errs = list(map(lambda p: p[1], frontier))
            plt.plot(costs, errs, label=name)

        plt.title('Estimated cost vs. cumulative average error (bits)')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Cumulative average error')
        plt.legend()
        plt.show()
