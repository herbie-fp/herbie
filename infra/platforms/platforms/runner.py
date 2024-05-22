from typing import List, Tuple, Optional
from subprocess import Popen, PIPE
from pathlib import Path

import json
import matplotlib.pyplot as plt
import multiprocessing as mp
import shutil
import json

from .cache import Cache, sanitize_name
from .fpcore import FPCore, parse_core
from .util import sample_repr, py_to_racket, racket_to_py

def baseline() -> FPCore:
    return FPCore(core='(FPCore () :name "baseline" 0)', key='synth:baseline', name='baseline', argc=0, override=True)
    
def sample_to_pcontext(sample):
    points, gts = sample
    input_strs = []
    for i, gt in enumerate(gts):
        pt = []
        for j, _ in enumerate(points):
            pt.append(points[j][i])
        pt_str = ' '.join(map(lambda v: py_to_racket(v), pt))
        input_strs.append(f'(({pt_str}) {py_to_racket(gt)})')
    input_str = ' '.join(input_strs)
    return f'({input_str})'

def error1(config: Tuple[FPCore, List, str, str]):
    core, sample, herbie_path, platform = config
    with Popen(
            args=['racket', str(herbie_path), "--platform", platform],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:
        
        pcontext = sample_to_pcontext(sample)
        print(f'(error {core.core} {pcontext})', file=server.stdin, flush=True)
        output = server.stdout.readline()

        print('(exit)', file=server.stdin, flush=True)
        _ = server.stdout.read()

    return float(output)

def sample1(config: Tuple[FPCore, int, str, str]) -> Tuple[List[List[float]], List[float]]:
    core, num_inputs, herbie_path, platform, py_sample = config
    if core.argc == 0:
        return ([], [])
    elif core.py_sample or py_sample:
        # sample using Python
        inputs = [sample_repr('double', num_inputs) for _ in range(core.argc)]
        gts = [None for _ in range(num_inputs)]
        return (inputs, gts)
    else:
        # sample using Herbie
        with Popen(
                args=['racket', str(herbie_path), "--platform", platform],
                stdin=PIPE,
                stdout=PIPE,
                universal_newlines=True) as server:
            
            print(f'(sample {num_inputs} {core.core})', file=server.stdin, flush=True)
            output = server.stdout.readline().strip()

            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.read()
            if output == '#f':
                return None

            gts = []
            points = [[] for _ in range(core.argc)]
            for input in output.split('|'):
                parts = input.split(',')
                if len(parts) != 2:
                    raise RuntimeError(f'malformed point {input}')
                
                for i, val in enumerate(parts[0].split(' ')):
                    points[i].append(racket_to_py(val.strip()))
                gts.append(racket_to_py(parts[1].strip()))

            return (points, gts)

def synthesize1(op: str, argc: int) -> FPCore:
    """Creates a single FPCore for an operation with an arity."""
    op_ = '-' if op == 'neg' else op
    key = sanitize_name(f'synth:{op}')
    vars = [f'x{i}' for i in range(argc)]
    arg_str = ' '.join(vars)
    app_str = '(' + ' '.join([op_] + vars) + ')'
    core = f'(FPCore ({arg_str}) :name "{op}" {app_str})'
    py_sample = op == 'lgamma' or op == 'tgamma'  # Rival struggles with these
    return FPCore(core, key=key, name=op, argc=argc, py_sample=py_sample)

def json_test_outputs(test: dict) -> List[Tuple[str, float, float]]:
    """Parses a single test entry in a Herbie JSON returning a list
    of tuples representing expr, cost, error"""
    output = test['output']
    cost_accuracy = test['cost-accuracy']
    
    outputs = [(output, cost_accuracy[1][0], cost_accuracy[1][1])]
    for entry in cost_accuracy[2]:
        cost = entry[0]
        err = entry[1]
        expr = entry[2]
        outputs.append((expr, cost, err))
    
    return outputs


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
        seed: int = 1,
        unary_ops: List[str] = [],
        binary_ops: List[str] = [],
        ternary_ops: List[str] = [],
        nary_ops: List[Tuple[int, str]] = [],
        key: Optional[str] = None
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
        self.seed = seed

        key = 'default' if key is None else key
        self.driver_dir = self.working_dir.joinpath('drivers', key, self.name)
        self.report_dir = self.working_dir.joinpath('output', key, self.name)

        # add empty list of jsons to the class instance
        self.jsons = []

        # mutable data
        self.cache = Cache(str(self.working_dir.joinpath('cache')))
        # if the working directories do not exist, create them
        self.log('working directory at `' + str(self.working_dir) + '`')
        if not self.driver_dir.exists():
            self.driver_dir.mkdir(parents=True)

        self.log('report directory at `' + str(self.report_dir) + '`')
        if not self.report_dir.exists():
            self.report_dir.mkdir(parents=True)
    
        # restore cache
        self.cache.restore()
        self.log(f'restored {self.cache.num_cores()} input cores from cache')
        self.log(f'restored {self.cache.num_platform_cores()} platform cores from cache')
        self.log(f'using seed {self.seed}')
    
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
    
    def load_json(self, path: str) -> List[FPCore]:
        """Reads a set of FPCores from Herbie JSON report."""
        # read JSON file
        with open(path, 'r') as f:
            report = json.load(f)

        # extract relevant fields from report JSON
        json_cores = []
        for test in report['tests']:
            vars = test['vars']
            name = test['name']
            prec = test['prec']
            pre = test['pre']
            spec = test['spec']
            status = test['status']

            if status != 'error' and status != 'timeout':
                for expr, cost, err in json_test_outputs(test):
                    json_cores.append((vars, name, prec, pre, spec, expr, cost, err, test))

        # construct FPCores (need to resugar expressions)
        cores = []
        with Popen(
            args=['racket', str(self.herbie_path)],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            for vars, name, prec, pre, spec, expr, cost, err, test in json_cores:
                print(f'(resugar ({" ".join(vars)}) "{name}" {prec} {pre} {spec} {expr})', file=server.stdin, flush=True)
                core_str = server.stdout.readline()

                core = parse_core(core_str)
                core.cost = cost
                core.err = err
                core.json = test

                cores.append(core)

            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()

        return cores

    
    def herbie_read(self, path: str) -> List[FPCore]:
        """Reads a benchmark suite from `path` returning all FPCores found."""
        path = Path(path)
        if not path.exists():
            raise RuntimeError(f'Path does not exist {path}')
        
        if path.is_file():
            with Popen(
                args=['racket', str(self.herbie_path), "--platform", self.name],
                stdin=PIPE,
                stdout=PIPE,
                universal_newlines=True) as server:

                # call out to server
                print(f'(read \"{path}\") (exit)', file=server.stdin, flush=True)
                output = server.stdout.read()

            cores = []
            for i, line in enumerate(output.split('\n')):
                if len(line) > 0:
                    core = parse_core(line.strip())
                    core.key = sanitize_name(f'file:{str(path)}:{i}')
                    cores.append(core)
            return cores
        else:
            cores = []
            for subdir in path.iterdir():
                cores += self.herbie_read(str(subdir))
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

    def herbie_desugar(self, input_cores: List[FPCore], cores: List[FPCore]) -> List[FPCore]:
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
                output = server.stdout.readline().strip()
                if output == '#f':
                    print(f'WARN: failed to desugar {core.name}')
                else:
                    core2 = parse_core(output)
                    core2.descr = core.descr
                    core2.key = core.key
                    desugared.append(core2)

            # terminate the server
            print('(exit)', file=server.stdin, flush=True)
            _ = server.stdout.readline()

        # we need to check if we dropped any cores for a particular input core
        # if we did, the "best" output core is just the input core
        cores_by_key = dict()
        for core in desugared:
            if core.name is None:
                raise RuntimeError('FPCore does not have name', core)
            if core.key in cores_by_key:
                cores_by_key[core.key].append(core)
            else:
                cores_by_key[core.key] = [core]

        for input in input_cores:
            if input.key not in cores_by_key:
                print(f'WARN: no output core for {input.key}, restoring input')
                desugared.append(input)

        self.log(f'desugared {len(desugared)} cores')
        return desugared

    def herbie_error(self, cores: List[FPCore]) -> None:
        """Computes the error of each FPCore, overriding the `error` variable of each FPCore."""
        # assuming all FPCores have names at this point
        configs = []
        for core in cores:
            _, sample = self.cache.get_core(core.key)
            configs.append((core, sample, self.herbie_path, self.name))

        with mp.Pool(processes=self.threads) as pool:
            errors = pool.map(error1, configs)

        for core, error in zip(cores, errors):
            core.err = error

        self.log(f'recomputed errors of {len(cores)} cores')

    def herbie_improve(
        self,
        cores: List[FPCore],
        threads: int = 1,
        platform: Optional[str] = None
    ):
        """Runs Herbie improvement on benchmarks under `path` appending
        all resulting FPCores to `self.cores`."""
        if platform is None:
            platform = self.name

        # TODO: embed key in the FPCore so it can be recovered
        uncached = []
        num_cached = 0
        key_dict = dict()
        gen_dict = dict()
        for core in cores:
            if core.name in gen_dict:
                raise RuntimeError(f'Duplicate key {core.name}')
            key_dict[core.name] = core.key

            maybe_cached = self.cache.get_platform_core(platform, core.key)
            if maybe_cached is None:
                uncached.append(core)
                gen_dict[core.key] = []
            else:
                num_cached += len(maybe_cached)
                gen_dict[core.key] = maybe_cached  

        num_improved = 0
        if len(uncached) > 0:
            with Popen(
                args=['racket', str(self.herbie_path), "--platform", platform],
                stdin=PIPE,
                stdout=PIPE,
                universal_newlines=True) as server:

                # call out to server
                core_strs = ' '.join(map(lambda c: c.core, uncached))
                print(f'(improve ({core_strs}) {threads} {self.report_dir}) (exit)', file=server.stdin, flush=True)
                _ = server.stdout.read()

            # if everything went well, Herbie should have created a datafile
            json_path = self.report_dir.joinpath('herbie.json')
            impl_cores = self.load_json(json_path)
            for core in impl_cores:
                core.key = key_dict[core.name]
                gen_dict[core.key].append(core)
                num_improved += 1

        gen_cores = []
        for key in gen_dict:
            cores = gen_dict[key]
            self.cache.write_platform_core(platform, key, cores)
            gen_cores += cores

        self.log(f'generated {num_improved} FPCores with Herbie ({num_cached} cached)')
        return gen_cores


    def herbie_pareto(self, input_cores: List[FPCore], cores: List[FPCore]) -> List[Tuple[float, float]]:
        """Runs Herbie's pareto frontier algorithm."""
        # group FPCore by key
        cores_by_group = dict()
        for core in cores:
            if core.key in cores_by_group:
                cores_by_group[core.key].append(core)
            else:
                cores_by_group[core.key] = [core]

        with Popen(
            args=['racket', str(self.herbie_path), "--platform", self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            frontiers = []
            for key in cores_by_group:
                group = cores_by_group[key]
                frontier = ' '.join(list(map(lambda c: f'({c.cost} {c.err})', group)))
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
            cost, err = float(datum[0]), float(datum[1])
            frontier.append((cost, err))

        self.log(f'computed Pareto frontier')
        return frontier

    def herbie_sample(self, cores: List[FPCore], py_sample: bool = False) -> List[List[List[float]]]:
        """Runs Herbie's sampler for each FPCore in `self.cores`."""
        # check cache first
        samples = []
        num_cached = 0
        for core in cores:
            maybe_cached = self.cache.get_core(core.key)
            if maybe_cached is None:
                samples.append(None)
            else:
                _, sample = maybe_cached
                input_points, _ = sample
                if len(input_points) == 0:
                    # no inputs
                    samples.append(None)
                elif len(input_points[0]) >= self.num_inputs:
                    # cached copy has enough points
                    samples.append(sample[:self.num_inputs])
                    num_cached += 1
                else:
                    # cached copy does not have enough points
                    samples.append(None)
                    self.cache.clear_core(core.key)

        # run sampling for un-cached ones
        configs = []
        for sample, core in zip(samples, cores):
            if sample is None:
                configs.append((core, self.num_inputs, self.herbie_path, self.name, py_sample))

        with mp.Pool(processes=self.threads) as pool:
            gen_samples = pool.map(sample1, configs)
        self.log(f'sampled {len(gen_samples)} cores ({num_cached} cached)')
    
        # update `samples`
        for i, (core, sample) in enumerate(zip(cores, samples)):
            if sample is None:
                samples[i] = gen_samples[0]
                gen_samples = gen_samples[1:]
                if samples[i] is not None:
                    self.cache.write_core(core, samples[i])
                else:
                    self.log(f'could not sample {core.name}')

        return samples

    def make_driver_dirs(self, cores: List[FPCore]) -> List[str]:
        """Creates the subdirectories for each driver: one subdirectory
        per FPCore in `cores`. Returns the list of subdirectories.
        Likely a utility function for `make_drivers()`."""
        # Nest the drivers properly
        driver_dirs = []
        for i, _ in enumerate(cores):
            subdir = self.driver_dir.joinpath(Path(str(i)))
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

    def restore_cores(self) -> List[FPCore]:
        """Restores platform FPCores from `self.report_dir`.
        Panics if no JSON file is found."""
        path = self.report_dir.joinpath('improve.json')
        with open(path, 'r') as f:
            report = json.load(f)

        cores = []
        for cores_json in report['cores']:
            for core_json in cores_json['platform_cores']:
                core = FPCore.from_json(core_json['platform_core'])
                cores.append(core)
        
        return cores

    def write_tuning_report(self, cores: List[FPCore], times: List[float]) -> None:
        """Writes tuning data to a JSON file."""
        report = dict()
        for core, time in zip(cores, times):
            report[core.name] = [core.cost, time]

        path = self.report_dir.joinpath('tuning.json')
        with open(path, 'w') as f:
            json.dump(report, f)

    def write_improve_report(
        self,
        input_cores: List[FPCore],
        platform_cores: List[FPCore],
        driver_dirs: List[str],
        times: List[float],
        frontier: List[Tuple[float, float]]
    ) -> None:
        """Writes improve data to a JSON file."""
        # group platform cores by input [key]
        by_key = dict()
        for core, dir, time in zip(platform_cores, driver_dirs, times):
            if core.key in by_key:
                by_key[core.key].append((core, dir, time))
            else:
                by_key[core.key] = [(core, dir, time)]

        # generate report fragments
        core_reports = []
        for input_core in input_cores:
            output_cores = by_key[input_core.key]
            platform_core_reports = []
            for platform_core, dir, time in output_cores:
                platform_core_reports.append({
                    'platform_core': platform_core.to_json(),
                    'dir': str(dir),
                    'time': time
                })
            
            core_reports.append({
                'input_core': input_core.to_json(),
                'platform_cores': platform_core_reports
            })

        report = { 'cores': core_reports, 'frontier': frontier }

        path = self.report_dir.joinpath('improve.json')
        with open(path, 'w') as _file:
            json.dump(report, _file)

    def write_cross_compile_report(
        self,
        name: str,
        input_cores: List[FPCore],
        foreign_cores: List[FPCore],
        platform_frontier: List[Tuple[float, float]],
        foreign_frontier: List[Tuple[float, float]]
    ) -> None:
        # group cores by input [key]
        by_key = dict()
        for core in foreign_cores:
            if core.key in by_key:
                by_key[core.key].append(core)
            else:
                by_key[core.key] = [core]

        core_reports = []
        for input_core in input_cores:
            output_cores = by_key[input_core.key]
            core_reports.append({
                'input_core': input_core.to_json(),
                'output_cores': list(map(lambda c: c.to_json(), output_cores))
            })

        report = {
            'cores': core_reports,
            'frontier1': platform_frontier,
            'frontier2': foreign_frontier
        }

        path = self.report_dir.joinpath(f'cross-compile-{name}.json')
        with open(path, 'w') as _file:
            json.dump(report, _file)

        pass

    def write_baseline_report(
        self,
        frontier: List[Tuple[float, float]],
        baseline_frontier: List[Tuple[float, float]]
    ) -> None:
        """Writes baseline data to a JSON file."""
        data = {
            "frontier": frontier,
            "baseline_frontier": baseline_frontier
        }
        path = self.report_dir.joinpath("baseline_report.json")
        with open(path, "w") as _file:
            json.dump(data, _file)

    def write_samples(
        self,
        input_cores: List[FPCore],
        platform_cores: List[FPCore],
        samples: List[List[List[float]]],
    ) -> None:
        by_key = dict()
        for core, sample in zip(platform_cores, samples):
            if core.key in by_key:
                by_key[core.key].append((core, sample))
            else:
                by_key[core.key] = [(core, sample)]
        data = [
            {
                'input_core': input_core.to_json(),
                'platform_cores': [{
                    'platform_core': platform_core.to_json(),
                    'sample': sample
                } for platform_core, sample in by_key[input_core.key]]
            }
            for input_core in input_cores
        ]
        path = self.report_dir.joinpath('sample.json')
        with open(path, 'w') as _file:
            json.dump(data, _file)

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
