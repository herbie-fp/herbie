from typing import List, Tuple, Optional, Dict
from subprocess import Popen, PIPE
from pathlib import Path

import json
import multiprocessing as mp
import shutil
import json

from .cache import Cache
from .fpcore import FPCore, parse_core
from .shim import shim_error, shim_sample, shim_read
from .util import SampleType, sample_repr, sanitize_name

def baseline() -> FPCore:
    return FPCore(
        core='(FPCore (x) :name "baseline" x)',
        key='synth__baseline',
        name='baseline',
        argc=1,
        override=True
    )

def sample1(
    core: FPCore,
    num_inputs: int,
    platform: str,
    seed: int,
    py_sample: bool
) -> SampleType:
    if core.argc == 0:
        return ([], [])
    elif core.py_sample or py_sample:
        # sample using Python
        inputs = [sample_repr('double', num_inputs) for _ in range(core.argc)]
        gts = [None for _ in range(num_inputs)]
        return (inputs, gts)
    else:
        # sample using Herbie
        return shim_sample(core, num_inputs, platform, seed)

def synthesize1(op: str, argc: int) -> FPCore:
    """Creates a single FPCore for an operation with an arity."""
    op_ = '-' if op == 'neg' else op
    key = sanitize_name(f'synth__{op}')
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

        # mutable data
        self.cache = Cache(self.working_dir.joinpath('cache'))
        self.jsons = []

        # if the working directories do not exist, create them
        self.log('working directory at `' + str(self.working_dir) + '`')
        if not self.driver_dir.exists():
            self.driver_dir.mkdir(parents=True)

        self.log('report directory at `' + str(self.report_dir) + '`')
        if not self.report_dir.exists():
            self.report_dir.mkdir(parents=True)

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

    def herbie_supported(self, cores: List[FPCore]) -> List[FPCore]:
        """Returns the FPCores supported in a platform."""
        with Popen(
            args=['racket', str(self.herbie_path), '--platform', self.name],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            core_strs = ' '.join(map(lambda c: str(c.core), cores))
            print(f'(supported {core_strs}) (exit)', file=server.stdin, flush=True)
            output = server.stdout.read().strip()

        supported_cores = []
        for core, v in zip(cores, output.split(' ')):
            if v == '#t':
                supported_cores.append(core)
            elif v == '#f':
                print(f'WARN: ignoring unsupported {core.core}')
            else:
                raise RuntimeError('Unexpected result:', v)
        return supported_cores
    
    def herbie_read(self, path: str) -> List[FPCore]:
        """Reads a benchmark suite from `path` returning all FPCores found."""
        path = Path(path)
        if not path.exists():
            raise RuntimeError(f'Path does not exist {path}')
        
        cores = shim_read(path, self.name)
        for core in cores:
            self.cache.put_core(core)
        return cores

    def herbie_compile(self, cores: List[FPCore]):
        """Compiles each FPCore in `cores` to the target language.
        This requires the target language to be supported by the
        \"compile\" command in the Racket script."""
        with Popen(
            args=[
                'racket', str(self.herbie_path),
                '--platform', self.name,
                '--seed', str(self.seed)
            ],
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
            args=[
                'racket', str(self.herbie_path),
                '--platform', self.name,
                '--seed', str(self.seed)
            ],
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

    def herbie_desugar(
        self,
        cores: List[FPCore],
        platform: str
    ) -> List[FPCore]:
        """Attempts to desugar an FPCore generated in another platform into the platform
        represented by this `Runner`. If desugaring fails, the FPCore is removed."""
        desugared = []
        with Popen(
            args=[
                'racket', str(self.herbie_path),
                '--platform', self.name,
                '--seed', str(self.seed)
            ],
            stdin=PIPE,
            stdout=PIPE,
            universal_newlines=True) as server:

            # call out to server
            for core in cores:
                print(f'(desugar {core.core} {platform})', file=server.stdin, flush=True)
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

        self.log(f'desugared {len(desugared)} cores')
        return desugared

    def herbie_error(self, cores: List[FPCore]) -> None:
        """Computes the error of each FPCore, overriding the `error` variable of each FPCore."""
        # group FPCore by sample and batch evaluate
        by_key: Dict[str, List[FPCore]] = dict()
        for core in cores:
            if core.key not in by_key:
                by_key[core.key] = []
            by_key[core.key].append(core)

        # parallel configurations
        configs = []
        for key in by_key:
            sample = self.cache.get_sample(key, self.seed)
            if sample is None:
                raise ValueError(f'Cannot find cached sample for {core.key}')
            configs.append((sample, by_key[key], self.name, self.seed))

        if self.threads > 1:
            with mp.Pool(processes=self.threads) as pool:
                errss = pool.starmap(shim_error, configs)
        else:
            errss = []
            for config in configs:
                errss.append(shim_error(*config))

        for key, errs in zip(by_key, errss):
            for core, err in zip(by_key[key], errs):
                core.err = err
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

        # get mapping from name to key
        name_to_key = dict()
        for core in cores:
            if core.name in name_to_key:
                raise RuntimeError(f'Duplicate key {core.name}')
            name_to_key[core.name] = core.key

        # run Herbie
        gen_cores: List[FPCore] = []
        num_improved = 0
        if len(cores) > 0:
            with Popen(
                args=[
                'racket', str(self.herbie_path),
                '--platform', platform,
                '--seed', str(self.seed)
            ],
                stdin=PIPE,
                stdout=PIPE,
                universal_newlines=True) as server:

                # call out to server
                core_strs = ' '.join(map(lambda c: c.core, cores))
                print(f'(improve ({core_strs}) {threads} {self.report_dir}) (exit)', file=server.stdin, flush=True)
                _ = server.stdout.read()

            # if everything went well, Herbie should have created a datafile
            json_path = self.report_dir.joinpath('herbie.json')
            impl_cores = self.load_json(json_path)
            for core in impl_cores:
                core.key = name_to_key[core.name]
                gen_cores.append(core)
                num_improved += 1

        self.log(f'generated {len(gen_cores)} FPCores with Herbie')
        return gen_cores

    def herbie_sample(self, cores: List[FPCore], py_sample: bool = False) -> List[List[List[float]]]:
        """Runs Herbie's sampler for each FPCore in `self.cores`."""
        # check cache first
        samples = []
        num_cached = 0
        for core in cores:
            sample = self.cache.get_sample(core.key, self.seed)
            if sample is None:
                samples.append(None)
            else:
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
                configs.append((core, self.num_inputs, self.name, self.seed, py_sample))

        if self.threads > 1:
            with mp.Pool(processes=self.threads) as pool:
                gen_samples = pool.starmap(sample1, configs)
        else:
            gen_samples = []
            for config in configs:
                gen_samples.append(sample1(*config))
    
        # update `samples`
        for i, (core, sample) in enumerate(zip(cores, samples)):
            if sample is None:
                samples[i] = gen_samples[0]
                gen_samples = gen_samples[1:]
                if samples[i] is not None:
                    self.cache.put_sample(core.key, self.seed, samples[i])
                else:
                    self.log(f'could not sample {core.name}')

        self.log(f'sampled {len(gen_samples)} cores ({num_cached} cached)')
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

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        """Runs all drivers for each compiled FPCore.
        Assumes `compile_drivers()` has already been previous called.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def restore_cores(self) -> Tuple[List[FPCore], List[FPCore]]:
        """Restores platform FPCores from `self.report_dir`.
        Panics if no JSON file is found."""
        path = self.report_dir.joinpath('improve.json')
        with open(path, 'r') as f:
            report = json.load(f)

        input_cores = []
        platform_cores = []
        for cores_json in report['cores']:
            input_cores.append(FPCore.from_json(cores_json['input_core']))
            for core_json in cores_json['platform_cores']:
                core = FPCore.from_json(core_json['platform_core'])
                platform_cores.append(core)
        
        return (input_cores, platform_cores)

    def write_tuning_report(self, cores: List[FPCore], times: List[float]) -> None:
        """Writes tuning data to a JSON file."""
        core_to_time = dict()
        for core, time in zip(cores, times):
            core_to_time[core.name] = [core.cost, time]

        report = {
            'platform': self.name,
            'time_unit': self.time_unit,
            'times': core_to_time
        }

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
            output_cores = by_key.get(input_core.key, [])
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

        report = {
            'platform': self.name,
            'time_unit': self.time_unit,
            'seed': self.seed,
            'cores': core_reports,
            'frontier': frontier
        }

        path = self.report_dir.joinpath('improve.json')
        with open(path, 'w') as _file:
            json.dump(report, _file)

    def write_cross_compile_report(
        self,
        name: str,
        input_cores: List[FPCore],
        cores: List[FPCore],
        supported_cores: List[FPCore],
        desugared_cores: List[FPCore]
    ) -> None:
        cores_by_key: Dict[str, List[FPCore]] = dict()
        for core in cores:
            if core.key in cores_by_key:
                cores_by_key[core.key].append(core)
            else:
                cores_by_key[core.key] = [core]
        
        supported_by_key: Dict[str, List[FPCore]] = dict()
        for core in supported_cores:
            if core.key in supported_by_key:
                supported_by_key[core.key].append(core)
            else:
                supported_by_key[core.key] = [core]

        desugared_by_key: Dict[str, List[FPCore]] = dict()
        for core in desugared_cores:
            if core.key in desugared_by_key:
                desugared_by_key[core.key].append(core)
            else:
                desugared_by_key[core.key] = [core]

        core_reports = []
        for input_core in input_cores:
            cores = cores_by_key.get(input_core.key, [])
            supported_cores = supported_by_key.get(input_core.key, [])
            desugared_cores = desugared_by_key.get(input_core.key, [])
            core_reports.append({
                'input_core': input_core.to_json(),
                'platform_cores': list(map(lambda c: c.to_json(), cores)),
                'supported_cores': list(map(lambda c: c.to_json(), supported_cores)),
                'desugared_cores': list(map(lambda c: c.to_json(), desugared_cores))
            })

        report = {
            'platform': self.name,
            'seed': self.seed,
            'cores': core_reports
        }

        path = self.report_dir.joinpath(f'cross-compile-{name}.json')
        with open(path, 'w') as _file:
            json.dump(report, _file)


    def write_baseline_report(
        self,
        frontier: List[Tuple[float, float]],
        baseline_frontier: List[Tuple[float, float]]
    ) -> None:
        """Writes baseline data to a JSON file."""
        data = {
            'seed': self.seed,
            'frontier': frontier,
            'baseline_frontier': baseline_frontier
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
