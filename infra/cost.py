from typing import Optional, Tuple, List
from pathlib import Path
import argparse
import os

from platforms.fpcore import FPCore
from platforms.runner import Runner

from platforms.arith import ArithRunner
from platforms.avx import AVXRunner
from platforms.c import CRunner
from platforms.math import MathRunner
from platforms.mkl import MKLRunner
from platforms.python import PythonRunner

# paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'cost.rkt')
curr_dir = os.getcwd()

# defaults
default_num_threads = 1
default_herbie_threads = 1
default_num_points = 10_000
default_num_runs = 25

# TODO: what should we do with unsamplable FPCores
def prune_unsamplable(samples: List[List[List[float]]], cores: List[FPCore]):
    samples2: List[List[List[float]]] = []
    cores2: List[FPCore] = []
    for sample, core in zip(samples, cores):
        if sample is not None:
            samples2.append(sample)
            cores2.append(core)
    return samples2, cores2

# Sanity check that samples match the FPCores
def check_samples(samples: List[List[List[float]]], cores: List[FPCore]):
    for sample, core in zip(samples, cores):
        input_points, _ = sample
        if len(input_points) != core.argc:
            raise RuntimeError(f'Sample does not have expected arity: {len(input_points)} != {core.argc} for {core}')
        if core.key is None:
            raise RuntimeError(f'Core does not have sample: {core}')

def run(
    runner: Runner,
    tune: bool,
    baseline: Optional[str],
    py_sample: bool,
    herbie_params: Optional[Tuple[str, int]] = None
):
    # generate and analyze phase
    if herbie_params is not None:
        # Using FPCores from directory, generate implementations using Herbie
        bench_dir, threads = herbie_params
        bench_dir = os.path.join(curr_dir, bench_dir)
        # read and sample input cores
        input_cores = runner.herbie_read(path=bench_dir)
        samples = runner.herbie_sample(cores=input_cores, py_sample=py_sample)
        samples, input_cores = prune_unsamplable(samples, input_cores)
        check_samples(samples, input_cores) # sanity check!

        # analyze input cores
        runner.herbie_cost(cores=input_cores)
        runner.herbie_error(cores=input_cores)

        # run Herbie improve and get associated sampled points
        cores = runner.herbie_improve(cores=input_cores, threads=threads)
        samples = runner.herbie_sample(cores=cores, py_sample=py_sample)
        check_samples(samples, cores) # sanity check!

        # analyze output cores
        runner.herbie_cost(cores=cores)
        runner.herbie_error(cores=cores)
    else:
        # Synthesize implementations
        cores = runner.synthesize()
        samples = runner.herbie_sample(cores=cores, py_sample=py_sample)
        input_cores = cores

    # plot phase
    if tune:
        # tuning cost model       
        runner.herbie_compile(cores=cores)

        driver_dirs = runner.make_driver_dirs(cores=cores)
        runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
        runner.compile_drivers(driver_dirs=driver_dirs)

        times = runner.run_drivers(driver_dirs=driver_dirs)
        runner.print_times(cores, times)
    elif baseline is not None:
        # run and compare against baseline
        bench_dir, threads = herbie_params
        frontier = runner.herbie_pareto(input_cores=input_cores, cores=cores)

        baseline_cores = runner.herbie_improve(cores=input_cores, threads=threads, platform=baseline)
        baseline_cores = runner.herbie_desugar(input_cores=input_cores, cores=baseline_cores)

        runner.herbie_cost(cores=baseline_cores) # recompute the cost
        runner.herbie_error(cores=baseline_cores) # recompute the error

        baseline_frontier = runner.herbie_pareto(input_cores=input_cores, cores=baseline_cores)
        runner.plot_pareto_comparison((runner.name, frontier), (f'{baseline} (baseline)', baseline_frontier))
    else:
        # run and plot results
        frontier = runner.herbie_pareto(input_cores=input_cores, cores=cores)
        runner.herbie_compile(cores=cores)

        driver_dirs = runner.make_driver_dirs(cores=cores)
        runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
        runner.compile_drivers(driver_dirs=driver_dirs)

        times = runner.run_drivers(driver_dirs=driver_dirs)
        runner.plot_times(cores, times)
        runner.plot_pareto(frontier)
        

def main():
    parser = argparse.ArgumentParser(description='Herbie cost tuner and evaluator')
    parser.add_argument('--threads', help='number of threads for compilation [1 by default]', type=int)
    parser.add_argument('--herbie-threads', help='number of threads for Herbie [1 by default]', type=int)
    parser.add_argument('--herbie-input', help='directory or FPCore file that Herbie will run on. Required when not in `--tune` mode or `--restore mode`', type=str)
    parser.add_argument('--num-points', help='number of input points to evalaute on [10_000 by default]', type=int)
    parser.add_argument('--num-runs', help='number of times to run drivers to obtain an average [100 by default]', type=int)
    parser.add_argument('--tune', help='cost tuning mode [OFF by default].', action='store_const', const=True, default=False)
    parser.add_argument('--baseline', help='platform to perform a baseline comparison', type=str)
    parser.add_argument('--py-sample', help='uses a Python based sampling method. Useful for debugging', action='store_const', const=True, default=False)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    parser.add_argument('lang', help='output language to use', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)

    # keep all non-None entries
    namespace = parser.parse_args()
    args = dict()
    for k, v in vars(namespace).items():
        if v is not None:
            args[k] = v
    # extract command line arguments
    threads = args.get('threads', default_num_threads)
    herbie_threads = args.get('herbie_threads', default_herbie_threads)
    bench_dir = args.get('herbie_input', None)
    num_points = args.get('num_points', default_num_points)
    num_runs = args.get('num_runs', default_num_runs)
    tune = args.get('tune')
    baseline = args.get('baseline', None)
    py_sample = args.get('py_sample')
    key = args.get('key', None)
    lang = args['lang']
    output_dir = os.path.join(curr_dir, args['output_dir'])

    if lang == 'arith':
        runner = ArithRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'c':
        runner = CRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'math':
        runner = MathRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'mkl':
        runner = MKLRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'python':
        runner = PythonRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
    elif lang == 'avx':
        runner = AVXRunner(
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads
        )
    else:
        raise ValueError(f'Unsupported output language: {lang}')

    if tune:
        # FPCores are synthetic
        herbie_params = None
    else:
        # need to actually run Herbie
        if bench_dir is None:
            raise ValueError('Need an directory to run Herbie on, try `--herbie_input`')
        herbie_params = (bench_dir, herbie_threads)

    if tune and baseline:
        print('WARN: baseline mode will be ignored when tuning')

    run(
        runner=runner,
        tune=tune,
        baseline=baseline,
        herbie_params=herbie_params,
        py_sample=py_sample
    )
    

if __name__ == "__main__":
    main()

