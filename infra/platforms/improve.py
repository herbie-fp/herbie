""" Runs platform-specific Herbie improvement """

import argparse
import os

from typing import List, Tuple

from platforms.c import get_cflags, set_cflags
from platforms.fpcore import FPCore
from platforms.runner import Runner
from platforms.runners import make_runner
from platforms.shim import shim_read, shim_error2

# paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'server.rkt')
curr_dir = os.getcwd()

# defaults
default_num_threads = 1
default_herbie_threads = 1
default_num_points = 10_000
default_num_runs = 10
default_seed = 1

# Sanity check that samples match the FPCores
def check_samples(samples: List[List[List[float]]], cores: List[FPCore]):
    for sample, core in zip(samples, cores):
        input_points, _ = sample
        if len(input_points) != core.argc:
            raise RuntimeError(f'Sample does not have expected arity: {len(input_points)} != {core.argc} for {core}')
        if core.key is None:
            raise RuntimeError(f'Core does not have sample: {core}')

# TODO: what should we do with unsamplable FPCores
def prune_unsamplable(samples: List[List[List[float]]], cores: List[FPCore]):
    samples2: List[List[List[float]]] = []
    cores2: List[FPCore] = []
    for sample, core in zip(samples, cores):
        if sample is not None:
            samples2.append(sample)
            cores2.append(core)
    return samples2, cores2

def analyze_cores(runner: Runner, cores: List[FPCore]):
    # compute estimated cost and error
    runner.herbie_cost(cores=cores)
    runner.herbie_error(cores=cores)

def run_cores(runner: Runner, cores: List[FPCore], py_sample: bool = False) -> List[str]:
    # get sample
    samples = runner.herbie_sample(cores=cores, py_sample=py_sample)
    check_samples(samples, cores) # sanity check!

    # compile FPCore
    runner.herbie_compile(cores=cores)

    # create drivers and compile
    driver_dirs = runner.make_driver_dirs(cores=cores)
    runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
    runner.compile_drivers(driver_dirs=driver_dirs)
    
    # run drivers to get timing
    times = runner.run_drivers(cores=cores, driver_dirs=driver_dirs)
    for core, time in zip(cores, times):
        core.time = time

    return driver_dirs

#################################################
# Clang comparison eval

default_flags = ['-std=gnu11', '-ffp-contract=off']
opt_flags = ['-O0', '-O1', '-O2', '-O3', '-Os', '-Oz']
fp_flags = [None, '-ffast-math']

def clang_eval(runner: Runner, cores: List[FPCore]):
    old_cflags = get_cflags()

    # get sample
    samples = runner.herbie_sample(cores=cores)
    check_samples(samples, cores) # sanity check!

    configs: List[Tuple[List[str], List[float]]] = []
    for opt_flag in opt_flags:
        for fp_flag in fp_flags:
            flags = [opt_flag] if fp_flag is None else [opt_flag, fp_flag]
            set_cflags(flags + default_flags)
            
            runner.log('running clang eval [time]', flags)
            driver_dirs = runner.make_driver_dirs(cores=cores)
            runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
            runner.compile_drivers(driver_dirs=driver_dirs)
            times = runner.run_drivers(cores=cores, driver_dirs=driver_dirs)

            runner.log('running clang eval [error]', flags)
            driver_dirs = runner.make_driver_dirs(cores=cores)
            runner.make_drivers2(cores=cores, driver_dirs=driver_dirs, samples=samples)
            runner.compile_drivers(driver_dirs=driver_dirs)
            inexactss = runner.run_drivers2(cores=cores, driver_dirs=driver_dirs)
            precs = list(map(lambda c: c.prec, cores))
            errors = shim_error2(samples, inexactss, precs)

            configs.append((flags, times, errors))

    set_cflags(old_cflags)
    return configs


def main():
    parser = argparse.ArgumentParser(description='Herbie cost tuner')
    parser.add_argument('--threads', help='number of threads for compilation [1 by default]', type=int)
    parser.add_argument('--herbie-threads', help='number of threads for Herbie [1 by default]', type=int)
    parser.add_argument('--num-points', help='number of input points to evalaute on [10_000 by default]', type=int)
    parser.add_argument('--num-runs', help='number of times to run drivers to obtain an average [100 by default]', type=int)
    parser.add_argument('--py-sample', help='uses a Python based sampling method. Useful for debugging', action='store_const', const=True, default=False)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    parser.add_argument('--seed', help='random seed to use for Herbie', type=int)
    parser.add_argument('platform', help='platform to use', type=str)
    parser.add_argument('bench_path', help='directory or FPCore for Herbie to run on', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)

    # keep all non-None entries
    args = dict()
    for k, v in vars(parser.parse_args()).items():
        if v is not None:
            args[k] = v

    # extract command line arguments
    threads = args.get('threads', default_num_threads)
    herbie_threads = args.get('herbie_threads', default_herbie_threads)
    num_points = args.get('num_points', default_num_points)
    num_runs = args.get('num_runs', default_num_runs)
    py_sample = args.get('py_sample')
    key = args.get('key', None)
    platform = args['platform']
    bench_path = os.path.join(curr_dir, args['bench_path'])
    output_dir = os.path.join(curr_dir, args['output_dir'])
    seed = args.get('seed', default_seed)
    
    # construct runner
    runner = make_runner(
        platform=platform,
        working_dir=output_dir,
        herbie_path=herbie_path,
        num_inputs=num_points,
        num_runs=num_runs,
        threads=threads,
        key=key,
        seed=seed
    )

    # read input cores
    all_input_cores = shim_read(path=bench_path)

    # prune and sample input cores
    input_cores = runner.herbie_supported(cores=all_input_cores)
    samples = runner.herbie_sample(cores=input_cores, py_sample=py_sample)
    samples, input_cores = prune_unsamplable(samples, input_cores)
    check_samples(samples, input_cores) # sanity check!

    # optionally run clang comparison
    if platform == 'c':
        runner.herbie_compile(cores=input_cores)
        clang_results = clang_eval(runner, input_cores)
    else:
        clang_results = None

    # run Herbie for output cores
    cores = runner.herbie_improve(cores=input_cores, threads=herbie_threads)

    # analyze input and output cores
    analyze_cores(runner, input_cores + cores)
    run_cores(runner, input_cores, py_sample)
    driver_dirs = run_cores(runner, cores, py_sample)

    # publish results
    runner.write_improve_report(all_input_cores, cores, driver_dirs, clang_results)


if __name__ == "__main__":
    main()
