""" Cross-platform comparison """

import argparse
import os

from typing import List

from platforms.fpcore import FPCore
from platforms.runner import Runner
from platforms.runners import make_runner

# paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'server.rkt')
curr_dir = os.getcwd()

# defaults
default_num_threads = 1
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

def main():
    parser = argparse.ArgumentParser(description='Herbie cost tuner')
    parser.add_argument('--threads', help='number of threads for compilation [1 by default]', type=int)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    parser.add_argument('--seed', help='random seed to use for Herbie', type=int)
    parser.add_argument('platform1', help='platform to evaluate in', type=str)
    parser.add_argument('platform2', help='platform to compare against', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)

    # keep all non-None entries
    args = dict()
    for k, v in vars(parser.parse_args()).items():
        if v is not None:
            args[k] = v

    # extract command line arguments
    threads = args.get('threads', default_num_threads)
    num_points = default_num_points
    num_runs = default_num_runs
    key = args.get('key', None)
    seed = args.get('seed', default_seed)

    platform1 = args['platform1']
    platform2 = args['platform2']
    output_dir = os.path.join(curr_dir, args['output_dir'])
    
    # load FPCores from platform A
    runner1 = make_runner(
        platform=platform1,
        working_dir=output_dir,
        herbie_path=herbie_path,
        num_inputs=num_points,
        num_runs=num_runs,
        threads=threads,
        key=key,
        seed=seed
    )

    input_cores, cores1 = runner1.restore_cores()
    all_keys = set(map(lambda c: c.key, input_cores))

    # load FPCores form platform B
    if platform2 == 'baseline':
        baseline_dir = os.path.join(output_dir, 'baseline', key)
        json_path = os.path.join(baseline_dir, 'baseline.json')
        cores2 = runner1.load_json(json_path)

        # cores are "keyless", so we need to identify their in-cache keys (somehow)
        key_dict = dict()
        for core in cores1:
            key_dict[core.name] = core.key
    
        for core in cores2:
            if core.name in key_dict:
                core.key = key_dict[core.name]
            else:
                print(f'WARN: no key for {core.name} from baseline')

        runner2 = make_runner(
            platform='c',
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key,
            seed=seed
        )
    else:
        runner2 = make_runner(
            platform=platform2,
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key,
            seed=seed
        )

        _, cores2 = runner2.restore_cores()

    # filter only relevant fpcores
    cores2 = list(filter(lambda c: c.key in all_keys, cores2))

    # run Herbie on all supported cores
    supported_cores = runner1.herbie_supported(cores=cores2)
    run_cores(runner1, supported_cores)

    # run Herbie on desugared cores
    # pull `cores2` back into `platform1`
    desugared_cores = runner2.herbie_desugar(cores=cores2, platform=platform1)
    run_cores(runner1, desugared_cores)

    # analyze all output FPCores
    analyze_cores(runner1, supported_cores + desugared_cores)

    # write report
    runner1.write_cross_compile_report(
        name=platform2,
        input_cores=input_cores,
        cores=cores1,
        supported_cores=supported_cores,
        desugared_cores=desugared_cores,
    )


if __name__ == "__main__":
    main()
