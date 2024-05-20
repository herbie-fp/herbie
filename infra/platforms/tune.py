""" Runs cost tuning for a given platform """

import argparse
import os

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

def main():
    parser = argparse.ArgumentParser(description='Herbie cost tuner')
    parser.add_argument('--threads', help='number of threads for compilation [1 by default]', type=int)
    parser.add_argument('--num-points', help='number of input points to evalaute on [10_000 by default]', type=int)
    parser.add_argument('--num-runs', help='number of times to run drivers to obtain an average [100 by default]', type=int)
    parser.add_argument('--py-sample', help='uses a Python based sampling method. Useful for debugging', action='store_const', const=True, default=False)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    parser.add_argument('platform', help='platform to use', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)

    # keep all non-None entries
    args = dict()
    for k, v in vars(parser.parse_args()).items():
        if v is not None:
            args[k] = v

    # extract command line arguments
    threads = args.get('threads', default_num_threads)
    num_points = args.get('num_points', default_num_points)
    num_runs = args.get('num_runs', default_num_runs)
    py_sample = args.get('py_sample')
    key = args.get('key', None)
    platform = args['platform']
    output_dir = os.path.join(curr_dir, args['output_dir'])
    
    # construct runner
    runner = make_runner(
        platform=platform,
        working_dir=output_dir,
        herbie_path=herbie_path,
        num_inputs=num_points,
        num_runs=num_runs,
        threads=threads,
        key=key
    )

    # synthesise implementations
    cores = runner.synthesize()
    samples = runner.herbie_sample(cores=cores, py_sample=py_sample)
    runner.herbie_compile(cores=cores)
    runner.herbie_cost(cores=cores)

    # make drivers
    driver_dirs = runner.make_driver_dirs(cores=cores)
    runner.make_drivers(cores=cores, driver_dirs=driver_dirs, samples=samples)
    runner.compile_drivers(driver_dirs=driver_dirs)

    # run drivers
    times = runner.run_drivers(driver_dirs=driver_dirs)
    runner.write_tuning_report(cores, times)
    runner.print_times(cores, times)


if __name__ == "__main__":
    main()
