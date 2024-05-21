""" Cross-platform comparison """

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
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
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
        key=key
    )

    cores1 = runner1.restore_cores()

    # load FPCores form platform B
    if platform2 == 'baseline':
        baseline_dir = os.path.join(output_dir, 'baseline', key)
        json_path = os.path.join(baseline_dir, 'baseline.json')
        print(json_path)
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
    else:
        runner2 = make_runner(
            platform=platform2,
            working_dir=output_dir,
            herbie_path=herbie_path,
            num_inputs=num_points,
            num_runs=num_runs,
            threads=threads,
            key=key
        )
        cores2 = runner2.restore_cores()

    # extract input fpcores based on `cores1`
    input_cores = []
    all_keys = set(map(lambda c: c.key, cores1))
    for key in all_keys:
        cached = runner1.cache.get_core(key)
        if cached is None:
            raise ValueError(f'no input FPCore cached with {key}')
        core, _ = cached
        input_cores.append(core)

    runner1.herbie_cost(cores=input_cores)
    runner1.herbie_error(cores=input_cores)

    # pull `cores2` back into `platform1`
    cores2 = list(filter(lambda c: c.key in all_keys, cores2)) # filter only relevant fpcores
    cores2 = runner1.herbie_desugar(input_cores=input_cores, cores=cores2)
    runner1.herbie_cost(cores=cores2)
    runner1.herbie_error(cores=cores2)

    # compute frontiers
    frontier1 = runner1.herbie_pareto(input_cores=input_cores, cores=cores1)
    frontier2 = runner1.herbie_pareto(input_cores=input_cores, cores=cores2)

    # write report
    runner1.write_cross_compile_report(
        name=platform2,
        input_cores=input_cores,
        foreign_cores=cores2,
        platform_frontier=frontier1,
        foreign_frontier=frontier2
    )


if __name__ == "__main__":
    main()
