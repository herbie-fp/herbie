import argparse
import os
import multiprocessing as mp

from subprocess import Popen, PIPE, STDOUT
import subprocess

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
baseline_path = os.path.join(script_dir, 'platforms', 'baseline.py')
run_path = os.path.join(script_dir, 'platforms', 'run.py')
plot_path = os.path.join(script_dir, 'platforms', 'plot.py')
curr_dir = os.getcwd()

# Defaults
default_key = 'default'
default_num_parallel = 1
default_num_threads = 1
default_start_seed = 1

def run_baseline(
    key: str,
    bench_path: str,
    output_dir: str,
    num_parallel: int,
    num_threads: int,
    num_seeds: int,
    start_seed: int
) -> None:
    print(f'running baseline eval')
    subprocess.run([
        'python3', baseline_path,
        '--key', key,
        '--parallel', str(num_parallel),
        '--threads', str(num_threads),
        '--start-seed', str(start_seed),
        bench_path,
        output_dir,
        str(num_seeds)
    ])


def run_seed(
    bench_path: str,
    output_dir: str,
    key: str,
    num_threads: int,
    seed: int,
    hide_output: bool
) -> None:
    print(f'running per-seed evaluation (seed={seed})')
    cmd = [
        'python3', run_path,
        bench_path,
        output_dir,
        key,
        str(num_threads),
        str(num_threads),
        str(seed)
    ]

    if hide_output:
        p = Popen(cmd, stdout=PIPE, stderr=STDOUT)
        stdout, _ = p.communicate()
        print(stdout.decode('utf-8').strip(), end='')
    else:
        subprocess.run(cmd)


def run_plot(
    output_dir: str,
    key: str,
    seed: int,
) -> None:
    print(f'plotting per-seed evaluation (seed={seed})')
    result_dir = os.path.join(output_dir, 'output', key)
    subprocess.run([
        'python3', plot_path,
        os.path.join(result_dir, 'results.json'),
        result_dir
    ])


def main():
    parser = argparse.ArgumentParser(description='Parallel evaluation runner')
    parser.add_argument('bench_path', help='path of Herbie benchmarks', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)
    parser.add_argument('num_seeds', help='number of seeds to run', type=int)
    parser.add_argument('--key', help='unique identifier for run [default: \'default\']', type=str)
    parser.add_argument('--parallel', help='maximum number of parallel runs [default: 1]', type=int)
    parser.add_argument('--threads', help='maximum number of threads [default: 1]', type=int)
    parser.add_argument('--start-seed', help='first seed to run (sequentially) [default: 1]', type=int)
    args = parser.parse_args()

    # extract command line arguments
    bench_path: str = os.path.join(curr_dir, args.bench_path)
    output_dir: str = os.path.join(curr_dir, args.output_dir)
    num_seeds: int = args.num_seeds
    key: str = args.key or default_key
    num_parallel: int = args.parallel or default_num_parallel
    num_threads: int = args.threads or default_num_threads
    start_seed: int = args.start_seed or default_start_seed

    # baseline evaluation
    run_baseline(
        key=key,
        bench_path=bench_path,
        output_dir=output_dir,
        num_parallel=num_parallel,
        num_threads=num_threads,
        num_seeds=num_seeds,
        start_seed=start_seed
    )

    # eval configurations
    configs = []
    hide_output = num_parallel > 1
    for seed in range(start_seed, start_seed + num_seeds):
        configs.append((bench_path, output_dir, f'{key}-{seed}', num_threads, seed, hide_output))

    # run eval in parallel
    if num_parallel > 1:
        with mp.Pool(processes=num_parallel) as pool:
            pool.starmap(run_seed, configs)
    else:
        for config in configs:
            run_seed(*config)

    # plot configurations
    configs = []
    for seed in range(start_seed, start_seed + num_seeds):
        configs.append((output_dir, f'{key}-{seed}', seed))

    # run plotting in parallel
    if num_parallel > 1:
        with mp.Pool(processes=num_parallel) as pool:
            pool.starmap(run_plot, configs)
    else:
        for config in configs:
            run_plot(*config)


if __name__ == "__main__":
    main()
