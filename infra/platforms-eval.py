import argparse
import os
import multiprocessing as mp

from typing import List
from subprocess import Popen, PIPE, STDOUT
import subprocess

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
platforms_dir = os.path.join(script_dir, 'platforms')

tune_path = os.path.join(platforms_dir, 'tune.py')
improve_path = os.path.join(platforms_dir, 'improve.py')
compare_path = os.path.join(platforms_dir, 'compare.py')
baseline_path = os.path.join(platforms_dir, 'baseline.py')
merge_path = os.path.join(platforms_dir, 'merge.py')
plot_path = os.path.join(platforms_dir, 'plot.py')

curr_dir = os.getcwd()

# Defaults
default_key = 'default'
default_num_parallel = 1
default_num_threads = 1
default_start_seed = 1

#############################
# Configuration

# Tuning and improvement
platforms = [
    # Hardware
    # 'arith',
    # 'arith-fma'
    # 'avx',

    # Language
    # 'c',
    # 'python',
    'julia',

    # Library
    # 'numpy',
    # 'vdt'
    # 'fdlibm'
]

# Evals
evals = [
    # 'baseline',
    'compare',
    # ablation
]

# Number of input points
num_tune_points = 10_000
num_eval_points = 10_000

#############################
# Runner

def run_subprocess(cmd: List[str], hide_output: bool):
    if hide_output:
        p = Popen(cmd, stdout=PIPE, stderr=STDOUT)
        stdout, _ = p.communicate()
        print(stdout.decode('utf-8').strip(), end='')
    else:
        subprocess.run(cmd)


def run_tuning(
    name: str,
    platform: str,
    output_dir: str,
    num_threads: int,
    seed: int
) -> None:
    print(f'Tuning eval for `{platform}`')
    subprocess.run([
        'python3', tune_path,
         '--threads', str(num_threads),
         '--num-points', str(num_tune_points),
         '--key', name,
         '--seed', str(seed),
         platform,
         output_dir
    ])

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


def run_improvement(
    name: str,
    platform: str,
    bench_dir: str,
    output_dir: str,
    num_herbie_threads: int,
    num_threads: int,
    seed: int,
    hide_output: bool
) -> None:
    print(f'Improvement eval for `{platform}`')
    cmd = [
        'python3', improve_path,
        '--threads', str(num_threads),
        '--num-points', str(num_eval_points),
        '--herbie-threads', str(num_herbie_threads),
        '--key', name,
        '--seed', str(seed),
        platform,
        bench_dir,
        output_dir
    ]

    run_subprocess(cmd, hide_output)


def run_cross_compile(
    name: str,
    platform1: str,
    platform2: str,
    output_dir: str,
    num_threads: int,
    seed: int,
    hide_output: bool
) -> None:
    print(f'Compare eval for `{platform1}` <- `{platform2}`')
    cmd = [
        'python3', compare_path,
        '--threads', str(num_threads),
        '--key', name,
        '--seed', str(seed),
        platform1,
        platform2,
        output_dir
    ]

    run_subprocess(cmd, hide_output)


def run_merge_json(
    name: str,
    output_dir: str,
    hide_output: bool
):
    cmd = [
        'python3', merge_path,
        '--key', name,
        output_dir
    ]

    run_subprocess(cmd, hide_output)


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

def run_seed(
    bench_path: str,
    output_dir: str,
    key: str,
    num_threads: int,
    seed: int,
    hide_output: bool
) -> None:
    print(f'running per-seed evaluation (seed={seed})')
    # run platform-based improvement
    for platform in platforms:
        run_improvement(
            name=key,
            platform=platform,
            bench_dir=bench_path,
            output_dir=output_dir,
            num_herbie_threads=num_threads,
            num_threads=num_threads,
            seed=seed,
            hide_output=hide_output
        )

    # run baseline comparison
    if 'baseline' in evals:
        for platform in platforms:
            run_cross_compile(
                name=key,
                platform1=platform,
                platform2='baseline',
                output_dir=output_dir,
                num_threads=num_threads,
                seed=seed,
                hide_output=hide_output
            )

    # run cross-platform comparison
    if 'compare' in evals:
        for platform1 in platforms:
            for platform2 in platforms:
                if platform1 != platform2:
                    run_cross_compile(
                        name=key,
                        platform1=platform1,
                        platform2=platform2,
                        output_dir=output_dir,
                        num_threads=num_threads,
                        seed=seed,
                        hide_output=hide_output
                    )

    # merge report jsons
    run_merge_json(
        name=key,
        output_dir=output_dir,
        hide_output=hide_output
    )


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
    if 'baseline' in evals:
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
        configs.append((
            bench_path,
            output_dir,
            f'{key}-{seed}',
            num_threads,
            seed,
            hide_output
        ))

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
