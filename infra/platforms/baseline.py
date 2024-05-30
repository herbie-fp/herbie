import argparse
import subprocess
import multiprocessing as mp
import os
import shutil

from pathlib import Path

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
infra_dir, _ = os.path.split(script_dir)
herbie_dir, _ = os.path.split(infra_dir)
curr_dir = os.getcwd()

# Defaults
default_key = 'default'
default_num_parallel = 1
default_num_threads = 1
default_start_seed = 1

def install_herbie(install_dir: Path):
    subprocess.run(['git', 'clone', '--branch', 'v2.0.2', 'https://github.com/herbie-fp/herbie', install_dir])
    subprocess.run(['make', 'install'], cwd=install_dir)

def run_herbie(
    bench_path: Path,
    output_dir: Path,
    num_threads: int,
    key: str,
    seed: int
):
    # Baseline directory
    key = 'default' if key is None else key
    baseline_dir = output_dir.joinpath('baseline', key)
    if not baseline_dir.exists():
        baseline_dir.mkdir(parents=True)

    # Subpaths
    report_dir = baseline_dir.joinpath('report')
    json_path = baseline_dir.joinpath('baseline.json')

    report_dir = output_dir.joinpath('baseline', key, 'report')
    if not report_dir.exists():
        report_dir.mkdir(parents=True)

    subprocess.run([
        'racket', '-l', 'herbie/herbie',
        'report',
        '--threads', str(num_threads),
        '--seed', str(seed),
        str(bench_path),
        str(report_dir)
    ])

    # Copy JSON and delete report directory
    shutil.copy(report_dir.joinpath('results.json'), json_path)
    shutil.rmtree(report_dir)


def reinstall_herbie():
    subprocess.run(['make', 'install'], cwd=herbie_dir)

def main():
    parser = argparse.ArgumentParser(description='Herbie baseline eval')
    parser.add_argument('bench_path', help='path of Herbie benchmarks', type=str)
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)
    parser.add_argument('num_seeds', help='number of seeds to run', type=int)
    parser.add_argument('--key', help='unique identifier for run [default: \'default\']', type=str)
    parser.add_argument('--parallel', help='maximum number of parallel runs [default: 1]', type=int)
    parser.add_argument('--threads', help='maximum number of threads [default: 1]', type=int)
    parser.add_argument('--start-seed', help='first seed to run (sequentially) [default: 1]', type=int)
    args = parser.parse_args()

    # extract command line arguments
    bench_path: str = Path(curr_dir).joinpath(args.bench_path)
    output_dir: str = Path(curr_dir).joinpath(args.output_dir)
    num_seeds: int = args.num_seeds
    key: str = args.key or default_key
    num_parallel: int = args.parallel or default_num_parallel
    num_threads: int = args.threads or default_num_threads
    start_seed: int = args.start_seed or default_start_seed

    # Install directory
    install_dir = output_dir.joinpath('herbie-2.0')
    if not install_dir.exists():
        install_dir.mkdir(parents=True)

    # Install Herbie
    install_herbie(install_dir)

    # Parallel configs
    configs = []
    for seed in range(start_seed, start_seed + num_seeds):
        configs.append((bench_path, output_dir, num_threads, f'{key}-{seed}', seed))

    # Run Herbie in parallel
    if num_parallel > 1:
        with mp.Pool(processes=num_parallel) as pool:
            pool.starmap(run_herbie, configs)
    else:
        for config in configs:
            run_herbie(*config)

    # Reinstall local Herbie
    reinstall_herbie()


if __name__ == "__main__":
    main()
