import argparse
import subprocess
import os
import shutil

from pathlib import Path

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
infra_dir, _ = os.path.split(script_dir)
herbie_dir, _ = os.path.split(infra_dir)
curr_dir = os.getcwd()

# defaults
default_seed = 1

def install_herbie(install_dir: Path):
    subprocess.run(['git', 'clone', '--branch', 'v2.0.2', 'https://github.com/herbie-fp/herbie', install_dir])
    subprocess.run(['make', 'install'], cwd=install_dir)

def run_herbie(bench_path: Path, report_dir: Path, threads: int, seed):
    subprocess.run([
        'racket', '-l', 'herbie/herbie',
        'report',
        '--threads', str(threads),
        '--seed', str(seed),
        str(bench_path),
        str(report_dir)
    ])

def reinstall_herbie():
    subprocess.run(['make', 'install'], cwd=herbie_dir)

def main():
    parser = argparse.ArgumentParser(description='Herbie baseline eval')
    parser.add_argument('bench_path', help='directory or FPCore for Herbie to run on', type=str)
    parser.add_argument('working_dir', help='directory to emit all working files', type=str)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    parser.add_argument('--threads', help='number of Herbie threads', type=int)
    parser.add_argument('--seed', help='random seed to use for Herbie', type=int)
    args = parser.parse_args()

    # extract command line arguments
    bench_path = Path(os.path.join(curr_dir, args.bench_path))
    working_dir = Path(os.path.join(curr_dir, args.working_dir))
    key: str = args.key
    threads: int = args.threads
    seed: int = args.seed

    if seed is None:
        seed = default_seed

    # Install directory
    install_dir = working_dir.joinpath('herbie-2.0')
    if not install_dir.exists():
        install_dir.mkdir(parents=True)

    # Baseline directory
    key = 'default' if key is None else key
    baseline_dir = working_dir.joinpath('baseline', key)
    if not baseline_dir.exists():
        baseline_dir.mkdir(parents=True)

    # Subpaths
    report_dir = baseline_dir.joinpath('report')
    json_path = baseline_dir.joinpath('baseline.json')

    report_dir = working_dir.joinpath('baseline', key, 'report')
    if not report_dir.exists():
        report_dir.mkdir(parents=True)

    # Install Herbie
    install_herbie(install_dir)

    # Run Herbie on benchmarks
    run_herbie(bench_path, report_dir, threads, seed)
    shutil.copy(report_dir.joinpath('results.json'), json_path)

    # Reinstall local Herbie and delete directories
    shutil.rmtree(report_dir)
    reinstall_herbie()


if __name__ == "__main__":
    main()
