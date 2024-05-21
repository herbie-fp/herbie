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

    # Handle key
    working_dir = working_dir.joinpath('baseline')
    if key is None:
        working_dir = working_dir.joinpath('default')
    else:
        working_dir = working_dir.joinpath(key)

    # Create the output directory
    if not working_dir.exists():
        working_dir.mkdir(parents=True)

    # Subdirectories
    install_dir = working_dir.joinpath('herbie')
    report_dir = working_dir.joinpath('report')

    # Install Herbie
    install_herbie(install_dir)

    # Run Herbie on benchmarks
    run_herbie(bench_path, report_dir, threads, seed)
    shutil.copy(report_dir.joinpath('results.json'), working_dir.joinpath('baseline.json'))

    # Reinstall local Herbie and delete directories
    shutil.rmtree(install_dir)
    shutil.rmtree(report_dir)
    reinstall_herbie()


if __name__ == "__main__":
    main()
