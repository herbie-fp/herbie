import argparse
import subprocess
import os
import shutil

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
infra_dir, _ = os.path.split(script_dir)
herbie_dir, _ = os.path.split(infra_dir)
curr_dir = os.getcwd()

# defaults
default_seed = 1

def install_herbie():
    subprocess.run(['raco', 'pkg', 'remove', 'herbie', 'egg-herbie', 'fpbench'])
    subprocess.run(['raco', 'pkg', 'install', '--auto', 'herbie'])

def run_herbie(bench_path: str, working_dir: str, threads: int, seed):
    # Insane way to run Herbie
    subprocess.run([
        'racket', '-l', 'herbie/herbie',
        'report',
        '--threads', str(threads),
        '--seed', str(seed),
        bench_path,
        working_dir
    ])

def reinstall_herbie():
    subprocess.run(['raco', 'pkg', 'remove', 'herbie', 'egg-herbie', 'fpbench'])
    subprocess.run(['make', 'install'], cwd=herbie_dir)

def main():
    parser = argparse.ArgumentParser(description='Herbie baseline eval')
    parser.add_argument('bench_path', help='directory or FPCore for Herbie to run on', type=str)
    parser.add_argument('working_dir', help='directory to emit all working files', type=str)
    parser.add_argument('json_path', help='path of the stored JSON', type=str)
    parser.add_argument('--threads', help='number of Herbie threads', type=int)
    parser.add_argument('--seed', help='random seed to use for Herbie', type=int)
    args = parser.parse_args()

    # extract command line arguments
    bench_path = os.path.join(curr_dir, args.bench_path)
    working_dir = os.path.join(curr_dir, args.working_dir)
    json_path = os.path.join(curr_dir, args.json_path)
    threads: int = args.threads
    seed: int = args.seed

    if seed is None:
        seed = default_seed

    # Install Herbie
    install_herbie()

    # Run Herbie on benchmarks
    run_herbie(bench_path, working_dir, threads, seed)
    shutil.copy(os.path.join(working_dir, 'results.json'), json_path)
    shutil.rmtree(working_dir)

    # Reinstall local Herbie
    reinstall_herbie()


if __name__ == "__main__":
    main()
