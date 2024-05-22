from pathlib import Path
import subprocess
import argparse
import json
import os
import re

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
tune_path = os.path.join(script_dir, 'platforms', 'tune.py')
improve_path = os.path.join(script_dir, 'platforms', 'improve.py')
compare_path = os.path.join(script_dir, 'platforms', 'compare.py')
baseline_path = os.path.join(script_dir, 'platforms', 'baseline.py')
merge_path = os.path.join(script_dir, 'platforms', 'merge.py')

#############################
# Configuration

# Tuning and improvement
platforms = [
    'c',
    'python',
    'avx'
]

# Number of input points
num_tune_points = 10_000
num_eval_points = 10_000

# Runners

def run_tuning(
    name: str,
    platform: str,
    output_dir: str,
    num_threads: int
) -> None:
    subprocess.run([
        'python3', tune_path,
         '--threads', str(num_threads),
         '--num-points', str(num_tune_points),
         '--key', name,
         platform,
         output_dir
    ])
    
def run_baseline(
    name: str,
    bench_path: str,
    output_dir: str,
    num_herbie_threads: int
) -> None:
    subprocess.run([
        'python3', baseline_path,
         '--threads', str(num_herbie_threads),
         '--key', name,
         bench_path,
         output_dir,
    ])


def run_improvement(
    name: str,
    platform: str,
    bench_dir: str,
    output_dir: str,
    num_herbie_threads: int,
    num_threads: int
) -> None:
    subprocess.run([
        'python3', improve_path, 
        '--threads', str(num_threads),
        '--num-points', str(num_eval_points),
        '--key', name,
        '--herbie-threads', str(num_herbie_threads), 
        platform,
        bench_dir,
        output_dir
    ])

def run_cross_compile(
    name: str,
    platform1: str,
    platform2: str,
    output_dir: str,
    num_threads: int
) -> None:
    subprocess.run([
        'python3', compare_path, 
        '--threads', str(num_threads),
        '--key', name,
        platform1,
        platform2,
        output_dir
    ])


def merge_json(output_dir: str, name: str):
    subprocess.run([
        'python3', merge_path,
        '--key', name,
        output_dir
    ])


def main():
    parser = argparse.ArgumentParser(description='Herbie platforms eval')
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)
    parser.add_argument('bench_path', help='path of Herbie benchmarks', type=str)
    parser.add_argument('name', help='unique name of run', type=str)
    parser.add_argument('herbie_threads', help='number of Herbie threads', type=int)
    parser.add_argument('threads', help='number of multiprocessing threads', type=int)
    args = parser.parse_args()

    output_dir: str = args.output_dir
    bench_path: str = args.bench_path
    name: str = args.name
    num_herbie_threads: int = args.herbie_threads
    num_threads: int = args.threads

    # run tuning
    for platform in platforms:
        run_tuning(
            name=name,
            platform=platform,
            output_dir=output_dir,
            num_threads=num_threads
        )

    run baseline
    run_baseline(
        name=name,
        bench_path=bench_path,
        output_dir=output_dir,
        num_herbie_threads=num_herbie_threads
    )

    # run platform-based improvement
    for platform in platforms:
        run_improvement(
            name=name,
            platform=platform,
            bench_dir=bench_path,
            output_dir=output_dir,
            num_herbie_threads=num_herbie_threads,
            num_threads=num_threads
        )

    # run baseline comparison
    for platform in platforms:
        run_cross_compile(
            name=name,
            platform1=platform,
            platform2='baseline',
            output_dir=output_dir,
            num_threads=num_threads
        )

    # run cross-platform comparison
    for platform1 in platforms:
        for platform2 in platforms:
            if platform1 != platform2:
                run_cross_compile(
                    name=name,
                    platform1=platform1,
                    platform2=platform2,
                    output_dir=output_dir,
                    num_threads=num_threads
                )

    # merge report jsons
    merge_json(output_dir=output_dir, name=name)


if __name__ == "__main__":
    main()
