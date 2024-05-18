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

#############################
# Configuration

# Tuning and improvement
platforms = [
    'c',
    'python'
   # 'avx'
]

# Cross-platform comparison
#  platform1 <- platform2
cross_compare = [
    ('c', 'python')
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
    info = dict()
    cross_pat = re.compile('cross-compile-([^.]*).json')

    platforms_path = Path(output_dir).joinpath('output', name)
    for platform_path in platforms_path.iterdir():
        if platform_path.is_dir():
            platform_info = dict()
            for file_path in platform_path.iterdir():
                if file_path.is_file():
                    if file_path.name == 'tune.json':
                        with open(file_path, 'r') as f:
                            platform_info['tune'] = json.load(f)
                    elif file_path.name == 'improve.json':
                        with open(file_path, 'r') as f:
                            platform_info['improve'] = json.load(f)
                    elif file_path.name.startswith('cross-compile'):
                        matches = re.match(cross_pat, file_path.name)
                        name = matches.group(1)
                        if 'compare' not in platform_info:
                            platform_info['compare'] = dict()
                        with open(file_path, 'r') as f:
                            platform_info['compare'][name] = json.load(f)

            info[platform_path.name] = platform_info
        
    output_dir = Path(output_dir).joinpath('results.json')
    with open(output_dir, 'w') as f:
        json.dump(info, f)


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

    # run improvement
    for platform in platforms:
        run_improvement(
            name=name,
            platform=platform,
            bench_dir=bench_path,
            output_dir=output_dir,
            num_herbie_threads=num_herbie_threads,
            num_threads=num_threads
        )

    # run cross-platform comparison
    for platform1, platform2 in cross_compare:
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
