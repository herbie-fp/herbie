from typing import List
import subprocess
import argparse
import os

script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
runner_path = os.path.join(script_dir, 'cost.py')

def run_config(output_dir: str, num_herbie_threads: int, num_threads: int, config):
    name: str = config[0]
    platform: str = config[1]
    bench_dir: str = config[2]
    num_points: int = config[3]
    baselines: List[str] = config[4]

    # tune
    subprocess.run([
        'python3', runner_path,
         '--threads', str(num_threads),
         '--num-points', str(num_points),
         '--key', name,
         '--tune',
         platform, output_dir
    ])

    # cost
    subprocess.run([
        'python3', runner_path, 
        '--threads', str(num_threads),
        '--num-points', str(num_points),
        '--key', name,
        '--herbie-threads', str(num_herbie_threads), 
        '--herbie-input', bench_dir,
        platform, output_dir
    ])

    # baselines
    for baseline in baselines:
        subprocess.run([
            'python3', runner_path, 
            '--threads', str(num_threads),
            '--num-points', str(num_points),
            '--key', name,
            '--herbie-threads', str(num_herbie_threads), 
            '--herbie-input', bench_dir,
            '--baseline', baseline,
             platform, output_dir
        ])

def main():
    parser = argparse.ArgumentParser(description='Herbie platforms eval')
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)
    parser.add_argument('herbie_threads', help='number of Herbie threads', type=int)
    parser.add_argument('threads', help='number of multiprocessing threads', type=int)
    parser.add_argument('configs', nargs=argparse.REMAINDER)
    args = parser.parse_args()

    output_dir: str = args.output_dir
    num_herbie_threads: int = args.herbie_threads
    num_threads: int = args.threads
    config_strs: List[str] = args.configs

    # config parsing
    configs = []
    for config_str in config_strs:
        atoms = config_str.strip().split(' ')
        if len(atoms) < 4:
            raise RuntimeError(f'Malformed config: {config_str}')
        
        name = atoms[0]
        platform = atoms[1]
        bench = atoms[2]
        num_points = atoms[3]
        baselines = atoms[4:]
        configs.append((name, platform, bench, num_points, baselines))

    for config in configs:
        run_config(output_dir, num_herbie_threads, num_threads, config)

if __name__ == "__main__":
    main()
