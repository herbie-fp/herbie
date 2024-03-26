from typing import List
import subprocess
import argparse
import os

script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
runner_path = os.path.join(script_dir, 'cost.py')

def run_config(output_dir: str, num_herbie_threads: int, num_threads: int, config):
    platform: str = config[0]
    bench_dir: str = config[1]
    num_points: int = config[2]
    baselines: List[str] = config[3]

    # tune
    tune_path = os.path.join(output_dir, platform, 'tune')
    subprocess.run([
        'python3', runner_path,
         '--threads', str(num_threads), '--num-points', str(num_points),
         '--tune', 
         platform, tune_path
    ])

    # cost
    cost_path = os.path.join(output_dir, platform, 'cost')
    subprocess.run([
        'python3', runner_path, 
        '--threads', str(num_threads), '--num-points', str(num_points),
        '--herbie-threads', str(num_herbie_threads), 
        '--herbie-input', bench_dir,
        platform, cost_path
    ])

    # baselines
    for baseline in baselines:
        cost_path = os.path.join(output_dir, platform, 'baseline', baseline)
        subprocess.run([
            'python3', runner_path, 
            '--threads', str(num_threads), '--num-points', str(num_points),
            '--herbie-threads', str(num_herbie_threads), 
            '--herbie-input', bench_dir,
            '--baseline', baseline,
            platform, cost_path
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
        if len(atoms) < 3:
            raise RuntimeError(f'Malformed config: {config_str}')
        
        platform = atoms[0]
        bench = atoms[1]
        num_points = atoms[2]
        baselines = atoms[3:]
        configs.append((platform, bench, num_points, baselines))

    for config in configs:
        run_config(output_dir, num_herbie_threads, num_threads, config)

if __name__ == "__main__":
    main()
