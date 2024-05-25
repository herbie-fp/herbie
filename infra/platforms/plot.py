"""Plots evaluation data"""

import argparse
import json
import os

import matplotlib.pyplot as plt
from pathlib import Path

from platforms.fpcore import FPCore
from platforms.runners import make_runner

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'server.rkt')

def plot_time(name: str, output_dir: Path, info: dict):
    """Plots Herbie cost estimate vs actual run time"""
    print(f'Plotting time {name}')

    costs = []
    times = []
    for input_info in info['cores']:
        for core_info in input_info['platform_cores']:
            cost = core_info['platform_core']['cost']
            time = core_info['time']

            costs.append(cost)
            times.append(time)
    
    plt.figure()
    plt.title("Estimated cost vs. actual run time")
    plt.xlabel("Estimated cost (Herbie)")
    plt.ylabel(f"Run time (???)") # TODO
    plt.scatter(costs, times)

    path = output_dir.joinpath(f'{name}-cost-vs-time.png')
    plt.savefig(str(path))
    plt.close()

def plot_improve(name: str, output_dir: Path, info):
    print(f'Plotting improve {name}')

    frontier_costs, frontier_errors = zip(*info["frontier"])

    plt.figure()
    plt.plot(frontier_costs, frontier_errors, label=name)
    plt.title('Estimated cost vs. cumulative average error (bits)')
    plt.xlabel('Estimated cost (Herbie)')
    plt.ylabel(f'Cumulative average error')

    path = output_dir.joinpath(f'{name}-pareto.png')
    plt.savefig(str(path))
    plt.close()

def plot_compare(name: str, name2: str, output_dir: Path, info):
    print(f'Plotting compare {name} <- {name2}')

    input_cores = []
    platform_cores = []
    supported_cores = []
    desugared_cores = []

    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = map(FPCore.from_json, core_info['platform_cores'])
        supported = map(FPCore.from_json, core_info['supported_cores'])
        desugared = map(FPCore.from_json, core_info['desugared_cores'])

        input_cores.append(input_core)
        platform_cores += platform
        supported_cores += supported
        desugared_cores += desugared

    runner = make_runner(
        platform='c',
        working_dir=output_dir,
        herbie_path=herbie_path
    )

    platform_frontier = runner.herbie_pareto(input_cores=input_cores, cores=platform_cores)
    platform_costs = list(map(lambda pt: pt[0], platform_frontier))
    platform_errs = list(map(lambda pt: pt[1], platform_frontier))    

    supported_frontier = runner.herbie_pareto(input_cores=input_cores, cores=supported_cores)
    supported_costs = list(map(lambda pt: pt[0], supported_frontier))
    supported_errs = list(map(lambda pt: pt[1], supported_frontier))

    desugared_frontier = runner.herbie_pareto(input_cores=input_cores, cores=desugared_cores)
    desugared_costs = list(map(lambda pt: pt[0], desugared_frontier))
    desugared_errs = list(map(lambda pt: pt[1], desugared_frontier))

    plt.plot(platform_costs, platform_errs, label=name)
    plt.plot(supported_costs, supported_errs, label=f'{name2} (supported)')
    plt.plot(desugared_costs, desugared_errs, label=f'{name2} (desugared)')
    plt.title('Estimated cost vs. cumulative average error (bits)')
    plt.xlabel('Estimated cost (Herbie)')
    plt.ylabel(f'Cumulative average error')
    plt.legend()

    path = output_dir.joinpath(f'{name}-vs-{name2}-pareto.png')
    plt.savefig(str(path))
    plt.close()

def main():
    parser = argparse.ArgumentParser(description='Herbie platforms eval')
    parser.add_argument('json_path', help='path to JSON file', type=str)
    parser.add_argument('output_dir', help='directory under which to produce plots', type=str)
    args = parser.parse_args()

    json_path: str = args.json_path
    output_dir: str = args.output_dir

    # Create output directory if needed
    output_dir = Path(output_dir)
    if not output_dir.exists():
        output_dir.mkdir(parents=True)

    # Read JSON
    with open(json_path, 'r') as f:
        report = json.load(f)

    # Iterate over platform data
    for name, platform_info in report.items():
        for field, field_info in platform_info.items():
            if field == 'improve':
                plot_improve(name, output_dir, field_info)
                plot_time(name, output_dir, field_info)
            elif field == 'compare':
                for name2, compare_info in field_info.items():
                    plot_compare(name, name2, output_dir, compare_info)


if __name__ == "__main__":
    main()
