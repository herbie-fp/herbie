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
    path = output_dir.joinpath(f'{name}-vs-{name2}-pareto.png')

    input_cores = []
    platform_cores = []
    supported_cores = []
    desugared_cores = []

    num_platform = 0
    num_supported = 0
    num_desugared = 0

    num_platform_impls = 0
    num_supported_impls = 0
    num_desugared_impls = 0

    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = list(map(FPCore.from_json, core_info['platform_cores']))
        supported = list(map(FPCore.from_json, core_info['supported_cores']))
        desugared = list(map(FPCore.from_json, core_info['desugared_cores']))
    
        any_platform = len(platform) > 0
        any_supported = len(supported) > 0
        any_desugared = len(desugared) > 0

        if any_platform:
            num_platform += 1
            num_platform_impls += len(platform)
        if any_supported:
            num_supported += 1
            num_supported_impls += len(supported)
        if any_desugared:
            num_desugared += 1
            num_desugared_impls += len(desugared)

        if any_platform and any_supported and any_desugared:
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

    # Colors
    platform_color = 'blue'
    supported_color = 'orange'
    desugared_color = 'green'

    fig, (ax1, ax2, ax3) = plt.subplots(ncols=3, width_ratios=[3, 1, 1])
    plt.subplots_adjust(bottom=0.2, wspace=0.3)
    fig.suptitle(f'Comparing {name} and {name2}')

    # Pareto frontiers
    ax1.set_title('Est. cost vs. cumulative avg. error (bits)', size='medium')
    ax1.set(xlabel='Estimated cost (Herbie)', ylabel=f'Cumulative average error')
    ax1.plot(platform_costs, platform_errs, label=f'{name} (Chassis)', color=platform_color)
    ax1.plot(supported_costs, supported_errs, label=f'{name2} (supported)', color=supported_color)
    ax1.plot(desugared_costs, desugared_errs, label=f'{name2} (desugared)', color=desugared_color)

    # Implementable FPCores
    ax2.set_title('# Benchmarks', size='medium')
    ax2.bar(0, num_platform, color=platform_color)
    ax2.bar(1, num_supported, color=supported_color)
    ax2.bar(2, num_desugared, color=desugared_color)
    ax2.get_xaxis().set_visible(False)
    ax2.set_ylim(0, num_platform)

    # Number of implementations
    ax3.set_title('# Implementations', size='medium')
    ax3.bar(0, num_platform_impls, color=platform_color)
    ax3.bar(1, num_supported_impls, color=supported_color)
    ax3.bar(2, num_desugared_impls, color=desugared_color)
    ax3.get_xaxis().set_visible(False)

    # Legend
    fig.legend(loc='lower right')
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
