"""Plots evaluation data"""

import argparse
import json
import os

import matplotlib.pyplot as plt
from typing import List, Dict
from pathlib import Path

from platforms.fpcore import FPCore
from platforms.shim import shim_pareto

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
    plt.ylabel(f"Run time ({info['time_unit']})")
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

# Colors
input_color = 'black'
platform_color = 'blue'
supported_color = 'orange'
desugared_color = 'green'

def core_max_error(core: FPCore) -> int:
    if core.prec == 'binary64':
        return 64
    elif core.prec == 'binary32':
        return 32
    else:
        raise RuntimeError('Unknown precision', core.prec)

def plot_compare1(name: str, name2: str, output_dir: Path, info):
    print(f'Plotting compare {name} <- {name2}')
    path = output_dir.joinpath(f'{name}-vs-{name2}-pareto.png')

    input_cores = []
    platform_cores = []
    supported_cores = []
    desugared_cores = []

    num_cores = len(info['cores'])
    num_platform = 0
    num_supported = 0
    num_desugared = 0

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
        if any_supported:
            num_supported += 1
        if any_desugared:
            num_desugared += 1

        if any_platform and any_supported and any_desugared:
            input_cores.append(input_core)
            platform_cores += platform
            supported_cores += supported
            desugared_cores += desugared

    platform_frontier, supported_frontier, desugared_frontier = shim_pareto(
        platform_cores,
        supported_cores,
        desugared_cores
    )

    platform_costs, platform_errs = zip(*platform_frontier)
    supported_costs, supported_errs = zip(*supported_frontier)
    desugared_costs, desugared_errs = zip(*desugared_frontier)

    fig, (ax1, ax2) = plt.subplots(ncols=2, gridspec_kw={'width_ratios': [9, 1]})
    plt.subplots_adjust(wspace=0.15)
    fig.suptitle(f'Comparing {name} and {name2}')

    # Pareto frontiers
    ax1.set_title('Est. cost vs. cumulative avg. error (bits)', size='medium')
    ax1.set(xlabel='Estimated cost (Herbie)', ylabel='Cumulative average error')
    ax1.plot(platform_costs, platform_errs, label=f'{name} (Chassis)', color=platform_color)
    ax1.plot(supported_costs, supported_errs, label=f'{name2} (supported)', color=supported_color)
    ax1.plot(desugared_costs, desugared_errs, label=f'{name2} (desugared)', color=desugared_color)
    ax1.legend()

    # Implementable FPCores
    ax2.set_title('#B', size='medium')
    ax2.bar(0, num_platform, color=platform_color)
    ax2.bar(1, num_supported, color=supported_color)
    ax2.bar(2, num_desugared, color=desugared_color)
    ax2.get_xaxis().set_visible(False)
    ax2.set_ylim(0, num_cores)

    # Number of implementations
    # ax3.set_title('# Implementations', size='medium')
    # ax3.bar(0, num_platform_impls, color=platform_color)
    # ax3.bar(1, num_supported_impls, color=supported_color)
    # ax3.bar(2, num_desugared_impls, color=desugared_color)
    # ax3.get_xaxis().set_visible(False)

    # Legend
    plt.tight_layout()
    plt.savefig(str(path))
    plt.close()

def plot_compare_all(output_dir: Path, entries):
    print(f'Plotting all platform comparison')
    path = output_dir.joinpath(f'comparison-pareto.png')

    names = []
    for name, _ , _ in entries:
        if name not in names:
            names.append(name)

    num_platforms = len(names)
    fig, axs = plt.subplots(ncols=num_platforms, nrows=num_platforms, figsize=((8, 8)))

    # fig.suptitle('Platform vs. platform comparison')
    fig.supxlabel('Estimated speedup (Herbie)')
    fig.supylabel('Cumulative average accuracy (bits)')

    # plot diagonal frontiers
    by_platform: Dict[str, List[FPCore]] = dict()
    for name, _, info in entries:
        if name not in by_platform:
            input_cores = []
            cores = []
            for core_info in info['cores']:
                input_core = FPCore.from_json(core_info['input_core'])
                platform_cores = list(map(FPCore.from_json, core_info['platform_cores']))
                if len(platform_cores) > 0:
                    input_cores.append(input_core)
                    cores += platform_cores

            max_error = sum(map(lambda c: core_max_error(c), input_cores))
            input_costs, input_errs = zip(*map(lambda c: (c.cost, c.err), input_cores))
            input_cost, input_err = sum(input_costs), sum(input_errs)
            input_speedup, input_accuracy = 1.0, max_error - input_err

            # compute frontier
            platform_frontier, *_ = shim_pareto(cores)
            platform_costs, platform_errs = zip(*platform_frontier)

            # transform (cost, err) -> (speedup, accuracy)
            platform_speedups = list(map(lambda c: input_cost / c, platform_costs))
            platform_accuracies = list(map(lambda e: max_error - e, platform_errs))

            # plot
            i = names.index(name)
            axs[i, i].plot([input_speedup], [input_accuracy], 's', color=input_color)
            axs[i, i].plot(platform_speedups, platform_accuracies, color=platform_color)

    # plot comparison frontiers
    for name, name2, info in entries:
        i = names.index(name)
        j = names.index(name2)
        ax = axs[i][j]

        input_cores: List[FPCore] = []
        platform_cores: List[FPCore] = []
        supported_cores: List[FPCore] = []
        desugared_cores: List[FPCore] = []
        for core_info in info['cores']:
            input_core = FPCore.from_json(core_info['input_core'])
            platform = list(map(FPCore.from_json, core_info['platform_cores']))
            supported = list(map(FPCore.from_json, core_info['supported_cores']))
            desugared = list(map(FPCore.from_json, core_info['desugared_cores']))

            if len(platform) > 0 and len(supported) > 0 and len(desugared) > 0:
                input_cores.append(input_core)
                platform_cores += platform
                supported_cores += supported
                desugared_cores += desugared

        # compute starting point
        max_error = sum(map(lambda c: core_max_error(c), input_cores))
        input_costs, input_errs = zip(*map(lambda c: (c.cost, c.err), input_cores))
        input_cost, input_err = sum(input_costs), sum(input_errs)

        input_speedup = 1.0
        input_accuracy = max_error - input_err

        # compute frontiers
        platform_frontier, supported_frontier, desugared_frontier = shim_pareto(
            platform_cores,
            supported_cores,
            desugared_cores
        )

        # decompose frontiers
        platform_costs, platform_errs = zip(*platform_frontier)
        supported_costs, supported_errs = zip(*supported_frontier)
        desugared_costs, desugared_errs = zip(*desugared_frontier)

        # transform (cost, err) -> (speedup, accuracy)
        platform_speedups = list(map(lambda c: input_cost / c, platform_costs))
        platform_accuracies = list(map(lambda e: max_error - e, platform_errs))
        supported_speedups = list(map(lambda c: input_cost / c, supported_costs))
        supported_accuracies = list(map(lambda e: max_error - e, supported_errs))
        desugared_speedups = list(map(lambda c: input_cost / c, desugared_costs))
        desugared_accuracies = list(map(lambda e: max_error - e, desugared_errs))

        # Pareto frontiers
        # ax.plot([input_cost], [input_err], 's', color='black')
        # ax.plot(platform_costs, platform_errs, color=platform_color)
        # ax.plot(supported_costs, supported_errs, color=supported_color)
        # ax.plot(desugared_costs, desugared_errs, color=desugared_color)
        
        ax.plot([input_speedup], [input_accuracy], 's', color=input_color)
        ax.plot(platform_speedups, platform_accuracies, color=platform_color)
        ax.plot(supported_speedups, supported_accuracies, color=supported_color)
        ax.plot(desugared_speedups, desugared_accuracies, color=desugared_color)

    # set labels
    for i, name in enumerate(names):
        axs[0][i].set(xlabel=name)
        axs[i][len(names) - 1].set(ylabel=name)
        for j, _ in enumerate(names):
            axs[i, j].xaxis.set_label_position('top')
            axs[i, j].yaxis.set_label_position('right')
            # axs[i, j].xaxis.set_ticks([])
            # axs[i, j].yaxis.set_ticks([])

    plt.tight_layout()
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
    improve_reports = []
    baseline_reports = []
    compare_reports = []
    for name, platform_info in report.items():
        for field, field_info in platform_info.items():
            if field == 'improve':
                improve_reports.append((name, field_info))
            elif field == 'compare':
                for name2, compare_info in field_info.items():
                    if name2 == 'baseline':
                        baseline_reports.append((name, compare_info))
                    else:
                        compare_reports.append((name, name2, compare_info))

    # Per-platform plot
    for name, info in improve_reports:
        plot_improve(name, output_dir, info)
        plot_time(name, output_dir, info)

    # Baseline plot
    for name, info in baseline_reports:
        plot_compare1(name, 'baseline', output_dir, info)

    # Comparison plot
    if compare_reports:
        plot_compare_all(output_dir, compare_reports)
        for name, name2, info in compare_reports:
            plot_compare1(name, name2, output_dir, info)


if __name__ == "__main__":
    main()
