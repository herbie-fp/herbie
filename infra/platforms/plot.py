"""Plots evaluation data"""

import argparse
import json
import os

import matplotlib.pyplot as plt
from typing import Dict, List, Tuple
from pathlib import Path

from platforms.fpcore import FPCore
from platforms.shim import shim_pareto

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'server.rkt')


def platform_cost_time(info):
    costs = []
    times = []
    for input_info in info['cores']:
        for core_info in input_info['platform_cores']:
            cost = core_info['platform_core']['cost']
            time = core_info['time']
            costs.append(cost)
            times.append(time)
    
    return costs, times

def plot_time(name: str, output_dir: Path, info: dict):
    """Plots Herbie cost estimate vs actual run time"""
    print(f'Plotting time {name}')

    costs, times = platform_cost_time(info)
    
    plt.figure()
    plt.title("Estimated cost vs. actual run time")
    plt.xlabel("Estimated cost")
    plt.ylabel(f"Run time ({info['time_unit']})")
    plt.scatter(costs, times)

    path = output_dir.joinpath(f'{name}-cost-vs-time.png')
    plt.savefig(str(path))
    plt.close()

def plot_time_all(output_dir: Path, entries):
    print(f'Plotting time for all platforms')
    path = output_dir.joinpath(f'cost-vs-time.png')
    size = 8

    names = []
    for name, _ in entries:
        names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    nrows = -(num_platforms // -3) # ceil_div(num_platforms, 3)
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))

    time_unit = None
    for _, info in entries:
        if time_unit is None:
            time_unit = info['time_unit']
        elif time_unit != info['time_unit']:
            raise RuntimeError('Time units do not match')

    fig.supxlabel("Estimated cost")
    fig.supylabel(f"Run time ({time_unit})")

    for i, (name, info) in enumerate(entries):
        costs, times = platform_cost_time(info)
        ax = axs[i // 3, i % 3]
        ax.scatter(costs, times)
        ax.set_title(name)
   
    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3]
        fig.delaxes(ax)

    plt.tight_layout()
    plt.savefig(str(path))
    plt.close()

def plot_improve(name: str, output_dir: Path, info):
    """Platform pareto frontier."""
    print(f'Plotting improve {name}')

    frontier_costs, frontier_errors = zip(*info["frontier"])

    plt.figure()
    plt.plot(frontier_costs, frontier_errors, label=name)
    plt.title('Estimated cost vs. cumulative average error (bits)')
    plt.xlabel('Estimated cost')
    plt.ylabel(f'Cumulative average error')

    path = output_dir.joinpath(f'{name}-pareto.png')
    plt.savefig(str(path))
    plt.close()

# Colors
input_color = 'black'
platform_color = 'blue'
supported_color = 'orange'
desugared_color = 'green'

input_style = 's'
platform_style = '.'
supported_style = '+'
desugared_style = 'x'

def core_max_error(core: FPCore) -> int:
    if core.prec == 'binary64':
        return 64
    elif core.prec == 'binary32':
        return 32
    else:
        raise RuntimeError('Unknown precision', core.prec)

def comparison_frontiers(info):
    num_input = 0
    num_platform = 0
    num_supported = 0
    num_desugared = 0

    input_cores: List[FPCore] = []
    platform_cores: List[FPCore] = []
    supported_cores: List[FPCore] = []
    desugared_cores: List[FPCore] = []
    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = list(map(FPCore.from_json, core_info['platform_cores']))
        supported = list(map(FPCore.from_json, core_info['supported_cores']))
        desugared = list(map(FPCore.from_json, core_info['desugared_cores']))

        num_input += 1
        if platform:
            num_platform += 1
        elif supported:
            num_supported += 1
        elif desugared:
            num_desugared += 1

        if platform and supported and desugared:
            input_cores.append(input_core)
            platform_cores += platform
            supported_cores += supported
            desugared_cores += desugared

    # compute starting point
    max_error = sum(map(lambda c: core_max_error(c), input_cores))
    input_costs, input_errs = zip(*map(lambda c: (c.cost, c.err), input_cores))
    input_cost, input_error = sum(input_costs), sum(input_errs)
    transform = lambda pt: (input_cost / pt[0], max_error - pt[1])

    # compute (cost, error) frontiers
    platform_frontier, supported_frontier, desugared_frontier = shim_pareto(platform_cores, supported_cores, desugared_cores)

    # compute (speedup, accuracy) frontiers
    input_speedup, input_accuracy = transform((input_cost, input_error))
    platform_frontier2 = list(map(transform, platform_frontier))
    supported_frontier2 = list(map(transform, supported_frontier))
    desugared_frontier2 = list(map(transform, desugared_frontier))

    return (input_cost, input_error), (input_speedup, input_accuracy), num_input, \
        platform_frontier, platform_frontier2, num_platform, \
        supported_frontier, supported_frontier2, num_supported, \
        desugared_frontier, desugared_frontier2, num_desugared

def plot_compare1(name: str, name2: str, output_dir: Path, info):
    """Single platform vs. platform comparison"""
    print(f'Plotting compare {name} <- {name2}')
    path = output_dir.joinpath(f'{name}-vs-{name2}-pareto.png')

    input_pt, _, num_input, \
        platform_frontier, _, num_platform, \
        supported_frontier, _, num_supported, \
        desugared_frontier, _, num_desugared = comparison_frontiers(info)

    input_cost, input_error = input_pt
    platform_costs, platform_errs = zip(*platform_frontier)
    supported_costs, supported_errs = zip(*supported_frontier)
    desugared_costs, desugared_errs = zip(*desugared_frontier)

    fig, (ax1, ax2) = plt.subplots(ncols=2, gridspec_kw={'width_ratios': [9, 1]})
    plt.subplots_adjust(wspace=0.15)
    fig.suptitle(f'Comparing {name} and {name2}')

    # Pareto frontiers
    ax1.set_title('Est. cost vs. cumulative avg. error (bits)', size='medium')
    ax1.set(xlabel='Estimated cost', ylabel='Cumulative average error')
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
    ax2.set_ylim(0, num_input)

    # Legend
    plt.tight_layout()
    plt.savefig(str(path))
    plt.close()

def plot_baseline_all(output_dir: Path, entries):
    """Entire baseline comparison (N)."""
    print(f'Plotting all baseline comparison')
    path = output_dir.joinpath(f'baseline-pareto.png')
    invert = True # (speedup, accuracy) vs. (cost, error)
    size = 8

    names = []
    for name, _ in entries:
        names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    nrows = -(num_platforms // -3) # ceil_div(num_platforms, 3)
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))

    if invert:
        fig.supxlabel('Estimated speedup')
        fig.supylabel('Cumulative average accuracy (bits)')
    else:
        fig.supxlabel('Cumulative estimated cost')
        fig.supylabel('Cumulative average eror (bits)')

    for i, (name, info) in enumerate(entries):
        input_pt, input_pt2, num_input, \
            platform_frontier, platform_frontier2, num_platform, \
            supported_frontier, supported_frontier2, num_supported, \
            desugared_frontier, desugared_frontier2, num_desugared = comparison_frontiers(info)

        # decompose frontiers
        input_cost, input_err = input_pt
        platform_costs, platform_errs = zip(*platform_frontier)
        supported_costs, supported_errs = zip(*supported_frontier)
        desugared_costs, desugared_errs = zip(*desugared_frontier)

        input_speedup, input_accuracy = input_pt2
        platform_speedups, platform_accuracies = zip(*platform_frontier2)
        supported_speedups, supported_accuracies = zip(*supported_frontier2)
        desugared_speedups, desugared_accuracies = zip(*desugared_frontier2)

        ax = axs[i // 3, i % 3]
        ax.set_title(name, size='medium')
        if invert:
            ax.plot([input_speedup], [input_accuracy], input_style, color=input_color)
            ax.plot(platform_speedups, platform_accuracies, platform_style, color=platform_color)
            ax.plot(supported_speedups, supported_accuracies, supported_style, color=supported_color)
            ax.plot(desugared_speedups, desugared_accuracies, desugared_style, color=desugared_color)
        else:
            ax.plot([input_cost], [input_err], input_style, color='black')
            ax.plot(platform_costs, platform_errs, platform_style, color=platform_color)
            ax.plot(supported_costs, supported_errs, supported_style, color=supported_color)
            ax.plot(desugared_costs, desugared_errs, desugared_style, color=desugared_color)

    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3]
        fig.delaxes(ax)

    plt.tight_layout()
    plt.savefig(str(path))
    plt.close()


def plot_compare_all(output_dir: Path, entries):
    """Entire platform vs. platform comparison (N^2 table)."""
    print(f'Plotting all platform comparison')
    path = output_dir.joinpath(f'comparison-pareto.png')
    invert = True # (speedup, accuracy) vs. (cost, error)
    size = 12

    names = []
    for name, _ , _ in entries:
        if name not in names:
            names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    fig, axs = plt.subplots(ncols=num_platforms, nrows=num_platforms, figsize=((size, size)))

    # fig.suptitle('Platform vs. platform comparison')
    if invert:
        fig.supxlabel('Estimated speedup')
        fig.supylabel('Cumulative average accuracy (bits)')
    else:
        fig.supxlabel('Cumulative estimated cost')
        fig.supylabel('Cumulative average eror (bits)')

    # collect input and platform cores
    by_platform: Dict[str, Tuple[List[FPCore], List[FPCore]]] = dict()
    for name, _, info in entries:
        if name not in by_platform:
            input_cores = []
            platform_cores = []
            for core_info in info['cores']:
                input_core = FPCore.from_json(core_info['input_core'])
                cores = list(map(FPCore.from_json, core_info['platform_cores']))
                if len(cores) > 0:
                    input_cores.append(input_core)
                    platform_cores += cores
            by_platform[name] = (input_cores, platform_cores)

    # plot diagonal frontiers
    platform_frontiers = shim_pareto(*map(lambda n: by_platform[n][1], names))
    for name, frontier in zip(names, platform_frontiers):
        input_cores, _ = by_platform[name]
        max_error = sum(map(lambda c: core_max_error(c), input_cores))
        input_costs, input_errs = zip(*map(lambda c: (c.cost, c.err), input_cores))
        input_cost, input_err = sum(input_costs), sum(input_errs)
        input_speedup, input_accuracy = 1.0, max_error - input_err

        # transform (cost, err) -> (speedup, accuracy)
        platform_costs, platform_errs = zip(*frontier)
        platform_speedups = list(map(lambda c: input_cost / c, platform_costs))
        platform_accuracies = list(map(lambda e: max_error - e, platform_errs))

        # plot
        i = names.index(name)
        if invert:
            axs[i, i].plot([input_speedup], [input_accuracy], input_style, color=input_color)
            axs[i, i].plot(platform_speedups, platform_accuracies, platform_style, color=platform_color)
        else:
            axs[i, i].plot([input_cost], [input_err], input_style, color=input_color)
            axs[i, i].plot(platform_costs, platform_errs, platform_style, color=platform_color)

    # plot comparison frontiers
    for name, name2, info in entries:
        input_pt, input_pt2, num_input, \
            platform_frontier, platform_frontier2, num_platform, \
            supported_frontier, supported_frontier2, num_supported, \
            desugared_frontier, desugared_frontier2, num_desugared = comparison_frontiers(info)

        # decompose frontiers
        input_cost, input_err = input_pt
        platform_costs, platform_errs = zip(*platform_frontier)
        supported_costs, supported_errs = zip(*supported_frontier)
        desugared_costs, desugared_errs = zip(*desugared_frontier)

        input_speedup, input_accuracy = input_pt2
        platform_speedups, platform_accuracies = zip(*platform_frontier2)
        supported_speedups, supported_accuracies = zip(*supported_frontier2)
        desugared_speedups, desugared_accuracies = zip(*desugared_frontier2)

        # Pareto frontiers
        ax = axs[names.index(name), names.index(name2)]
        if invert:
            ax.plot([input_speedup], [input_accuracy], input_style, color=input_color)
            ax.plot(platform_speedups, platform_accuracies, platform_style, color=platform_color)
            ax.plot(supported_speedups, supported_accuracies, supported_style, color=supported_color)
            ax.plot(desugared_speedups, desugared_accuracies, desugared_style, color=desugared_color)
        else:
            ax.plot([input_cost], [input_err], input_style, color='black')
            ax.plot(platform_costs, platform_errs, platform_style, color=platform_color)
            ax.plot(supported_costs, supported_errs, supported_style, color=supported_color)
            ax.plot(desugared_costs, desugared_errs, desugared_style, color=desugared_color)

    # set labels
    for i, name in enumerate(names):
        axs[0][i].set(xlabel=name)
        axs[i][len(names) - 1].set(ylabel=name)
        for j, _ in enumerate(names):
            axs[i, j].xaxis.set_label_position('top')
            axs[i, j].yaxis.set_label_position('right')
            axs[i, j].xaxis.set_ticks([])
            axs[i, j].yaxis.set_ticks([])

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
    if improve_reports:
        plot_time_all(output_dir, improve_reports)
        for name, info in improve_reports:
            plot_improve(name, output_dir, info)
            plot_time(name, output_dir, info)

    # Baseline plot
    # if baseline_reports:
    #     plot_baseline_all(output_dir, baseline_reports)
    #     for name, info in baseline_reports:
    #         plot_compare1(name, 'baseline', output_dir, info)

    # Comparison plot
    # if compare_reports:
    #     plot_compare_all(output_dir, compare_reports)
    #     for name, name2, info in compare_reports:
    #         plot_compare1(name, name2, output_dir, info)


if __name__ == "__main__":
    main()
