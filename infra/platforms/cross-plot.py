"""Plots cross-benchmark evaluation data"""

import matplotlib.pyplot as plt
from scipy.stats import spearmanr

from typing import List, Tuple
from argparse import ArgumentParser
from pathlib import Path
import json
import math

from platforms.fpcore import FPCore
from platforms.shim import shim_pareto

# Globals
invert_axes = True # (speedup, accuracy) vs. (cost, error)
use_time = True # time vs cost

input_color = 'black'
platform_color = 'blue'
supported_color = 'orange'
desugared_color = 'green'

input_style = 's'
platform_style = '.'
supported_style = '+'
desugared_style = 'x'

# preferred order
order = [
    'arith', 'arith-fma', 'avx',
    'c', 'julia', 'python',
    'vdt', 'fdlibm', 'numpy'
]

# plot extensions
plt_exts = ['png', 'pdf']

######################################
# Utils

def core_max_error(core: FPCore) -> int:
    if core.prec == 'binary64':
        return 64
    elif core.prec == 'binary32':
        return 32
    else:
        raise RuntimeError('Unknown precision', core.prec)
    
def flip_point(input_cost: float, max_error: float, pt: Tuple[float, float]):
    """Transforms `(cost, error)` points into `(speedup, accuracy)` points."""
    cost, error = pt
    return (input_cost / cost, max_error - error)

#######################################
# Cost vs. Time

def platform_cost_time(info):
    costs = []
    times = []
    for input_info in info['cores']:
        for core_info in input_info['platform_cores']:
            core = FPCore.from_json(core_info['platform_core'])
            costs.append(core.cost)
            times.append(core.time)
    return costs, times

def plot_time_all(output_dir: Path, entries):
    print(f'Plotting time for all platforms')
    size = 8

    names = []
    for name, _ in entries:
        names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    nrows = (num_platforms + 2) // 3 # ceil_div(num_platforms, 3)
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))

    time_unit = None
    for _, info in entries:
        if time_unit is None:
            time_unit = info['time_unit']
        elif time_unit != info['time_unit']:
            raise RuntimeError('Time units do not match')

    fig.supxlabel("Estimated cost")
    fig.supylabel(f"Run time ({time_unit})")

    min_rho = math.inf
    max_rho = -math.inf
    for i, (name, info) in enumerate(entries):
        costs, times = platform_cost_time(info)
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        ax.scatter(costs, times)
        ax.set_title(name)

        rho = spearmanr(costs, times).statistic
        if rho < min_rho:
            min_rho = rho
        if rho > max_rho:
            max_rho = rho
   
    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        fig.delaxes(ax)

    plt.tight_layout()
    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'cost-vs-time.{ext}'))
    plt.close()

    print('min_rho:', min_rho)
    print('max_rho:', max_rho)

#######################################
# Baseline

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
    input_costs, input_errs = zip(*map(lambda c: (c.time if use_time else c.cost, c.err), input_cores))
    input_cost, input_error = sum(input_costs), sum(input_errs)
    flip = lambda pt: flip_point(input_cost, max_error, pt)

    # compute (cost, error) frontiers
    platform_frontier, supported_frontier, desugared_frontier = \
        shim_pareto(platform_cores, supported_cores, desugared_cores, use_time=use_time)

    # compute (speedup, accuracy) frontiers
    input_speedup, input_accuracy = flip((input_cost, input_error))
    platform_frontier2 = list(map(flip, platform_frontier))
    supported_frontier2 = list(map(flip, supported_frontier))
    desugared_frontier2 = list(map(flip, desugared_frontier))

    return (input_cost, input_error), (input_speedup, input_accuracy), num_input, \
        platform_frontier, platform_frontier2, num_platform, \
        supported_frontier, supported_frontier2, num_supported, \
        desugared_frontier, desugared_frontier2, num_desugared

def plot_baseline_all(output_dir: Path, entries):
    """Entire baseline comparison (N)."""
    print(f'Plotting all baseline comparison')
    size = 8

    names = []
    for name, _ in entries:
        names.append(name)

    names = sorted(names)
    num_platforms = len(names)
    nrows = (num_platforms + 2) // 3 # ceil_div(num_platforms, 3)
    fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))

    if invert_axes:
        fig.supxlabel('Speedup' if use_time else 'Estimated speedup')
        fig.supylabel('Cumulative average accuracy (bits)')
    else:
        fig.supxlabel('Cumulative run time' if use_time else 'Cumulative estimated cost')
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

        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        ax.set_title(name, size='medium')
        if invert_axes:
            ax.plot([input_speedup], [input_accuracy], input_style, color=input_color)
            ax.plot(platform_speedups, platform_accuracies, platform_style, color=platform_color)
            ax.plot(supported_speedups, supported_accuracies, supported_style, color=supported_color)
            ax.plot(desugared_speedups, desugared_accuracies, desugared_style, color=desugared_color)
        else:
            ax.plot([input_cost], [input_err], input_style, color=input_color)
            ax.plot(platform_costs, platform_errs, platform_style, color=platform_color)
            ax.plot(supported_costs, supported_errs, supported_style, color=supported_color)
            ax.plot(desugared_costs, desugared_errs, desugared_style, color=desugared_color)

    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        fig.delaxes(ax)

    plt.tight_layout()
    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'baseline-pareto.{ext}'))
    plt.close()

#######################################
# Entrypoint

def main():
    parser = ArgumentParser(description='Herbie platforms eval')
    parser.add_argument('output_dir', help='path to evaluation output', type=str)
    args = parser.parse_args()
    output_dir = Path(args.output_dir)

    improve_by_platform = dict()
    baseline_by_platform = dict()
    for bench_dir in output_dir.iterdir():
        if bench_dir.is_dir():
            json_path = bench_dir.joinpath('results.json')
            with open(json_path, 'r') as f:
                report = json.load(f)
            
            for name, platform_info in report.items():
                platform_info = report[name]
                for field, field_info in platform_info.items():
                    if field == 'improve':
                        if name in improve_by_platform:
                            improve_by_platform[name] = { **improve_by_platform[name], **field_info }
                        else:
                            improve_by_platform[name] = field_info

                    elif field == 'compare':
                        for name2, compare_info in field_info.items():
                            if name2 == 'baseline':
                                if name in baseline_by_platform:
                                    baseline_by_platform[name] = { **baseline_by_platform[name], **compare_info }
                                else:
                                    baseline_by_platform[name] = compare_info
                                pass
    
    improve_reports = []
    baseline_reports = []
    for name in sorted(report.keys(), key=lambda k: order.index(k)):
        improve_reports.append((name, improve_by_platform[name]))
        baseline_reports.append((name, baseline_by_platform[name]))
        
    plot_time_all(output_dir, improve_reports)
    plot_baseline_all(output_dir, baseline_reports)


if __name__ == "__main__":
    main()
