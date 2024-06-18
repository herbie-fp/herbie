"""Plots cross-benchmark evaluation data"""

import matplotlib.pyplot as plt
from scipy.stats import spearmanr

from argparse import ArgumentParser
from pathlib import Path
import json
import math

from platforms.fpcore import FPCore

# Globals
invert_axes = True # (speedup, accuracy) vs. (cost, error)
use_time = True # time vs cost

# preferred order
order = [
    'arith', 'arith-fma', 'avx',
    'c', 'julia', 'python',
    'vdt', 'fdlibm', 'numpy'
]

# plot extensions
plt_exts = ['png', 'pdf']

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
# Entrypoint

def main():
    parser = ArgumentParser(description='Herbie platforms eval')
    parser.add_argument('output_dir', help='path to evaluation output', type=str)
    args = parser.parse_args()
    output_dir = Path(args.output_dir)

    by_platform = dict()
    for bench_dir in output_dir.iterdir():
        if bench_dir.is_dir():
            json_path = bench_dir.joinpath('results.json')
            with open(json_path, 'r') as f:
                report = json.load(f)
            
            for name, platform_info in report.items():
                platform_info = report[name]
                for field, field_info in platform_info.items():
                    if field == 'improve':
                        if name in by_platform:
                            by_platform[name] = { **by_platform[name], **field_info }
                        else:
                            by_platform[name] = field_info
    
    improve_reports = []
    for name in sorted(report.keys(), key=lambda k: order.index(k)):
        improve_reports.append((name, by_platform[name]))
    plot_time_all(output_dir, improve_reports)


if __name__ == "__main__":
    main()
