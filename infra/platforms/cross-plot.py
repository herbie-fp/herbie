"""Plots cross-benchmark evaluation data"""

import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
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
platform_color = '#0072B2'
supported_color = '#009E73'
desugared_color = '#D55E00'

input_style = 's'
platform_style = '.'
supported_style = '2'
desugared_style = '_'

# preferred order
order = [
    'arith', 'arith-fma', 'avx',
    'c', 'julia', 'python',
    'vdt', 'fdlibm', 'numpy'
]

# preferred name
display_names = {
    'arith': 'Arith',
    'arith-fma': 'Arith+FMA',
    'avx': 'AVX',
    'c': 'C',
    'julia': 'Julia',
    'python': 'Python',
    'vdt': 'vdt',
    'fdlibm': 'fdlibm',
    'numpy': 'NumPy'
}

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
        ax.scatter(costs, times, color=platform_color)
        ax.set_title(display_names[name])

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

def comparison_frontiers2(info):
    input_cores: List[FPCore] = []
    platform_cores: List[FPCore] = []
    supported_cores: List[FPCore] = []
    desugared_cores: List[FPCore] = []
    for core_info in info['cores']:
        input_core = FPCore.from_json(core_info['input_core'])
        platform = list(map(FPCore.from_json, core_info['platform_cores']))
        supported = list(map(FPCore.from_json, core_info['supported_cores']))
        desugared = list(map(FPCore.from_json, core_info['desugared_cores']))

        if platform and supported and desugared:
            input_cores.append(input_core)
            platform_cores += platform
            supported_cores += supported
            desugared_cores += desugared

    # compute starting point
    max_error = sum(map(lambda c: core_max_error(c), input_cores))
    input_times, input_errs = zip(*map(lambda c: (c.time, c.err), input_cores))
    input_time, input_acc = sum(input_times), max_error - sum(input_errs)

    # compute (time, error) frontiers
    platform_frontier, supported_frontier, desugared_frontier = \
        shim_pareto(platform_cores, supported_cores, desugared_cores, use_time=use_time)

    # compute (time, accuracy) frontiers
    platform_frontier = list(map(lambda pt: (pt[0], max_error - pt[1]), platform_frontier))
    supported_frontier = list(map(lambda pt: (pt[0], max_error - pt[1]), supported_frontier))
    desugared_frontier = list(map(lambda pt: (pt[0], max_error - pt[1]), desugared_frontier))
    return (input_time, input_acc), platform_frontier, supported_frontier, desugared_frontier

def normalize(pts: List[Tuple[float, float]], pts2: List[Tuple[float, float]]):
    norm_pts = []
    for pt in pts:
        before = max(filter(lambda pt2: pt2[0] <= pt[0], pts2), key=lambda pt: pt[0], default=None)
        after = min(filter(lambda pt2: pt2[0] >= pt[0], pts2), key=lambda pt: pt[0], default=None)
        if before is None:
            # at or below minimium x
            # apply linear interpolation
            pt1, pt2 = pts2[0], pts2[1]
            m = (pt2[1] - pt1[1]) / (pt2[0] - pt1[0])
            y = m * (pt[0] - pt1[0]) + pt1[1]
            norm_pts.append((pt[0], y / pt[1]))
        elif after is None:
            # at or above maximum x
            # applying interpolation is insane since the line is exponential
            # pt1, pt2 = pts2[-2], pts2[-1]
            # m = (pt2[1] - pt1[1]) / (pt2[0] - pt1[0])
            # y = m * (pt[0] - pt2[0]) + pt2[1]
            # print(pt2, m, y, pt[1])
            # norm_pts.append((pt[0], y / pt[1]))
            pass
        elif before == after:
            # x is the same
            norm_pts.append((pt[0],  before[1] / pt[1]))
        else:
            # x is not the same
            # apply linear interpolation
            m = (before[1] - after[1]) / (before[0] - after[0])
            y = m * (pt[0] - before[0]) + before[1]
            norm_pts.append((pt[0], y / pt[1]))

    return norm_pts

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
        fig.supylabel('Sum of accuracy log2(ULP)')
    else:
        fig.supxlabel('Cumulative run time' if use_time else 'Cumulative estimated cost')
        fig.supylabel('Sum of eror log2(ULP)')

    for i, (name, info) in enumerate(entries):
        input_pt, input_pt2, num_input, \
            platform_frontier, platform_frontier2, num_platform, \
            supported_frontier, supported_frontier2, num_supported, \
            desugared_frontier, desugared_frontier2, num_desugared = comparison_frontiers(info)

        # decompose frontiers
        if invert_axes:
            input_x, input_y = input_pt2
            platform_xs, platform_ys = zip(*platform_frontier2)
            supported_xs, supported_ys = zip(*supported_frontier2)
            desugared_xs, desugared_ys = zip(*desugared_frontier2)
        else:
            input_x, input_y = input_pt
            platform_xs, platform_ys = zip(*platform_frontier)
            supported_xs, supported_ys = zip(*supported_frontier)
            desugared_xs, desugared_ys = zip(*desugared_frontier)

        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        ax.set_title(display_names[name], size='medium')
        ax.plot([input_x], [input_y], input_style, color=input_color)
        ax.plot(platform_xs, platform_ys, platform_style, color=platform_color)
        ax.plot(supported_xs, supported_ys, supported_style, color=supported_color)
        ax.plot(desugared_xs, desugared_ys, desugared_style, color=desugared_color)

    for i in range(len(names), 3 * nrows):
        ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
        fig.delaxes(ax)

    plt.tight_layout()
    for ext in plt_exts:
        plt.savefig(output_dir.joinpath(f'baseline-pareto.{ext}'))
    plt.close()

    # second mode
    if invert_axes and use_time:
        fig, axs = plt.subplots(ncols=3, nrows=nrows, figsize=((size, size)))
        fig.supxlabel('Sum of accuracy log2(ULP)')
        fig.supylabel('Speedup')
    
        for i, (name, info) in enumerate(entries):
            input_pt, platform_frontier, supported_frontier, desugared_frontier = comparison_frontiers2(info)
            
            # flip frontiers (x, y) -> (y, x)
            input_pt = (input_pt[1], input_pt[0])
            platform_frontier = list(map(lambda pt: (pt[1], pt[0]), platform_frontier))
            supported_frontier = list(map(lambda pt: (pt[1], pt[0]), supported_frontier))
            desugared_frontier = list(map(lambda pt: (pt[1], pt[0]), desugared_frontier))
            
            # sort frontiers by y
            platform_frontier.sort(key=lambda pt: pt[0])
            supported_frontier.sort(key=lambda pt: pt[0])
            desugared_frontier.sort(key=lambda pt: pt[0])

            # platform_max = max(map(lambda pt: pt[0], platform_frontier))
            # desugared_max = max(map(lambda pt: pt[0], desugared_frontier))

            relative_frontier = desugared_frontier
            input_pt = normalize([input_pt], relative_frontier)[0]
            platform_frontier = normalize(platform_frontier, relative_frontier)
            supported_frontier = normalize(supported_frontier, relative_frontier)
            desugared_frontier = normalize(desugared_frontier, relative_frontier)

            # decompose frontiers
            input_x, input_y = input_pt
            platform_xs, platform_ys = zip(*platform_frontier)
            supported_xs, supported_ys = zip(*supported_frontier)
            desugared_xs, desugared_ys = zip(*desugared_frontier)

            # plot
            ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
            ax.set_title(display_names[name], size='medium')
            ax.plot([input_x], [input_y], input_style, color=input_color)
            ax.plot(platform_xs, platform_ys, platform_style, color=platform_color)
            ax.plot(supported_xs, supported_ys, supported_style, color=supported_color, mfc='none')
            ax.plot(desugared_xs, desugared_ys, desugared_style, color=desugared_color)

            # y-axis formatting
            ax.yaxis.set_major_formatter(FormatStrFormatter('%.1f'))
            ymax = max(map(lambda pt: pt[1], platform_frontier))
            print(name, ymax)
            if ymax > 2:
                ax.set(ylim=(0, 4))
            elif ymax > 1.25:
                ax.set(ylim=(0, 2))
            else:
                ax.set(ylim=(0, 1.5))

            # x-axis formatting
            # WARN: hard coded
            # if name in ['arith', 'arith-fma', 'avx']:
            #     ax.set(xlim=(500, 1000))
            # else:
            #     ax.set(xlim=(1350, 2800))

            # xmin = min(map(lambda pt: pt[0], platform_frontier + supported_frontier + desugared_frontier))
            # xmax = max(map(lambda pt: pt[0], platform_frontier + supported_frontier + desugared_frontier))
            # print(name, xmin, xmax)

            #ax.hlines(y=1, xmin=xmin, xmax=xmax, color='gray', linestyle='--')

        for i in range(len(names), 3 * nrows):
            ax = axs[i // 3, i % 3] if num_platforms > 3 else axs[i]
            fig.delaxes(ax)
        
        plt.tight_layout()
        for ext in plt_exts:
            plt.savefig(output_dir.joinpath(f'baseline-pareto2.{ext}'))
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
