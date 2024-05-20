"""Plots evaluation data"""

import argparse
import json
import os

import matplotlib.pyplot as plt
from pathlib import Path

# Paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)


def plot_tune(name: str, output_dir: Path, info: dict):
    """Plots Herbie cost estimate vs actual run time"""
    print(f'Plotting tune {name}')

    costs = []
    times = []
    for _, (cost, time) in info.items():
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

    frontier1_costs, frontier1_errors = zip(*info["frontier1"])
    frontier2_costs, frontier2_errors = zip(*info["frontier2"])

    plt.plot(frontier1_costs, frontier1_errors, label=name)
    plt.plot(frontier2_costs, frontier2_errors, label=name2)
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
            if field == 'tune':
                plot_tune(name, output_dir, field_info)
            elif field == 'improve':
                plot_improve(name, output_dir, field_info)
            elif field == 'compare':
                for name2, compare_info in field_info.items():
                    plot_compare(name, name2, output_dir, compare_info)


if __name__ == "__main__":
    main()
