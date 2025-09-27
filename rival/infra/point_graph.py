import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_points_graph(outcomes):
    fig, ax = plt.subplots(figsize=(4, 3.5))

    series_labels = ['zero', 'non-zero']
    colors = ['tab:blue',  'tab:orange', 'tab:green']

    category_labels = ['Reval', 'Sollya', 'Baseline']

    data = np.zeros((len(series_labels), len(category_labels)))

    # Zeros difference
    # Rival
    data[0][0] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-zero']['number_of_points'].sum()
    data[0][0] += outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-zero']['number_of_points'].sum()
    data[0][0] += outcomes.loc[outcomes['tool_name'] == 'valid-rival-only-zero']['number_of_points'].sum()

    # Sollya
    data[0][1] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-zero']['number_of_points'].sum()
    data[0][1] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya+baseline-zero']['number_of_points'].sum()
    data[0][1] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya-only-zero']['number_of_points'].sum()

    # Baseline
    data[0][2] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-zero']['number_of_points'].sum()
    data[0][2] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya+baseline-zero']['number_of_points'].sum()
    data[0][2] += outcomes.loc[outcomes['tool_name'] == 'valid-baseline-only-zero']['number_of_points'].sum()

    # Real number difference
    # Rival
    data[1][0] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-real']['number_of_points'].sum()
    data[1][0] += outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-real']['number_of_points'].sum()
    data[1][0] += outcomes.loc[outcomes['tool_name'] == 'valid-rival-only-real']['number_of_points'].sum()

    # Sollya
    data[1][1] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-real']['number_of_points'].sum()
    data[1][1] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya+baseline-real']['number_of_points'].sum()
    data[1][1] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya-only-real']['number_of_points'].sum()

    # Baseline
    data[1][2] = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-real']['number_of_points'].sum()
    data[1][2] += outcomes.loc[outcomes['tool_name'] == 'valid-sollya+baseline-real']['number_of_points'].sum()
    data[1][2] += outcomes.loc[outcomes['tool_name'] == 'valid-baseline-only-real']['number_of_points'].sum()

    infinite_difference = outcomes.loc[outcomes['tool_name'] == 'valid-rival+baseline-inf']['number_of_points'].sum()
    infinite_difference += outcomes.loc[outcomes['tool_name'] == 'valid-rival+sollya-inf']['number_of_points'].sum()
    infinite_difference += outcomes.loc[outcomes['tool_name'] == 'valid-rival-only-inf']['number_of_points'].sum()

    sollya_faithful_cnt = outcomes.loc[outcomes['tool_name'] == 'sollya-faithful-rounding']['number_of_points'].sum()
    sollya_correct_cnt = outcomes.loc[outcomes['tool_name'] == 'sollya-correct-rounding']['number_of_points'].sum()

    sollya_exit_times = outcomes.loc[outcomes['tool_name'] == 'exit-sollya']['time'].sum()/1000
    sollya_exit_cnt = outcomes.loc[outcomes['tool_name'] == 'exit-sollya']['number_of_points'].sum()

    rival_exit_times = outcomes.loc[outcomes['tool_name'] == 'exit-rival']['time'].sum()/1000
    rival_exit_cnt = outcomes.loc[outcomes['tool_name'] == 'exit-rival']['number_of_points'].sum()

    base_exit_times = outcomes.loc[outcomes['tool_name'] == 'exit-baseline']['time'].sum()/1000
    base_exit_cnt = outcomes.loc[outcomes['tool_name'] == 'exit-baseline']['number_of_points'].sum()

    print("\\newcommand{\\SollyaExitTime}{" + str(round(sollya_exit_times, 2)) + "\\xspace}")
    print("\\newcommand{\\RivalExitTime}{" + str(round(rival_exit_times, 2)) + "\\xspace}")
    print("\\newcommand{\\BaselineExitTime}{" + str(round(base_exit_times, 2)) + "\\xspace}")
    print("\\newcommand{\\RivalExitTimetoSollya}{" + str(round(sollya_exit_times/rival_exit_times, 2)) + "\\xspace}")
    print("\\newcommand{\\RivalExitTimetoBaseline}{" + str(round(base_exit_times / rival_exit_times, 2)) + "\\xspace}")

    print("\\newcommand{\\SollyaExitCnt}{" + str(sollya_exit_cnt) + "\\xspace}")
    print("\\newcommand{\\RivalExitCnt}{" + str(rival_exit_cnt) + "\\xspace}")
    print("\\newcommand{\\BaselineExitCnt}{" + str(base_exit_cnt) + "\\xspace}")

    print("\\newcommand{\\CorrecttoFaithfulSollya}{" + str(round(100*sollya_correct_cnt/(sollya_faithful_cnt+sollya_correct_cnt), 2)) + "\\xspace}")
    print("\\newcommand{\\SollyaFaithfulCnt}{" + str(sollya_faithful_cnt) + "\\xspace}")
    print("\\newcommand{\\SamplingInfiniteDifference}{" + str(infinite_difference) + "\\xspace}")
    print("\\newcommand{\\RivalZeros}{" + str(int(data[0][0])) + "\\xspace}")
    print("\\newcommand{\\BaselineZeros}{" + str(int(data[0][2])) + "\\xspace}")
    print("\\newcommand{\\RivalNonZero}{" + str(int(data[1][0])) + "\\xspace}")
    print("\\newcommand{\\BaselineNonZero}{" + str(int(data[1][2])) + "\\xspace}")
    print("\\newcommand{\\SollyaZeros}{" + str(int(data[0][1])) + "\\xspace}")
    print("\\newcommand{\\SollyaNonZero}{" + str(int(data[1][1])) + "\\xspace}")

    # Plotting top part of the bar
    bottom = np.zeros(len(category_labels))
    for label, weight, color in zip(series_labels, data, colors):
        ax.bar(np.arange(len(category_labels)), weight, label=label, bottom=bottom, color=color)
        bottom += weight
        
    ax.set_xlabel("Tool")
    ax.set_ylabel("Number of points")
    fig.tight_layout()
     
    y_offset = 80
    for i, total in enumerate(bottom):
        ax.text(i, total + y_offset, str(int(total)), ha='center', color='black')

    for bar in ax.patches:
        if bar.get_height() > 300:
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                bar.get_height() + bar.get_y() - bar.get_height()/2 - 200,
                int(bar.get_height()),
                ha='center',
                color='black')

    ax.set_xticks(np.arange(len(category_labels)), category_labels)
    ax.legend(loc='upper center')
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    
    plt.savefig(args.path + "/point_graph.png", format="png")
    plt.savefig(args.path + "/point_graph.pdf", format="pdf")


def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'baseline_precision', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='point_graph.py', description='Script outputs plots for a Herbie run')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)
plot_points_graph(outcomes)

