import numpy as np
import requests
from matplotlib import pyplot as plt, ticker
import matplotlib
import pandas as pd
import json
import argparse

def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["density"]
    outcomes = pd.DataFrame(outcomes, columns=['precision', 'count'])
    return outcomes

def plot_density(args):
    rival = load_outcomes(args.timeline)
    rival['precision'] = np.array(rival['precision'], dtype=float) - np.array(rival['precision'], dtype=float) % 0.05
    rival = rival.groupby(by=['precision'], as_index=False, sort=True).sum()
    
    fig, ax = plt.subplots(figsize=(4, 3))
    
    ax.bar(rival['precision']+0.025, rival["count"], color="red", alpha=0.7, width=0.05, label='reval')
    
    ax.set_ylabel("Number of operations")
    ax.set_xlabel("Precision (normalized)")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)

    print("\\newcommand{\\DensityPercentageOfLowerPrecision}{" + str(round(rival["count"][:4].sum() / rival["count"].sum() * 100, 2)) + "}")
    
    plt.legend()
    plt.tight_layout()
    plt.savefig(args.path + "/density_plot.pdf", format="pdf")
    
    ax.set_title("Density plot")
    plt.tight_layout()
    plt.savefig(args.path + "/density_plot.png", format="png")

parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")

args = parser.parse_args()
matplotlib.rcParams.update({'font.size': 12})
plot_density(args)
