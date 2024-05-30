from pathlib import Path

import json
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import os

localize_new_cost = os.getcwd() + '\\localize_new_cost\\results.json'
localize_old_cost = os.getcwd() + '\\localize_old_cost\\results.json'
no_localize_new_cost = os.getcwd() + '\\no_localize_new_cost\\results.json'
no_localize_old_cost = os.getcwd() + '\\no_localize_old_cost\\results.json'


plt.figure()
#plt.gca().yaxis.set_major_formatter(ticker.PercentFormatter(xmax=100))
# plt.ylim(0, 100)
with open(localize_new_cost, 'r') as f:
        report = json.load(f)
        cost_accuracy = report['merged-cost-accuracy'][1]
        cost, accuracy = zip(*cost_accuracy)
        plt.plot(report['merged-cost-accuracy'][0][0], report['merged-cost-accuracy'][0][1], 'rs')
        plt.plot(cost, accuracy, label='localize_new_cost')
    
with open(localize_old_cost, 'r') as f:
        report = json.load(f)
        cost_accuracy = report['merged-cost-accuracy'][1]
        cost, accuracy = zip(*cost_accuracy)
        plt.plot(report['merged-cost-accuracy'][0][0], report['merged-cost-accuracy'][0][1], 'rs')
        plt.plot(cost, accuracy, label='localize_old_cost')

with open(no_localize_new_cost, 'r') as f:
        report = json.load(f)
        cost_accuracy = report['merged-cost-accuracy'][1]
        cost, accuracy = zip(*cost_accuracy)
        plt.plot(report['merged-cost-accuracy'][0][0], report['merged-cost-accuracy'][0][1], 'rs')
        plt.plot(cost, accuracy, label='no_localize_new_cost')

with open(no_localize_old_cost, 'r') as f:
        report = json.load(f)
        cost_accuracy = report['merged-cost-accuracy'][1]
        cost, accuracy = zip(*cost_accuracy)
        plt.plot(report['merged-cost-accuracy'][0][0], report['merged-cost-accuracy'][0][1], 'rs')
        plt.plot(cost, accuracy, label='no_localize_old_cost')

plt.legend()
plt.show()  