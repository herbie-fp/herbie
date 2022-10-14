import sys
import json
import matplotlib
import matplotlib.pyplot as plt

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['savefig.bbox'] = 'tight'

assert (len(sys.argv) == 4)
results_file_vanilla = sys.argv[1]
results_file_egglog = sys.argv[2]
output_plot_error = sys.argv[3]

vanilla_data = json.load(open(results_file_vanilla))["tests"]
egglog_data = json.load(open(results_file_egglog))["tests"]


tests = set(map(lambda row: row["input"], vanilla_data))
assert (len(tests) == len(vanilla_data))
vanilla_tests = dict(map(lambda row: (row["input"], row), vanilla_data))
egglog_tests = dict(map(lambda row: (row["input"], row), egglog_data))

def test_error_diff(test):
  return vanilla_tests[test]["end"] - egglog_tests[test]["end"]

def plot_error():
  tests_sorted = list(tests)
  tests_sorted.sort(key = lambda test: test_error_diff(test))
  print(tests_sorted)

plot_error()
#plt.savefig('egglogreport/error.pdf')


