import sys
import json
import matplotlib
import matplotlib.pyplot as plt

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['savefig.bbox'] = 'tight'

assert (len(sys.argv) == 6)
results_file_vanilla = sys.argv[1]
results_file_egglog = sys.argv[2]
output_plot_error = sys.argv[3]
macro_file = sys.argv[4]
output_plot_hist = sys.argv[5]

vanilla_data = json.load(open(results_file_vanilla))["tests"]
egglog_data = json.load(open(results_file_egglog))["tests"]


tests_unfiltered = set(map(lambda row: row["name"], vanilla_data))
assert (len(tests_unfiltered) == len(vanilla_data))
assert (len(vanilla_data) == len(egglog_data))

vanilla_tests = dict(map(lambda row: (row["name"], row), vanilla_data))
egglog_tests = dict(map(lambda row: (row["name"], row), egglog_data))


tests = set(filter(lambda test: vanilla_tests[test]["start"] > 0.5, tests_unfiltered))


def test_error_diff(test):
  return egglog_tests[test]["end"] - vanilla_tests[test]["end"]

def plot_error():
  tests_sorted = list(tests)
  tests_sorted.sort(key = lambda test: test_error_diff(test))
  for test in tests_sorted:
   print(egglog_tests[test]["name"])
  
  xs = range(len(tests_sorted))
  ys = list(map(lambda test: test_error_diff(test), tests_sorted))

  fig = plt.figure()
  ax = fig.add_subplot()
  ax.scatter(xs, ys, color = "black", marker = "o", linestyle = "None", facecolors='none')
  plt.savefig(output_plot_error)  

HIST_CUTOFF = 60

def makecdf(data):
  copy = data.copy()
  copy.sort()
  cdfdata = []
  sum = 0
  xs = []
  for val in copy:
    xs.append(val)
    cdfdata.append(sum)
    sum += 1
    
  return (xs, cdfdata)


def histogram_error():
  fig = plt.figure()
  errors = list(map(lambda test: test_error_diff(test), tests))
  error_filtered = list(filter(lambda error: error < HIST_CUTOFF and abs(error) > 1, errors))
  bins = list(range(-HIST_CUTOFF-1, HIST_CUTOFF+1))
  bins_filtered = list(filter(lambda bin: bin % 2 == 1, bins))

  plt.hist(error_filtered, bins = bins_filtered, color = "blue", alpha = 0.5)
  plt.savefig(output_plot_hist)  

def cdf_error():
  fig = plt.figure()
  errors = list(map(lambda test: test_error_diff(test), tests))
  error_filtered = list(filter(lambda error: error < HIST_CUTOFF, errors))

  (xs, ys) = makecdf(error_filtered)
  plt.plot(xs, ys)
  plt.savefig(output_plot_hist)  

plot_error()
histogram_error()

#plt.savefig('egglogreport/error.pdf')

macro_port = open(macro_file, "w")

def save_macro(name, val):
  macro_port.write("\\newcommand{\\%s}{%s\\xspace}\n" % (name, val))

def save_macro_round(name, val):
  save_macro(name, round(val, 2))

def save_macro_percent(name, val):
  save_macro(name, str(round((val * 100.0), 2)) + "\\%")

def generate_macros():
  num_tests = len(tests)
  save_macro("numtests", num_tests)
  num_better_error = len(list(filter(lambda test: test_error_diff(test) < 0, tests)))
  save_macro("numbettererror", num_better_error)
  num_worse_error = len(list(filter(lambda test: test_error_diff(test) > 0, tests)))
  save_macro("numworseerror", num_worse_error)

  save_macro("numwithinone", len(list(filter(lambda test: abs(test_error_diff(test)) <= 1, tests))))
  save_macro("numgreaterone", len(list(filter(lambda test: abs(test_error_diff(test)) > 1, tests))))
  save_macro("numlessnegone", len(list(filter(lambda test: test_error_diff(test) < -1, tests))))

  save_macro_round("timeegglogminutes", sum(map(lambda test: egglog_tests[test]["time"], tests)) / (1000 * 60.0))
  save_macro_round("timevanillaminutes", sum(map(lambda test: vanilla_tests[test]["time"], tests)) / (1000.0 * 60.0))
  save_macro_percent("besttimepercentofvanilla", min(map(lambda test: (egglog_tests[test]["time"] / vanilla_tests[test]["time"]), tests)))

  save_macro("numhistcutoff", len(list(filter(lambda test: test_error_diff(test) > HIST_CUTOFF, tests))))

  
generate_macros()
