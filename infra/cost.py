import os, io, shutil
import re, math
import subprocess
import argparse

import matplotlib.pyplot as plt
import multiprocessing as mp
import plotly.express as px

default_num_runs = 10000
output_file = 'output.c'
racket_file = 'cost.rkt'

fpcore_pat = re.compile('// \(FPCore (.*)')
comment_pat = re.compile('// ')
cost_pat = re.compile('// cost: ([0-9.]*)')
proto_pat = re.compile('(double|float) ([a-zA-Z0-9_]+)\(([a-zA-Z0-9_ ,]*)\)')
func_pat = re.compile('([a-zA-Z0-9_]+)_32_variant_32_([0-9]+)')
time_pat = re.compile('([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) us')

def double_to_c_str(f: float):
    if math.isnan(f):
        return 'NAN'
    elif math.isinf(f):
        return '-INFINITY' if f < 0 else 'INFINITY'
    else:
        return str(f)

class Core(object):
    def __init__(self, name, key, core, output, cost, argc, type):
        self.name = name
        self.key = key
        self.core = core
        self.output = output
        self.cost = cost
        self.argc = argc
        self.type = type

    def __repr__(self):
        return 'Core(' + \
            'name=' + repr(self.name) + ', ' + \
            'key=' + repr(self.key) + ', ' + \
            'core=' + repr(self.core) + ', ' + \
            'cost=' + repr(self.cost) + ', ' + \
            'argc=' + repr(self.argc) + ', ' + \
            'type=' + repr(self.type) + \
            ')'

def read_core(f: io.TextIOWrapper):
    props = {}
    core = None
    line = f.readline()
    while line != '':
        fpcore = re.match(fpcore_pat, line)
        if fpcore is not None:
            # found an fpcore
            lines = [line]
            line = f.readline()
            while line != '' and line != '// ---\n':
                lines.append(line)
                line = f.readline()
            for i, _ in enumerate(lines):
                lines[i] = lines[i].replace('//', '').replace('\n', '').strip()
            props['core'] = '\n'.join(lines)
        else:
            cost = re.match(cost_pat, line)
            if cost is not None:
                # found a cost
                props['cost'] = cost.group(1)
            else:
                proto = re.match(proto_pat, line)
                if proto is not None:
                    # found a function, so we should make a driver for it
                    func_name = proto.group(2)    # function name
                    arg_str = proto.group(3)      # function arguments
                    scalar_type = proto.group(1)  # function type (assuming uniform)
                    # hacky argument counting
                    argc = 0 if len(arg_str) == 0 else len(arg_str.split(','))
                    # get sample key
                    func = re.match(func_pat, func_name)
                    assert func is not None
                    sample_key = func.group(1)
                    # make core
                    core = props.get('core')
                    cost = float(props.get('cost')) if 'cost' in props else None
                    # load function
                    line = f.readline()
                    lines = []
                    while line != '}\n':
                        lines.append(line)
                        line = f.readline()
                    return Core(func_name, sample_key, core, ''.join(lines), cost, argc, scalar_type)
        line = f.readline()
    return core

def run(bench_dir, output_dir, args):
    if len(args) == 0:
        print(f'Running tests in \'{output_dir}\' with default flags')
    else:
        arg_str = ' '.join(args)
        print(f'Running tests in \'{output_dir}\' with \'{arg_str}\'')

    output_name = os.path.join(output_dir, output_file)
    if os.path.exists(output_dir):
        shutil.rmtree(output_dir)

    # run Herbie to produce a C file
    os.mkdir(output_dir)
    subprocess.run(
        ['racket', '-y', 'src/herbie.rkt', 'improve', '--lang', 'c'] + 
        args + 
        [bench_dir, output_name]
    )

    
def sample_core(core: Core, n: int):
    script_path = os.path.abspath(__file__)
    script_dir, _ = os.path.split(script_path)
    sample_path = os.path.join(script_dir, racket_file)
    core_str = core.core.replace('\"', '\\\"').strip()

    p = subprocess.Popen(['racket', sample_path, 'sample', str(n), f'\"{core_str}\"'], stdout=subprocess.PIPE)
    stdout, _ = p.communicate()
    output = stdout.decode('utf-8')

    inputs = []
    for line in output.split('\n'):
        if len(line) > 0:
            inputs.append(list(map(float, line.split(' '))))

    assert n == len(inputs)
    return inputs

def write_driver(driver_name: str, core: Core, sample: list[list]):
    by_vars = [list() for _ in range(core.argc)]
    num_inputs = len(sample)

    with open(driver_name, 'a') as f:
        for i, sample in enumerate(by_vars):
            print(f'const {core.type} x{i}[{num_inputs}] = {{', file=f)
            print(',\n'.join(map(double_to_c_str, sample)), file=f)
            print('};', file=f)
        
        # print('#include <stdio.h>', file=f)
        # print('#include <time.h>', file=f)
        print('#include <chrono>', file=f)
        print('#include <iostream>', file=f)

        print('int main() {', file=f)
        # print('struct timespec start, end;', file=f)
        print(f'volatile {core.type} res;', file=f)
        print(f'auto start = std::chrono::high_resolution_clock::now();', file=f)
        # print('clock_gettime(CLOCK_MONOTONIC, &start);', file=f)

        print(f'for (long i = 0; i < {num_inputs}; i++) {{', file=f)
        arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
        print(f'  res = {core.name}({arg_str});', file=f)
        print('}', file=f)

        print(f'auto end = std::chrono::high_resolution_clock::now();', file=f)
        print(f'auto diff = end - start;', file=f)
        print(f'std::cout << (static_cast<double>(diff.count()) / 1000.0) << " us";', file=f)

        # print('clock_gettime(CLOCK_MONOTONIC, &end);', file=f)
        # print('printf("%.6f us", (end.tv_nsec - start.tv_nsec) / 1000.0);', file=f)
        print('  return 0;', file=f)
        print('}', file=f)

def make_driver(config):
    output_dir, core, sample = config
    output_name = os.path.join(output_dir, output_file)
    driver_dir = os.path.join(output_dir, core.name)
    # make directory for file
    if os.path.exists(driver_dir):
        shutil.rmtree(driver_dir)
    os.mkdir(driver_dir)
    # copy file (manual copy since we have to repair lines)
    driver_name = os.path.join(driver_dir, f'{core.name}.cpp')
    # amend functions to be inline
    with open(output_name, 'r') as src:
        with open(driver_name, 'w') as dst:
            for line in src.readlines():
                proto = re.match(proto_pat, line)
                if proto is not None:
                    print(f'inline {line}', file=dst, end='')
                else:
                    print(line, file=dst, end='')
    # write driver
    print(f'Generating driver for \'{core.name}\'')
    write_driver(driver_name, core, sample)
    return driver_name

def compile_driver(driver_name):
    driver_dir, fname = os.path.split(driver_name)
    exe_name = os.path.join(driver_dir, fname.removesuffix('.cpp'))
    print(f'Compiling \'{fname}\' ==> \'{os.path.split(exe_name)[1]}\'')
    p = subprocess.Popen(['c++', '-std=c++11', '-O0', driver_name, '-o', exe_name])
    # p = subprocess.Popen(['cc', '-std=c11', '-O3', driver_name, '-o', exe_name, '-lm'])
    _, _ = p.communicate()

def generate_sample(config):
    core, num_runs = config
    print(f'Sampling \'{core.key}\'')
    return sample_core(core, num_runs)

def make_drivers(output_dir: str, cores: list[Core], num_runs: int, threads: int = 1):
    with mp.Pool(processes=threads) as p:
        # group cores by sample key
        by_sample = {}
        for core in cores:
            if core.key in by_sample:
                by_sample[core.key].append(core)
            else:
                by_sample[core.key] = [core]
        # extract representative from each group
        representatives = []
        for in_sample in by_sample.values():
            representatives.append(in_sample[0])
        # generate samples
        samples = p.map(generate_sample, map(lambda c : (c, num_runs), representatives))
        by_sample = {}
        for representative, sample in zip(representatives, samples):
            by_sample[representative.key] = sample
        # create drivers for each core
        drivers = p.map(make_driver, map(lambda c : (output_dir, c, by_sample[c.key]), cores))
        # compile each driver
        p.map(compile_driver, drivers)

def run_drivers(output_dir, cores):
    # run each driver
    times = []
    for core in cores:
        driver_dir = os.path.join(output_dir, core.name)
        exe_path = os.path.join(driver_dir, f'{core.name}')
        print(f'Running \'{os.path.split(exe_path)[1]}\'')
        p = subprocess.Popen([exe_path], stdout=subprocess.PIPE)
        stdout, _ = p.communicate()
        output = stdout.decode('utf-8')
        print(output)

        match = re.search(time_pat, output)
        if match is None:
            raise ValueError('Unexpected output')
        time = match.group(1)
        times.append(float(time))

    return times


def plot_cost_time(cores: list[Core], costs, times, num_runs):
    # plt.scatter(costs, times)
    # plt.title(f'Synthetic cost vs. run time ({num_runs} runs)')
    # plt.xlabel('Cost')
    # plt.ylabel('Time (us)')
    # plt.show()

    data = { 'x': costs, 'y': times, 'expr': map(lambda c: c.output, cores) }
    fig = px.scatter(data, x='x', y='y', hover_name='expr')
    fig.update_layout(
        title=f'Synthetic cost vs. run time ({num_runs} runs)',
        xaxis_title='Cost',
        yaxis_title='Time (us)',
        hovermode='closest'
    )

    fig.show()


def main():
    parser = argparse.ArgumentParser(description='Herbie C-code generator')
    parser.add_argument('--num-inputs', help='number of inputs to evaluate run time', type=int)
    parser.add_argument('--threads', help='number of threads for driver compilation', type=int)
    parser.add_argument('--recompile', help='recompile existing Herbie output and plot results', action='store_const', const=True, default=False)
    parser.add_argument('--plot-only', help='only plot results', action='store_const', const=True, default=False)
    parser.add_argument('bench_dir', help='directory of benchmarks', type=str)
    parser.add_argument('output_dir', help='output directory', type=str)
    parser.add_argument('herbie_flags', help='Herbie command-line flags', type=str)
    
    namespace = parser.parse_args()
    args = vars(namespace)

    bench_dir = os.path.join(os.getcwd(), args['bench_dir'])
    output_dir = os.path.join(os.getcwd(), args['output_dir'])
    num_runs = default_num_runs if args['num_inputs'] is None else args['num_inputs']
    threads = 1 if args['threads'] is None else args['threads']
    recompile = args['recompile']
    plot_only = args['plot_only']
    args = args['herbie_flags'].split(' ')
    
    if not recompile and not plot_only:
        # run Herbie
        run(bench_dir=bench_dir, output_dir=output_dir, args=args)

    # load cores
    cores: list[Core] = []
    with open(os.path.join(output_dir, output_file), 'r') as f:
        core = read_core(f)
        while core is not None:
            cores.append(core)
            core = read_core(f)

    # create drivers and run tests
    if not plot_only:
        make_drivers(output_dir=output_dir, cores=cores, num_runs=num_runs, threads=threads)

    # run drivers
    times = run_drivers(output_dir=output_dir, cores=cores)
    # extract synthetic costs
    costs = list(map(lambda c : c.cost, cores))
    # plot cost vs. time
    plot_cost_time(cores, costs, times, num_runs)


if __name__ == "__main__":
    main()

