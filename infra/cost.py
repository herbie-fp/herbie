import os, io
import shutil
import subprocess
import re
import ctypes
import random
import math
import argparse
import matplotlib.pyplot as plt

output_file = 'output.c'
racket_file = 'cost.rkt'

def bv64_to_double(i: int):
    if i < 0 or i >= 2 ** 64:
        raise ValueError('out of bounds', i)
    return ctypes.c_double.from_buffer(ctypes.c_uint64(i)).value

def double_to_c_str(f: float):
    if math.isnan(f):
        return 'NAN'
    elif math.isinf(f):
        return '-INFINITY' if f < 0 else 'INFINITY'
    else:
        return str(f)

class Core(object):
    def __init__(self, name, core, cost, argc, type):
        self.name = name
        self.core = core
        self.cost = cost
        self.argc = argc
        self.type = type

    def __repr__(self):
        return 'Core(' + \
            'name=' + repr(self.name) + ', ' + \
            'core=' + repr(self.core) + ', ' + \
            'cost=' + repr(self.cost) + ', ' + \
            'argc=' + repr(self.argc) + ', ' + \
            'type=' + repr(self.type) + \
            ')'

fpcore_pat = re.compile('// \(FPCore (.*)')
comment_pat = re.compile('// ')
cost_pat = re.compile('// best cost: ([0-9.]*)')
fun_pat = re.compile('(double|float) ([a-zA-Z0-9_]+)\(([a-zA-Z0-9_ ,]*)\)')

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
            props['core'] = ''.join(map(lambda s : s.replace('//', '').replace('\n', ''), lines))
        else:
            cost = re.match(cost_pat, line)
            if cost is not None:
                # found a cost
                props['cost'] = cost.group(1)
            else:
                fun = re.match(fun_pat, line)
                if fun is not None:
                    # found a function, so we should make a driver for it
                    func_name = fun.group(2)    # function name
                    arg_str = fun.group(3)      # function arguments
                    scalar_type = fun.group(1)  # function type (assuming uniform)
                    # hacky argument counting
                    argc = 0 if len(arg_str) == 0 else len(arg_str.split(','))
                    # make core
                    core = props.get('core')
                    cost = int(props.get('cost')) if 'cost' in props else None
                    return Core(func_name, core, cost, argc, scalar_type)
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

def sample_repr(scalar_type, n):
    if scalar_type == 'double':
        return [bv64_to_double(random.randint(0, 2 ** 64 - 1)) for _ in range(n)]
    elif scalar_type == 'float':
        raise NotImplementedError('float sampling')
    else:
        raise ValueError('unrecognized representation', scalar_type)
    
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

def write_driver(driver_name: str, core: Core, num_runs: int):
    by_vars = [list() for _ in range(core.argc)]
    sample = sample_core(core, num_runs)
    for input in sample:
        for i, value in enumerate(input):
            by_vars[i].append(value)

    with open(driver_name, 'a') as f:
        for i, sample in enumerate(by_vars):
            print(f'const {core.type} x{i}[{num_runs}] = {{', file=f)
            print(',\n'.join(map(double_to_c_str, sample)), file=f)
            print('};', file=f)
        
        print('#include <stdio.h>', file=f)
        print('#include <time.h>', file=f)

        print('int main() {', file=f)
        print('struct timespec start, end;', file=f)
        print(f'volatile {core.type} res;', file=f)
        print('clock_gettime(CLOCK_MONOTONIC, &start);', file=f)

        print(f'for (long i = 0; i < {num_runs}; i++) {{', file=f)
        arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
        print(f'  res = {core.name}({arg_str});', file=f)
        print('}', file=f)

        print('clock_gettime(CLOCK_MONOTONIC, &end);', file=f)
        print('printf("%.6f us", (end.tv_nsec - start.tv_nsec) / 1000.0);', file=f)
        print('  return 0;', file=f)
        print('}', file=f)

def make_drivers(output_dir: str, cores: list[Core], num_runs: int):
    output_name = os.path.join(output_dir, output_file)

    # create drivers for each core
    drivers = []
    for core in cores:
        driver_dir = os.path.join(output_dir, core.name)
        if os.path.exists(driver_dir):
            shutil.rmtree(driver_dir)
        os.mkdir(driver_dir)
        # copy file (manual copy since we have to repair lines)
        driver_name = os.path.join(driver_dir, f'{core.name}.c')
        shutil.copy(output_name, driver_name)
        # write driver
        print(f'Generating driver for \'{core.name}\'')
        write_driver(driver_name, core, num_runs)
        drivers.append(driver_name)

    # compile each driver
    exes = []
    for driver_name in drivers:
        driver_dir, fname = os.path.split(driver_name)
        exe_name = os.path.join(driver_dir, fname.removesuffix('.c'))
        print(f'Compiling \'{fname}\' ==> \'{os.path.split(exe_name)[1]}\'')
        p = subprocess.Popen(['cc', '-O3', driver_name, '-o', exe_name, '-lm'])
        _, _ = p.communicate()
        exes.append(exe_name)

    return exes

def run_drivers(exes):
    # run each driver
    time_pat = re.compile('([0-9.]+) us')
    times = []
    for exe in exes:
        print(f'Running \'{os.path.split(exe)[1]}\'')
        p = subprocess.Popen([exe], stdout=subprocess.PIPE)
        stdout, _ = p.communicate()
        output = stdout.decode('utf-8')
        print(output)

        match = re.search(time_pat, output)
        if match is None:
            raise ValueError('Unexpected output')
        time = match.group(1)
        times.append(float(time))

    return times

def get_costs(output_dir):
    output_name = os.path.join(output_dir, output_file)
    pattern = re.compile('// best cost: ([0-9.]*)') # really hacky match

    costs = []
    with open(output_name, 'r') as f:
        for line in f.readlines():
            match = re.search(pattern, line)
            if match is not None:
                cost = match.group(1)
                costs.append(float(cost))

    return costs

def plot_cost_time(costs, times, num_runs):
    plt.scatter(costs, times)
    plt.title(f'Synthetic cost vs. run time ({num_runs} runs)')
    plt.xlabel('Cost')
    plt.ylabel('Time (us)')
    plt.show()


def main():
    default_num_runs = 10000

    parser = argparse.ArgumentParser(description='Herbie C-code generator')
    parser.add_argument('--num-inputs', help='number of inputs to evaluate run time', type=int)
    parser.add_argument('--recompile', help='recompile existing Herbie output and plot results', action='store_const', const=True, default=False)
    parser.add_argument('bench_dir', help='directory of benchmarks', type=str)
    parser.add_argument('output_dir', help='output directory', type=str)
    parser.add_argument('herbie_flags', help='Herbie command-line flags', type=str)
    
    namespace = parser.parse_args()
    args = vars(namespace)

    bench_dir = os.path.join(os.getcwd(), args['bench_dir'])
    output_dir = os.path.join(os.getcwd(), args['output_dir'])
    num_runs = default_num_runs if args['num_inputs'] is None else args['num_inputs']
    recompile = False if args['recompile'] is None else args['recompile']
    args = args['herbie_flags'].split(' ')
    
    if not recompile:
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
    exes = make_drivers(output_dir=output_dir, cores=cores, num_runs=num_runs)
    # run drivers
    times = run_drivers(exes=exes)
    # extract synthetic costs
    costs = list(map(lambda c : c.cost, cores))
    # plot cost vs. time
    plot_cost_time(costs, times, num_runs)


if __name__ == "__main__":
    main()

