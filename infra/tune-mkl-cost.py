import os, shutil
import math, random
import ctypes
import argparse
import subprocess
import multiprocessing as mp
import re

nullary_ops = ['baseline']
unary_ops = ['recip', 'rsqrt', 'neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'expm1', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'log1p', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'hypot', 'pow', 'remainder']
ternary_ops = []

default_num_inputs = 10000
racket_file = 'cost.rkt'
driver_file = 'main.c'
name_file = 'NAME'

# MKL
target_lang = 'mkl'
compiler = 'cc'
c_flags = ['-std=c11', '-O3', '-march=skylake']
ld_flags = ['-lmkl_intel_lp64', '-lmkl_intel_thread', '-liomp5', '-lmkl_core', '-lpthread', '-lm']

script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
time_pat = re.compile('([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) s')

def bv64_to_double(i: int):
    if i < 0 or i >= 2 ** 64:
        raise ValueError('out of bounds', i)
    return ctypes.c_double.from_buffer(ctypes.c_uint64(i)).value

def sample_repr(scalar_type, n):
    if scalar_type == 'double':
        return [bv64_to_double(random.randint(0, 2 ** 64 - 1)) for _ in range(n)]
    elif scalar_type == 'float':
        raise NotImplementedError('float sampling')
    else:
        raise ValueError('unrecognized representation', scalar_type)

def double_to_c_str(f: float):
    if math.isnan(f):
        return 'NAN'
    elif math.isinf(f):
        return '-INFINITY' if f < 0 else 'INFINITY'
    else:
        return str(f)
    
def compile(op: str, argc: int):
    op = '-' if op == 'neg' else op
    vars = [f'x{i}' for i in range(argc)]
    arg_str = ' '.join(vars)
    app_str = '(' + ' '.join([op] + vars) + ')'
    fpcore = f'(FPCore ({arg_str}) :name "foo" {app_str})'

    compile_path = os.path.join(script_dir, racket_file)
    p = subprocess.Popen(['racket', compile_path, 'compile', target_lang, f'\"{fpcore}\"'], stdout=subprocess.PIPE)
    stdout, _ = p.communicate()
    output = stdout.decode('utf-8')
    return f'inline {output}'

def chunkify(l, n):
    for i in range(0, len(l), n):  
        yield l[i:i + n]

def format_m256(arr):
    assert len(arr) == 4
    return '{' + double_to_c_str(arr[0]) + \
        ',' + double_to_c_str(arr[1]) + \
        ',' + double_to_c_str(arr[2]) + \
        ',' + double_to_c_str(arr[3]) + \
        "}"

def make_driver(output_dir: str, num_inputs: str, op: str, func: str, key: str, argc: int):
    # make directory for drivers
    driver_dir = os.path.join(output_dir, key)
    if os.path.exists(driver_dir):
        shutil.rmtree(driver_dir)
    os.mkdir(driver_dir)
    
    # emit name
    name_path = os.path.join(driver_dir, name_file)
    with open(name_path, 'w') as f:
        print(f'{op}', file=f, end='')

    # create driver
    driver_path = os.path.join(driver_dir, driver_file)
    with open(driver_path, 'w') as f:
        print('#include <stdio.h>', file=f)
        print('#include <stdlib.h>', file=f)
        print('#include <math.h>', file=f)
        print('#include <time.h>', file=f)
        print('#include <immintrin.h>', file=f)
        print('#include <mkl.h>', file=f)
        print('#define TRUE 1', file=f)
        print('#define FALSE 0', file=f)

        if op != 'baseline':
            print(func, file=f)

        for i in range(argc):
            print(f'const __m256d x{i}[{num_inputs}] = {{', file=f)
            sample = sample_repr('double', num_inputs)
            print(',\n'.join(map(format_m256, chunkify(sample, 4))), file=f)
            print('};', file=f)

        print('int main() {', file=f)
        print(f'volatile __m256d res;', file=f)
        print(f'clock_t start = clock();', file=f)
        
        arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(argc)))
        if op == 'baseline':
            app_str = '(__m256d) { 0.0, 0.0, 0.0, 0.0 }'
        else:
            app_str =  f'foo({arg_str})'

        # print(f'for (long k = 0; k < 10; k++) {{', file=f)
        print(f'for (long i = 0; i < {num_inputs}; i++) {{', file=f)
        print(f'  res = {app_str};', file=f)
        print('}', file=f)
        # print('}', file=f)

        print(f'clock_t end = clock();', file=f)
        print(f'double diff = ((double) (end - start)) / CLOCKS_PER_SEC;', file=f)
        print(f'printf("%.17g s", diff);', file=f)
        print('  return 0;', file=f)
        print('}', file=f)

    return driver_path
        
def compile_driver(cpp: str):
    exe_path = cpp.replace('.c', '')
    print(f'Compiling \'{cpp}\'')
    p = subprocess.Popen([compiler] + c_flags + ['-o', exe_path, cpp] + ld_flags)
    _, _ = p.communicate()

def run_driver(cpp: str):
    exe_path = cpp.replace('.c', '')
    p = subprocess.Popen([exe_path], stdout=subprocess.PIPE)
    stdout, _ = p.communicate()
    output = stdout.decode('utf-8')
    time = re.match(time_pat, output)
    if time is None:
        print(f'Unexpected error when running {cpp}: {output}')
        return None
    else:
        return float(time.group(1))

def make_runner(config):
    output_dir, num_inputs, op, argc, key = config
    print(f'Generating driver for {op} [argc: {argc}]')
    if op == 'baseline':
        func = ''
    else:
        func = compile(op, argc)
    cpp = make_driver(output_dir, num_inputs, op, func, key, argc)
    compile_driver(cpp)
    return cpp

def main():
    parser = argparse.ArgumentParser(description='Herbie cost model generator')
    parser.add_argument('--num-inputs', help='number of inputs to evaluate run time', type=int)
    parser.add_argument('--threads', help='number of threads for driver compilation', type=int)
    parser.add_argument('--generate', help='generate all drivers', action='store_const', const=True, default=False)
    parser.add_argument('output_dir', help='output directory', type=str)
    namespace = parser.parse_args()

    args = vars(namespace)
    output_dir = os.path.join(os.getcwd(), args['output_dir'])
    num_inputs = default_num_inputs if args['num_inputs'] is None else args['num_inputs']
    threads = 1 if args['threads'] is None else args['threads']
    generate = args['generate']

    if generate:
        # make directory for drivers
        if os.path.exists(output_dir):
            shutil.rmtree(output_dir)
        os.mkdir(output_dir)

        configs = []
        for argc, ops in enumerate([nullary_ops, unary_ops, binary_ops, ternary_ops]):
            for i, op in enumerate(ops):
                key = f'{argc}-{i}'
                configs.append((output_dir, num_inputs, op, argc, key))
        
        with mp.Pool(processes=threads) as p:
            p.map(make_runner, configs)

    elif os.path.exists(output_dir):
        names = []
        cpps = []
        for elt in os.listdir(output_dir):
            driver_dir = os.path.join(output_dir, elt)
            if os.path.isdir(driver_dir):
                driver_path = os.path.join(driver_dir, driver_file)
                name_path = os.path.join(driver_dir, name_file)
                cpps.append(driver_path.replace('.c', ''))
                with open(name_path, 'r') as f:
                    names.append(f.read())

        costs = [run_driver(cpp) for cpp in cpps]
        entries = [(name, cost) for name, cost in zip(names, costs)]

        # look for baseline
        baseline = None
        entries = sorted(entries, key=lambda e: str.lower(e[0]))
        for name, cost in entries:
            if name == 'baseline':
                baseline = cost
                break
        
        print(f'baseline: {baseline} s')
        print('op | relative (multiple of baseline)')
        for name, cost in entries:
            if name != 'baseline':
                print(f'[{name} {cost / baseline}]')

if __name__ == "__main__":
    main()
