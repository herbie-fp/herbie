import subprocess
import math
import sys

NUM_RUNS = 250
BASE_DIR = "growlibm/timing"
unary_accelerators = ['log1pmd', 'invgud', 'verdcos', 'powcos2', 'powcos4', 'powcos6']
binary_accelerators = ['sinprod', 'cosprod', 'sinquot', 'cosquot', 'hypot', 'powcos']
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc'] + unary_accelerators
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'pow', 'remainder'] + binary_accelerators
# to_test = ['sin_xy', 'cos_xy', 'sin_quotient_xy', 'cos_quotient_xy', 'sin', 'cos', '/']
#'log', 'log1pmd', 'invgud', 'hypot', 'verdcos', 'sin', 'cos', 'sinprod', 'cosprod',
to_test =  ['powcos', 'pow', 'cos', 'powcos2', 'powcos4', 'powcos6']
class FPCore(object):
    def __init__(self, core, arity) -> None:
        self.arity = arity
        self.core = core

def double_to_c_str(f: float):
    if math.isnan(f):
        return 'NAN'
    elif math.isinf(f):
        return '-INFINITY' if f < 0 else 'INFINITY'
    else:
        return str(f)

def format_fpcore(op : str, arity : int) -> FPCore:
    op_ = '-' if op == 'neg' else op
    vars = [f'z{i}' for i in range(arity)]
    arg_str = ' '.join(vars)
    app_str = ' '.join([op_] + vars)
    core = f'(FPCore ({arg_str}) ({app_str}))'
    return FPCore(core, arity)

def generate_fpcore(op : str) -> FPCore:
    core = FPCore("",0)
    if op in unary_ops:
        core = format_fpcore(op, 1)
    elif op in binary_ops:
        core = format_fpcore(op, 2)
    return core

def generate_driver(compiled, input_points, arity, filepath):
    with open(filepath, 'w') as f:
        print('#include <math.h>', file=f)
        print('#include <stdio.h>', file=f)
        print('#include <stdlib.h>', file=f)
        print('#include <time.h>', file=f)
        print('#include \"../../accelerators/accelerators.h\"', file=f)
        print('#define TRUE 1', file=f)
        print('#define FALSE 0', file=f)
        print(f'#define NUM_RUNS {NUM_RUNS}', file=f)

        print(f'static inline {compiled}', file=f)

        for i, points in enumerate(input_points):
            print(f"const double x{i}[{len(input_points[0])}] = {{", file=f)
            print(',\n'.join(points), file=f)
            print('};', file=f)
        arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(int(arity))))
        app_str =  f'foo({arg_str})'

        print('int main() {', file=f)
        print(f'    struct timespec ts1, ts2;', file=f)
        print(f'    volatile double res;', file=f)
        print(f'    double total = 0;', file=f)
        print('    // warmup loop', file=f)
        print(f'    for (long i = 0; i < {len(input_points[0])}; i++) {{', file=f)
        print(f'        res = {app_str};', file=f)
        print('    }', file=f)
        print('    // timing loop', file=f)
        print(f'    for(long run = 0; run < NUM_RUNS; run++) {{', file=f)
        print(f'        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);', file=f)
        print(f'        for (long i = 0; i < {len(input_points[0])}; i++) {{', file=f)
        print(f'            res = {app_str};', file=f)
        print('        }', file=f)
        print(f'        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts2);', file=f)
        print(f'        double diff = (1000.0 * ts2.tv_sec + 1e-6 * ts2.tv_nsec) - (1000.0 * ts1.tv_sec + 1e-6 * ts1.tv_nsec);', file=f)
        print(f'        total += diff;', file=f)
        print('    }', file=f)
        print(f'    printf("%.17g", total / NUM_RUNS);', file=f)
        print('    return 0;', file=f)
        print('}', file=f)

def run_command(args):
    result = subprocess.run(args, capture_output=True, text=True, check=True)
    return result.stdout

def sample_points(fpcore, seed):
    cmd = ['racket', f'{BASE_DIR}/sample.rkt', fpcore.core]
    if seed is not None:
        cmd.append(str(seed))
    result = run_command(cmd)
    return [i.split() for i in result.splitlines()]

def time_op(op, sort_points, points):
    fpcore = generate_fpcore(op)
    points = list(points)
    if sort_points:
        points.sort(key= lambda x: float(x[0]))
    _points = [list(col) for col in zip(*points)]

    result = run_command(['racket', f'{BASE_DIR}/compile.rkt', fpcore.core])
    op_name = 'div' if op == '/' else op
    driver_name = f'{'sorted' if sort_points else 'unsorted'}_{op_name}.c'
    generate_driver(result, _points, str(fpcore.arity), f'{BASE_DIR}/drivers/{driver_name}')

    run_command(['clang', '-O2', '-I', '..', f'{BASE_DIR}/drivers/{driver_name}', '-o', 'benchmark', '-Lgrowlibm/accelerators', '-laccelerators'])

    result = run_command(['./benchmark'])
    return float(result)

if __name__ == "__main__":
    seed = None
    if len(sys.argv) == 2:
        seed = sys.argv[1]
    elif len(sys.argv) == 3 and sys.argv[1] == '--seed':
        seed = sys.argv[2]

    print('--------------------------------------------------')
    print('|', 'op'.ljust(20), '|', 'unsorted'.ljust(10), '|', 'sorted'.ljust(10), '|')
    print('--------------------------------------------------')
    times = {}
    for op in to_test:
        points = sample_points(generate_fpcore(op), seed)
        print('|', op.ljust(20), '|', f'{time_op(op, False, points):.5f} ms', '|', f'{time_op(op, True, points):.5f} ms', '|')
    print('--------------------------------------------------')
