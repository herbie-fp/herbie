import subprocess
import math

NUM_RUNS = 100
time_unit = 'ms'
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'pow', 'remainder'] + ['sin_xy', 'cos_xy', 'sin_quotient_xy', 'cos_quotient_xy']

to_test = ['sin_xy', 'cos_xy', 'sin_quotient_xy', 'cos_quotient_xy', 'sin', 'cos', '/']
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

def generate_driver(compiled, input_points, arity):
    with open("driver.c", 'w') as f:
        print('#include <math.h>', file=f)
        print('#include <stdio.h>', file=f)
        print('#include <stdlib.h>', file=f)
        print('#include <time.h>', file=f)
        print('#include <accelerators.h>', file=f)
        print('#define TRUE 1', file=f)
        print('#define FALSE 0', file=f)

        print(f'static inline {compiled}', file=f)

        for i, points in enumerate(input_points):
            print(f"const double x{i}[{len(input_points[0])}] = {{", file=f)
            print(',\n'.join(points), file=f)
            print('};', file=f)

        print('int main() {', file=f)
        print(f'struct timespec ts1, ts2;', file=f)
        print(f'volatile double res;', file=f)
        print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);', file=f)

        arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(int(arity))))
        app_str =  f'foo({arg_str})'
        print(f'for (long i = 0; i < {len(input_points[0])}; i++) {{', file=f)
        print(f'  res = {app_str};', file=f)
        print('}', file=f)

        print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts2);', file=f)
        print(f'double diff = (1000.0 * ts2.tv_sec + 1e-6 * ts2.tv_nsec) - (1000.0 * ts1.tv_sec + 1e-6 * ts1.tv_nsec);', file=f)
        print(f'printf("%.17g {time_unit}", diff);', file=f)
        print('  return 0;', file=f)
        print('}', file=f)

if __name__ == "__main__":
    print("op".ljust(20), '|', 'average', '|', 'median')
    print("-------------------------------------------------")
    for op in to_test:
        fpcore = generate_fpcore(op)
        pargs = ['racket', 'sample.rkt', fpcore.core]
        result = subprocess.run(pargs, capture_output=True, text=True, check=True)
        points = [i.split() for i in result.stdout.splitlines()]
        _points = [list(col) for col in zip(*points)]
        pargs = ['racket', 'compile.rkt', fpcore.core]
        result = subprocess.run(pargs, capture_output=True, text=True, check=True)
        generate_driver(result.stdout, _points, str(fpcore.arity))
        pargs = ['clang', '-O2', '-I', '..', 'driver.c', '../accelerators.c', '-o', 'benchmark']
        result = subprocess.run(pargs, capture_output=True, text=True, check=True)
        pargs = ['./benchmark']
        times = []
        for i in range(NUM_RUNS):
            result = subprocess.run(pargs, capture_output=True, text=True, check=True)
            times.append(float(result.stdout.split()[0]))
        times.sort()

        print(op.ljust(20), '|', f"{sum(times)/len(times):.5f}" , '|', f"{(times[len(times)//2]):.5f}")