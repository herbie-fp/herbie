import argparse
import math
import subprocess
import sys

DEFAULT_NUM_RUNS = 250
BASE_DIR = "growlibm/timing"
unary_accelerators = ['log1pmd', 'invgud', 'verdcos', 'powcos2', 'powcos4', 'powcos6']
binary_accelerators = ['sinprod', 'cosprod', 'hypot', 'powcos', 'pow1ms']
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'exp', 'exp2', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc'] + unary_accelerators
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'pow', 'remainder'] + binary_accelerators
all_ops = binary_ops + unary_ops
default_ops = all_ops
times = {}
costs = {}
plus_time = 0

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

def positive_int(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("value must be positive")
    return parsed

def parse_args(argv):
    parser = argparse.ArgumentParser(
        description="Time growlibm ops by sampling inputs, compiling a driver, and benchmarking it."
    )
    parser.add_argument(
        "--seed",
        help="Seed passed to sample.rkt",
    )
    parser.add_argument(
        "--runs",
        type=positive_int,
        default=DEFAULT_NUM_RUNS,
        help=f"Number of benchmark iterations per op (default: {DEFAULT_NUM_RUNS})",
    )
    parser.add_argument(
        "--sample-limit",
        type=positive_int,
        help="Limit the number of sampled inputs per op",
    )
    parser.add_argument(
        "--ops",
        nargs="+",
        default=default_ops,
        help=f"Ops to benchmark (default: {' '.join(default_ops)})",
    )
    parser.add_argument(
        "--order",
        choices=["both", "sorted", "unsorted"],
        default="both",
        help="Whether to benchmark sorted inputs, unsorted inputs, or both",
    )
    parser.add_argument(
        "legacy_seed",
        nargs="?",
        help="Backward-compatible positional seed",
    )

    args = parser.parse_args(argv)
    if args.seed is not None and args.legacy_seed is not None:
        parser.error("pass the seed either positionally or with --seed, not both")

    args.seed = args.seed if args.seed is not None else args.legacy_seed

    invalid_ops = [op for op in args.ops if op not in all_ops]
    if invalid_ops:
        parser.error(f"unknown ops: {', '.join(invalid_ops)}")

    return args

def generate_driver(compiled, input_points, arity, filepath, runs):
    with open(filepath, 'w') as f:
        print('#include <math.h>', file=f)
        print('#include <stdio.h>', file=f)
        print('#include <stdlib.h>', file=f)
        print('#include <time.h>', file=f)
        print('#include \"../../accelerators/accelerators.h\"', file=f)
        print('#define TRUE 1', file=f)
        print('#define FALSE 0', file=f)
        print(f'#define NUM_RUNS {runs}', file=f)

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

def run_command(args, env=None):
    try:
        result = subprocess.run(args, capture_output=True, text=True, check=True, env=env)
        return result.stdout
    except subprocess.CalledProcessError as err:
        if err.stdout:
            print(err.stdout, file=sys.stderr, end="")
        if err.stderr:
            print(err.stderr, file=sys.stderr, end="")
        raise

def sample_points(fpcore, seed):
    cmd = ['racket', f'{BASE_DIR}/sample.rkt', fpcore.core]
    if seed is not None:
        cmd.append(str(seed))
    result = run_command(cmd)
    return [i.split() for i in result.splitlines()]

def order_modes(order):
    if order == "sorted":
        return [("sorted", True)]
    if order == "unsorted":
        return [("unsorted", False)]
    return [("unsorted", False), ("sorted", True)]

def print_header(order):
    print('--------------------------------------------------')
    if order == "both":
        print('|', 'op'.ljust(20), '|', 'unsorted'.ljust(10), '|', 'sorted'.ljust(10), '|')
    else:
        print('|', 'op'.ljust(20), '|', order.ljust(10), '|')
    print('--------------------------------------------------')

def print_result_row(op, timings, order):
    if order == "both":
        print('|', op.ljust(20), '|', f'{timings["unsorted"]:.5f} ms', '|', f'{timings["sorted"]:.5f} ms', '|')
    else:
        print('|', op.ljust(20), '|', f'{timings[order]:.5f} ms', '|')

def time_op(op, sort_points, points, runs):
    fpcore = generate_fpcore(op)
    points = list(points)
    if sort_points:
        points.sort(key= lambda x: float(x[0]))
    _points = [list(col) for col in zip(*points)]

    result = run_command(['racket', f'{BASE_DIR}/compile.rkt', fpcore.core])
    op_name = 'div' if op == '/' else op
    order_name = "sorted" if sort_points else "unsorted"
    driver_name = f"{order_name}_{op_name}.c"
    generate_driver(result, _points, str(fpcore.arity), f'{BASE_DIR}/drivers/{driver_name}', runs)

    cmd = [
        'clang',
        '-O2',
        '-I',
        '..',
        f'{BASE_DIR}/drivers/{driver_name}',
        '-o',
        'benchmark',
        '-Lgrowlibm/accelerators',
        '-laccelerators',
        '-lm'
    ]
    if sys.platform.startswith('linux'):
        cmd.append('-Wl,-rpath,$ORIGIN/growlibm/accelerators')

    run_command(cmd)

    result = run_command(['./benchmark'])
    return float(result)

if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    mode_list = order_modes(args.order)

    print_header(args.order)

    for op in args.ops:
        points = sample_points(generate_fpcore(op), args.seed)
        if args.sample_limit is not None:
            points = points[:args.sample_limit]

        if not points:
            raise RuntimeError(f"no sample points generated for {op}")

        print(
            f"Timing {op} with {len(points)} sample points and {args.runs} runs...",
            file=sys.stderr,
        )

        timings = {}
        for label, sort_points in mode_list:
            timings[label] = time_op(op, sort_points, points, args.runs)

        primary_label = "unsorted" if "unsorted" in timings else mode_list[0][0]
        times[op] = timings[primary_label]
        if op == "+":
            plus_time = timings[primary_label]

        print_result_row(op, timings, args.order)

    print('--------------------------------------------------')

    if plus_time > 0:
        for op, time in times.items():
            costs[op] = time / plus_time * .2

        for op, cost in costs.items():
            print(op, cost)
    else:
        print("Skipping cost ratios because '+' was not benchmarked.", file=sys.stderr)
