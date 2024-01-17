import sys, os
import shutil
import subprocess
import re
import ctypes
import random
import math

output_file = 'output.c'

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

def write_driver(driver_name, func_name, scalar_type, argc, num_runs):
    with open(driver_name, 'a') as f:
        for i in range(argc):
            sample = sample_repr(scalar_type, num_runs)
            print(f'const {scalar_type} x{i}[{num_runs}] = {{', file=f)
            print(',\n'.join(map(double_to_c_str, sample)), file=f)
            print('};', file=f)
        
        print('#include <stdio.h>', file=f)
        print('#include <time.h>', file=f)

        print('int main() {', file=f)
        print('struct timespec start, end;', file=f)
        print(f'volatile {scalar_type} res;', file=f)
        print('clock_gettime(CLOCK_MONOTONIC, &start);', file=f)

        print(f'for (long i = 0; i < {num_runs}; i++) {{', file=f)
        arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(argc)))
        print(f'  res = {func_name}({arg_str});', file=f)
        print('}', file=f)

        print('clock_gettime(CLOCK_MONOTONIC, &end);', file=f)
        print('printf("%ld us\\n", (end.tv_nsec - start.tv_nsec) / 1000);', file=f)
        print('  return 0;', file=f)
        print('}', file=f)


def time(output_dir, num_runs=100000):
    output_name = os.path.join(output_dir, output_file)
    pattern = re.compile('(double|float) ([a-zA-Z0-9_]+)\(([a-zA-Z0-9_ ,]*)\)') # really hacky match

    # create drivers for each benchmark
    drivers = []
    with open(output_name, 'r') as f:
        for line in f.readlines():
            match = re.search(pattern, line)
            if match is not None:
                # found a function, so we should make a driver for it
                func_name = match.group(2)    # function name
                arg_str = match.group(3)      # function arguments
                scalar_type = match.group(1)  # function type (assuming uniform)
                # hacky argument counting
                argc = 0 if len(arg_str) == 0 else len(arg_str.split(','))
                # create directory
                driver_dir = os.path.join(output_dir, func_name)
                os.mkdir(driver_dir)
                # copy file (manual copy since we have to repair lines)
                driver_name = os.path.join(driver_dir, f'{func_name}.c')
                shutil.copy(output_name, driver_name)
                # with open(output_name, 'r') as src:
                #     with open(driver_name, 'w') as dst:
                #         for line in src.readlines():
                #             if re.search(pattern, line) is None:
                #                 dst.write(line)
                #             else:
                #                 dst.write('volatile ')
                #                 dst.write(line)
                # write driver
                write_driver(driver_name, func_name, scalar_type, argc, num_runs)
                drivers.append(driver_name)

    # compile each driver
    exes = []
    for driver_name in drivers:
        driver_dir, fname = os.path.split(driver_name)
        exe_name = os.path.join(driver_dir, fname.removesuffix('.c'))
        print(f'Compiling \'{fname}\' ==> \'{os.path.split(exe_name)[1]}\'')
        p = subprocess.Popen(['cc', '-O3', driver_name, '-o', exe_name, '-lm'])
        stdout, stderr = p.communicate()
        exes.append(exe_name)

    # run each driver
    for exe in exes:
        print(f'Running \'{os.path.split(exe)[1]}\'')
        p = subprocess.Popen([exe])
        stdout, stderr = p.communicate()
        

def main():
    if len(sys.argv) < 3:
        print(f'Usage: {sys.argv[0]} <bench_dir> <output_dir>')
        sys.exit(1)
    
    bench_dir = os.path.join(os.getcwd(), sys.argv[1])
    output_dir = os.path.join(os.getcwd(), sys.argv[2])
    args = sys.argv[3:]

    run(bench_dir=bench_dir, output_dir=output_dir, args=args)
    time(output_dir=output_dir)


if __name__ == "__main__":
    main()

