import sys, os
import shutil
import subprocess
import re

output_file = 'output.c'

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

def write_driver(driver_name, func_name, scalar_type, argc):
    with open(driver_name, 'a') as f:
        print('int main() {', file=f)

        args = []
        for i in range(argc):
            args.append(f'x{i}')
            print(f'  {scalar_type} x{i} = 0.0;', file=f)
        arg_str = ', '.join(args)

        print(f'  {func_name}({arg_str});', file=f)
        print('  return 0;', file=f)
        print('}', file=f)


def time(output_dir, num_runs=1000000):
    output_name = os.path.join(output_dir, output_file)
    pattern = re.compile('(double|float) ([a-zA-Z0-9_]+)\(([a-zA-Z0-9_ ,]*)\)') # really hacky match

    # create drivers for each benchmark
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
                # copy file
                driver_name = os.path.join(driver_dir, f'{func_name}.c')
                shutil.copy(output_name, driver_name)
                # write driver
                write_driver(driver_name, func_name, scalar_type, argc)


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

