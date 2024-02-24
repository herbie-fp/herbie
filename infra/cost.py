import os

from cost.c import CRunner

script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
herbie_path = os.path.join(script_dir, 'cost.rkt')

curr_dir = os.getcwd()

def main():
    runner = CRunner(working_dir='/tmp/herbie/c', herbie_path=herbie_path, num_inputs=100_000, threads=8)
    # bench_dir = os.path.join(curr_dir, 'bench/hamming/quadratic.fpcore')
    # runner.herbie_improve(bench_dir, threads=4)
    runner.synthesize()
    runner.herbie_compile()
    runner.herbie_cost()
    runner.make_driver_dirs()
    runner.make_drivers()
    runner.compile_drivers()
    runner.run_drivers()
    runner.print_times()

if __name__ == "__main__":
    main()

