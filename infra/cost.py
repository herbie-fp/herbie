import os

from cost.c import CRunner

curr_path = os.path.abspath(__file__)
curr_dir, _ = os.path.split(curr_path)
herbie_path = os.path.join(curr_dir, 'cost.rkt')

def main():
    runner = CRunner(working_dir='/tmp/herbie/c', herbie_path=herbie_path)
    runner.synthesize()
    runner.compile()
    runner.make_drivers()

if __name__ == "__main__":
    main()

