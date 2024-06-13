from argparse import ArgumentParser
from pathlib import Path
import json
import os

from platforms.fpcore import FPCore
from platforms.shim import shim_expr

# Paths
cwd = os.getcwd()

def main():
    parser = ArgumentParser(description='Table generator from eval')
    parser.add_argument('report_path', help='path to eval report', type=str)
    args = parser.parse_args()

    report_path = Path(cwd).joinpath(args.report_path)
    with open(report_path, 'r') as f:
        report = json.load(f)

    by_input = dict()
    for name, info in report.items():
        improve_report = info['improve']
        for input_info in improve_report['cores']:
            input_core = FPCore.from_json(input_info['input_core'])
            platform_cores = []
            for core_info in input_info['platform_cores']:
                platform_cores.append(FPCore.from_json(core_info['platform_core']))
            
            if platform_cores != []:
                if input_core.key not in by_input:
                    by_platform = dict()
                    by_platform[name] = platform_cores
                    by_input[input_core.key] = (input_core, by_platform)
                else:
                    _, by_platform = by_input[input_core.key]
                    by_platform[name] = platform_cores

    for input_core, by_platform in by_input.values():
        print(f'{input_core.name}:')
        print(f'  {input_core.core}')
        for platform, cores in by_platform.items():
            print(f'  {platform}')
            exprs = shim_expr(cores, platform)
            for i, (core, expr) in enumerate(zip(cores, exprs)):
                print(f'    {i+1}: {core.time} {core.err} {expr}')


if __name__ == '__main__':
    main()
