import argparse
import json
import re
import os

from pathlib import Path

script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
curr_dir = os.getcwd()

cross_pat = re.compile('cross-compile-([^.]*).json')

def merge_json(output_dir: Path, name: str):
    info = dict()
    cross_pat = re.compile('cross-compile-([^.]*).json')

    platforms_path = output_dir.joinpath('output', name)
    for platform_path in platforms_path.iterdir():
        if platform_path.is_dir():
            platform_info = dict()
            for file_path in platform_path.iterdir():
                if file_path.is_file():
                    if file_path.name == 'tuning.json':
                        with open(file_path, 'r') as f:
                            platform_info['tune'] = json.load(f)
                    elif file_path.name == 'improve.json':
                        with open(file_path, 'r') as f:
                            platform_info['improve'] = json.load(f)
                    elif file_path.name.startswith('cross-compile'):
                        matches = re.match(cross_pat, file_path.name)
                        name = matches.group(1)
                        if 'compare' not in platform_info:
                            platform_info['compare'] = dict()
                        with open(file_path, 'r') as f:
                            platform_info['compare'][name] = json.load(f)

            info[platform_path.name] = platform_info
        
    json_path = output_dir.joinpath('results.json')
    with open(json_path, 'w') as f:
        json.dump(info, f)


def main():
    parser = argparse.ArgumentParser(description='Herbie eval report merger')
    parser.add_argument('output_dir', help='directory to emit all working files', type=str)
    parser.add_argument('--key', help='unique identifier under which to place plots and other output', type=str)
    args = parser.parse_args()

    # extract command line arguments
    output_dir = Path(curr_dir).joinpath(args.output_dir)
    key: str = args.key
    if key is None:
        key = 'default'

    merge_json(output_dir, key)


if __name__ == '__main__':
    main()
