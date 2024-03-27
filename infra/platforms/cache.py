from typing import List, Optional, Tuple
from pathlib import Path
from io import TextIOWrapper
import pickle

from .fpcore import FPCore, parse_core

# File system
#
# <top>
# |-- input
#     |-- key
#         |-- input.fpcore
#         |-- sample
#     ...
# |-- platforms
#     |-- name
#         |-- fpcores
#             |-- input-name
#                 |-- input-key
#                 |-- output.fpcore
#             ...
#         |-- build
#             ...
#         |-- plots
#             ...
#    ...
# ...

def sanitize_name(name: str):
    return name.replace('*', '_times_').replace('+', '_plus_').replace('/', '_slash_')

class Cache(object):
    def __init__(self, path: str):
        self.path = Path(path)
        self.cores = dict()

    def __repr__(self) -> str:
        return 'Cache(' + \
            'path=' + repr(self.path) + ', ' + \
            'cores=' + repr(len(self.cores)) + ')'

    def restore(self) -> None:
        input_path = self.path.joinpath('input')
        if input_path.exists():
            for core_dir in input_path.iterdir():
                key = core_dir.name
                fpcore_path = core_dir.joinpath('input.fpcore')
                sample_path = core_dir.joinpath('sample')
                if fpcore_path.exists() and sample_path.exists():
                    try:
                        with open(fpcore_path, 'r') as f:
                            fpcore = parse_core(f.read())
                            fpcore.key = key
                        with open(sample_path, 'rb') as f:
                            sample = pickle.load(f)
                        self.cores[key] = (fpcore, sample)
                    except RuntimeError:
                        print(f'Failed to parse FPCore at {fpcore_path}')

    def get_core(self, name: str) -> Optional[Tuple]:
        return self.cores.get(sanitize_name(name), None)
    
    def write_core(self, core: FPCore, sample):
        if core.key is None:
            raise RuntimeError(f'Cannot write FPCore without key: {core}')

        synth_path = self.path.joinpath('input')
        core_dir = synth_path.joinpath(core.key)
        if not core_dir.exists():
            core_dir.mkdir(parents=True)

        core_path = core_dir.joinpath('input.fpcore')
        with open(core_path, 'w') as f:
            f.write(core.core)

        sample_path = core_dir.joinpath('sample')
        with open(sample_path, 'wb') as f:
            pickle.dump(sample, f)
