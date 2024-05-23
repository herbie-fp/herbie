from typing import List, Dict, Optional, Tuple
from pathlib import Path
import tempfile
import pickle
import os

from .fpcore import FPCore

SampleType = List[List[float]]
CacheType = Dict[str, Tuple[FPCore, SampleType]]

class Cache(object):
    path: Path
    samples: Dict[str, Dict[int, FPCore]]

    def __init__(self, path: Path):
        self.path = path
        self.samples = dict()

    def put_sample(self, key: str, seed: int, sample: SampleType):
        """Caches a sample."""
        if key not in self.samples:
            self.samples[key] = dict()
        self.samples[key][seed] = sample

        key_dir = self.path.joinpath(key)
        if not key_dir.exists():
            try:
                key_dir.mkdir(parents=True)
            except FileExistsError:
                pass

        tmp_path = key_dir.joinpath(f'~{seed}-{os.getpid()}.sample')
        with open(tmp_path, 'wb') as f:
            pickle.dump(sample, f)
            f.flush()
            os.fsync(f.fileno())

        sample_path = key_dir.joinpath(f'{seed}.sample')
        os.replace(tmp_path, sample_path)

    def get_sample(self, key: str, seed: int) -> Optional[SampleType]:
        """Looks up an FPCore by key and seed"""
        samples = self.samples.get(key, None)
        if samples is not None:
            return samples.get(seed, None)

        key_dir = self.path.joinpath(key)
        if key_dir.exists():
            sample_path = key_dir.joinpath(f'{seed}.sample')
            if sample_path.exists():
                with open(sample_path, 'rb') as f:
                    sample = pickle.load(f)

                if key not in self.samples:
                    self.samples[key] = dict()
                self.samples[key][seed] = sample
                return sample

        return None


    def clear_core(self, key: str):
        """Eliminates a key from the cache."""
        del self.cores[key]
        del self.samples[key]

        with open('/tmp/test.txt', 'w') as f:
            f.write('hi')


# class Cache(object):
#     def __init__(self, path: str):
#         self.path: Path = Path(path)
#         self.input_cores: CacheType = dict()
#         self.platform_cores: Dict[str, Dict[str, List[FPCore]]] = dict()

#     def __repr__(self) -> str:
#         return 'Cache(' + \
#             'path=' + repr(self.path) + ', ' + \
#             'input_cores=' + repr(len(self.input_cores)) + ', ' + \
#             'platform_cores=' + repr(len(self.platform_cores)) + ')'

#     def restore(self) -> None:
#         # restore input cores
#         input_path = self.path.joinpath('input')
#         if input_path.exists():
#             for core_dir in input_path.iterdir():
#                 key = core_dir.name
#                 fpcore_path = core_dir.joinpath('input.fpcore')
#                 sample_path = core_dir.joinpath('sample')
#                 if fpcore_path.exists() and sample_path.exists():
#                     try:
#                         with open(fpcore_path, 'r') as f:
#                             fpcore = parse_core(f.read())
#                             fpcore.key = key
#                         with open(sample_path, 'rb') as f:
#                             sample = pickle.load(f)
#                         self.input_cores[key] = (fpcore, sample)
#                     except RuntimeError:
#                         print(f'Failed to parse FPCore at {fpcore_path}')
#         # restore platform cores
#         platforms_path = self.path.joinpath('platform')
#         if platforms_path.exists():
#             for platform_path in platforms_path.iterdir():
#                 in_platform = dict()
#                 platform = platform_path.name
#                 for cores_path in platform_path.iterdir():
#                     cores = []
#                     key = cores_path.name
#                     for core_path in cores_path.iterdir():
#                         with open(core_path, 'r') as f:
#                             fpcore = parse_core(f.read())
#                             fpcore.key = key
#                             cores.append(fpcore)
#                     in_platform[key] = cores
#                 self.platform_cores[platform] = in_platform

#     def write_core(self, core: FPCore, sample: SampleType):
#         if core.key is None:
#             raise RuntimeError(f'Cannot write FPCore without key: {core}')

#         input_path = self.path.joinpath('input')
#         core_dir = input_path.joinpath(core.key)
#         if not core_dir.exists():
#             core_dir.mkdir(parents=True)

#         core_path = core_dir.joinpath('input.fpcore')
#         with open(core_path, 'w') as f:
#             f.write(core.core)

#         sample_path = core_dir.joinpath('sample')
#         with open(sample_path, 'wb') as f:
#             pickle.dump(sample, f)

#         if core.key in self.input_cores and not core.override:
#             raise RuntimeError(f'Duplicate key: {core.key}')
#         self.input_cores[core.key] = (core, sample)

#     def write_platform_core(self, name: str, key: str, cores: List[FPCore]):
#         platform_path = self.path.joinpath('platform', name)
#         core_dir = platform_path.joinpath(key)
#         if not core_dir.exists():
#             core_dir.mkdir(parents=True)

#         for i, core in enumerate(cores):
#             core_path = core_dir.joinpath(f'{i}.fpcore')
#             with open(core_path, 'w') as f:
#                 f.write(core.core)

#         in_platform = self.platform_cores.get(name, None)
#         if in_platform is None:
#             in_platform = dict()
#             in_platform[key] = cores
#             self.platform_cores[name] = in_platform
#         else:
#             in_platform[key] = cores

#     def get_core(self, name: str):
#         return self.input_cores.get(name, None)

#     def clear_core(self, key: str):
#         del self.input_cores[key]

#     def get_platform_core(self, platform: str, key: str):
#         in_platform = self.platform_cores.get(platform, None)
#         if in_platform is None:
#             return None
#         return in_platform.get(key)

#     def num_cores(self):
#         return len(self.input_cores)

#     def num_platform_cores(self):
#         num = 0
#         for key in self.platform_cores:
#             in_platform = self.platform_cores[key]
#             for key in in_platform:
#                 num += len(in_platform[key])
#         return num
