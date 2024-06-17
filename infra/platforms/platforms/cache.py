from typing import List, Dict, Optional, Tuple
from pathlib import Path
import pickle
import os

from .fpcore import FPCore, parse_core
from .util import SampleType

class Cache(object):
    path: Path
    cores: Dict[str, FPCore]
    samples: Dict[str, Dict[int, FPCore]]

    def __init__(self, path: Path):
        self.path = path
        self.cores = dict()
        self.samples = dict()

    def put_core(self, core: FPCore):
        """Caches an `FPCore`"""
        key = core.key
        if key not in self.cores:
            self.cores[key] = core

        key_dir = self.path.joinpath(key)
        if not key_dir.exists():
            try:
                key_dir.mkdir(parents=True)
            except FileExistsError:
                pass

        tmp_path = key_dir.joinpath(f'~input-{os.getpid()}.fpcore')
        with open(tmp_path, 'w') as f:
            f.write(core.core)
            f.flush()
            os.fsync(f.fileno())

        core_path = key_dir.joinpath(f'input.fpcore')
        os.replace(tmp_path, core_path)


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

    def get_core(self, key: str) -> Optional[FPCore]:
        """Looks up an FPCore by key."""
        core = self.cores.get(key, None)
        if core is not None:
            return core

        key_dir = self.path.joinpath(key)
        if key_dir.exists():
            core_path = key_dir.joinpath(f'input.fpcore')
            with open(core_path, 'r') as f:
                core = parse_core(f.read())

            core.key = key
            self.cores[key] = core
            return core

        return None

    def get_sample(self, key: str, seed: int) -> Optional[SampleType]:
        """Looks up a sample by key and seed"""
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
        if key in self.cores:
            del self.cores[key]
        if key in self.samples:
            del self.samples[key]
