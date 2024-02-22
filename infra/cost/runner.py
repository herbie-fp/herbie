from typing import List
from pathlib import Path
import shutil

from .fpcore import FPCore

def synthesize1(op: str, argc: int) -> FPCore:
        """Creates a single FPCore for an operation with an arity."""
        op = '-' if op == 'neg' else op
        vars = [f'x{i}' for i in range(argc)]
        arg_str = ' '.join(vars)
        app_str = '(' + ' '.join([op] + vars) + ')'
        core = f'(FPCore ({arg_str}) :name "driver for {op}" {app_str})'
        return FPCore(core)

class Runner(object):
    """Representing a runner for a given platform"""

    def __init__(
        self,
        name: str,
        working_dir: str,
        herbie_path: str,
        unary_ops: List[str] = [],
        binary_ops: List[str] = [],
        ternary_ops: List[str] = []
    ):
        # configuration data
        self.name = name
        self.working_dir = Path(working_dir)
        self.herbie_path = Path(herbie_path)
        self.unary_ops = unary_ops
        self.binary_ops = binary_ops
        self.ternary_ops = ternary_ops
        # mutable data
        self.cores: List[FPCore] = []

        # if the working directory does not exist, create it
        if not self.working_dir.exists(): 
            self.working_dir.mkdir(parents=True)
            self.log('created working directory at `' + str(self.working_dir) + '`')

    def log(self, msg: str, *args):
        """Logging routine for this runner."""
        print(f'[Runner:{self.name}]:', msg, *args)

    def synthesize(self):
        """For each operator, create an FPCore and append to `self.cores`."""
        for op in self.unary_ops:
            self.cores.append(synthesize1(op, 1))
        for op in self.binary_ops:
            self.cores.append(synthesize1(op, 2))
        for op in self.ternary_ops:
             self.cores.append(synthesize1(op, 3))

    def make_driver_dirs(self) -> List[str]:
        """Creates the subdirectories for each driver: one subdirectory
        per FPCore in `self.cores`. Returns the list of subdirectories.
        Likely a utility function for `make_drivers()`."""
        subdirs = []
        for i, _ in enumerate(self.cores):
            subdir = self.working_dir.joinpath(Path(str(i)))
            if subdir.exists():
                shutil.rmtree(subdir)
            subdir.mkdir()
            subdirs.append(subdir)
        self.log(f'prepared driver subdirectories')
        return subdirs
    
    def compile(self) -> None:
        """Compiles each FPCore in `self.cores` to the target language and stores
        the compiled (string) result in the `compiled` field of each FPCore.
        This method must be overriden by every implementation of `Runner`."""
        raise NotImplementedError('virtual method')

    def make_drivers(self) -> None:
        """Creates drivers for each compiled FPCore.
        Assumes `compile()` has already been previous called."""
        raise NotImplementedError('virtual method')

