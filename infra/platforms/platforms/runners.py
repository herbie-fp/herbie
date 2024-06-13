""" Constructs Runners """

from .runner import Runner

from .arith import ArithRunner
from .arith_fma import ArithFMARunner
from .avx import AVXRunner
from .c import CRunner
from .julia import JuliaRunner
from .math import MathRunner
from .mkl import MKLRunner
from .numpy import NumpyRunner
from .vdt import VdtRunner
from .python import PythonRunner
from .fdlibm import FdlibmRunner

def make_runner(
    platform: str,
    working_dir: str,
    herbie_path: str,
    **kwargs
) -> Runner:
    kwargs = {
        'working_dir': working_dir,
        'herbie_path': herbie_path,
        **kwargs
    }

    if platform == 'arith':
        return ArithRunner(**kwargs)
    elif platform == 'arith-fma':
        return ArithFMARunner(**kwargs)
    elif platform == 'c':
        return CRunner(**kwargs)
    elif platform == 'math':
        return MathRunner(**kwargs)
    elif platform == 'mkl':
        return MKLRunner(**kwargs)
    elif platform == 'python':
        return PythonRunner(**kwargs)
    elif platform == 'avx':
        return AVXRunner(**kwargs)
    elif platform == 'numpy':
        return NumpyRunner(**kwargs)
    elif platform == 'vdt':
        return VdtRunner(**kwargs)
    elif platform == 'julia':
        return JuliaRunner(**kwargs)
    elif platform == 'fdlibm':
        return FdlibmRunner(**kwargs)
    else:
        raise ValueError(f'Unsupported output platform: {platform}')
