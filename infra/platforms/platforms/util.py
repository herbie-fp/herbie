import math, random
import subprocess
import ctypes

from typing import List

def bv64_to_double(i: int):
    if i < 0 or i >= 2 ** 64:
        raise ValueError('out of bounds', i)
    return ctypes.c_double.from_buffer(ctypes.c_uint64(i)).value

def sample_repr(scalar_type, n):
    if scalar_type == 'double':
        return [bv64_to_double(random.randint(0, 2 ** 64 - 1)) for _ in range(n)]
    elif scalar_type == 'float':
        raise NotImplementedError('float sampling')
    else:
        raise ValueError('unrecognized representation', scalar_type)

def double_to_c_str(f: float):
    if math.isnan(f):
        return 'NAN'
    elif math.isinf(f):
        return '-INFINITY' if f < 0 else 'INFINITY'
    else:
        return str(f)

def chunks(xs: list, size: int):
    return (xs[pos:pos + size] for pos in range(0, len(xs), size))

def racket_to_py(s: str):
    if s == '+inf.0':
        return math.inf
    elif s == '-inf.0':
        return -math.inf
    elif s == '+nan.0':
        return math.nan
    else:
        return float(s)
    
def py_to_racket(v: float):
    if math.isnan(v):
        return '+nan.0'
    elif math.isinf(v):
        if v < 0:
            return '-inf.0'
        else:
            return '+inf.0'
    else:
        return str(v)

def sanitize_name(name: str):
    return name.replace('*', '_times_').replace('+', '_plus_').replace('/', '__')

SampleType = List[List[float]]

def sample_to_pcontext(sample: SampleType):
    points, gts = sample
    input_strs = []
    for i, gt in enumerate(gts):
        pt = []
        for j, _ in enumerate(points):
            pt.append(points[j][i])
        pt_str = ' '.join(map(lambda v: py_to_racket(v), pt))
        input_strs.append(f'(({pt_str}) {py_to_racket(gt)})')
    input_str = ' '.join(input_strs)
    return f'({input_str})'

def run_subprocess(
    cmd: List[str],
    capture_stdout: bool = False,
    redirect_stderr: bool = False,
    check: bool = True
) -> str:
    stdout = subprocess.PIPE if capture_stdout else None
    stderr = subprocess.STDOUT if redirect_stderr else None
    p = subprocess.run(cmd, stdout=stdout, stderr=stderr, check=check)
    return p.stdout.decode()
