"""Python-to-Racket shim for interfacing with Herbie"""

import os

from typing import List, Optional, Tuple
from subprocess import Popen, PIPE
from pathlib import Path

from .cache import SampleType
from .fpcore import FPCore, parse_core
from .util import sample_to_pcontext, py_to_racket, racket_to_py, sanitize_name

# paths
script_path = os.path.abspath(__file__)
script_dir, _ = os.path.split(script_path)
platforms_dir, _ = os.path.split(script_dir)
server_path = os.path.join(platforms_dir, 'server.rkt')
curr_dir = os.getcwd()

### Running the server

def run_server(
    cmds: List[List[str]],
    platform: Optional[str] = None,
    seed: Optional[int] = None
)-> List[str]:
    """Creates a Herbie server instances and runs a list of commands.
    The result is a list of strings containing the result of each command."""

    # build command line arguments
    pargs = ['racket', '-y', server_path]
    if platform is not None:
        pargs += ['--platform', platform]
    if seed is not None:
        pargs += ['--seed', str(seed)]

    # run server and execute commands
    results: List[str] = []
    with Popen(args=pargs, stdin=PIPE, stdout=PIPE, text=True) as p:
        # run commands
        for cmd in cmds:
            print(cmd, file=p.stdin, flush=True)
            results.append(p.stdout.readline())
        # shutdown server
        print('(exit)', file=p.stdin, flush=True)
        _ = p.stdout.read()

    return results

### Shim interface

def shim_error(
    sample: SampleType,
    cores: List[FPCore],
    platform: str,
    seed: int
) -> List[float]:
    pcontext = sample_to_pcontext(sample)
    core_strs = ' '.join(map(lambda c: str(c.core), cores))

    results = run_server([f'(error {pcontext} {core_strs})'], platform=platform, seed=seed)
    output = results[0].strip()

    errs = []
    for val in output.split(' '):
        errs.append(float(val))
    return errs

def shim_sample(
    core: FPCore,
    num_inputs: int,
    platform: str,
    seed: int
) -> Optional[SampleType]:
    results = run_server([f'(sample {num_inputs} {core.core})'], platform=platform, seed=seed)
    output = results[0].strip()

    if output == '#f':
        return None

    gts = []
    points = [[] for _ in range(core.argc)]
    for input in output.split('|'):
        parts = input.split(',')
        if len(parts) != 2:
            raise RuntimeError(f'malformed point {input}')

        for i, val in enumerate(parts[0].split(' ')):
            points[i].append(racket_to_py(val.strip()))
        gts.append(racket_to_py(parts[1].strip()))

    return (points, gts)

def shim_read(
    path: str,
    platform: Optional[str] = None
) -> List[FPCore]:
    path = Path(path)
    cores = []
    if path.is_file():
        results = run_server([f'(read \"{path}\")'], platform=platform)
        output = results[0].strip()
        for line in output.split('|'):
            if len(line) > 0:
                parts = line.strip().split(']')
                if len(parts) != 2:
                    raise ValueError(f'Unexpected result: {line}')

                id = parts[0]
                core_str = parts[1]

                core = parse_core(core_str.strip())
                core.key = sanitize_name(f'file:{str(path)}:{id}')
                cores.append(core)
    else:
        for subdir in path.iterdir():
            cores += shim_read(str(subdir))

    return cores

def shim_pareto(*core_groups: List[List[FPCore]], use_time: bool = False) -> List[List[Tuple[float, float]]]:
    # build commands
    cmds = []
    for cores in core_groups:
        # group FPCore by key
        cores_by_group = dict()
        for core in cores:
            if core.key in cores_by_group:
                cores_by_group[core.key].append(core)
            else:
                cores_by_group[core.key] = [core]

        # create frontiers
        frontiers = []
        for key in cores_by_group:
            group = cores_by_group[key]
            xs = list(map(lambda c: c.time if use_time else c.cost, group))
            ys = list(map(lambda c: c.err, group))

            frontier = ' '.join(list(map(lambda x, y: f'({x} {y})', xs, ys)))
            frontiers.append(f'({frontier})')

        # add command
        frontier_strs = ' '.join(frontiers)
        cmds.append(f'(pareto {frontier_strs})')

    # call server
    results = run_server(cmds)

    # build output frontiers
    frontiers = []
    for result in results:
        frontier = []
        for line in result.strip().split('|'):
            datum = line.split(' ')
            if len(datum) != 2:
                raise RuntimeError('Pareto frontier malformed:', datum)
            cost, err = float(datum[0]), float(datum[1])
            frontier.append((cost, err))
        frontiers.append(frontier)

    return frontiers

def shim_error2(
    samples: List[SampleType],
    inexactss: List[List[float]],
    precs: List[str]
) -> float:

    # build commands
    cmds = []
    for sample, inexacts, prec in zip(samples, inexactss, precs):
        pcontext = sample_to_pcontext(sample)
        inexact_str = ' '.join(map(py_to_racket, inexacts))
        cmds.append(f'(error2 {pcontext} ({inexact_str}) {prec})')

    # call server
    results = run_server(cmds)

    # parse results
    return list(map(lambda r: float(r.strip()), results))

def shim_expr(cores: List[FPCore], platform: str) -> List[str]:
    cmds = list(map(lambda c: f'(expr {c.core})', cores))
    results = run_server(cmds, platform=platform)
    return list(map(lambda r: r.strip(), results))
