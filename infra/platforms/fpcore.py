"""FPCore utilities"""

from typing import Optional
import re

fpcore_pat = re.compile('\(FPCore \(([^\(\)]*)\)')
named_fpcore_pat = re.compile('\(FPCore [^\s]+ \(([^\(\)]*)\)')
fpcore_prop_name_pat = re.compile('^.*:name "([^\"]*)"')
fpcore_prop_descr_pat = re.compile('^.*:description "([^\"]*)"')

class FPCore(object):
    """Python representation of an FPCore"""

    def __init__(
        self,
        core: str,
        key: Optional[str] = None,
        name: Optional[str] = None,
        descr: Optional[str] = None,
        argc: Optional[int] = None,
        compiled: Optional[str] = None,
        cost: Optional[float] = None,
        err: Optional[float] = None,
        py_sample: bool = False,
        override: bool = False
    ):
        self.core = core
        self.key = key
        self.name = name
        self.descr = descr
        self.argc = argc
        self.compiled = compiled
        self.cost = cost
        self.err = err
        self.py_sample = py_sample
        self.override = override
    
    def __repr__(self) -> str:
        return 'FPCore(' + \
            'core=' + repr(self.core) + ', ' + \
            'key=' + repr(self.key) + ', ' + \
            'name=' + repr(self.name) + ', ' + \
            'descr=' + repr(self.descr) + ', ' + \
            'argc=' + repr(self.argc) + ', ' + \
            'compiled=' + repr(self.compiled) + ', ' + \
            'cost=' + repr(self.cost) + ', ' + \
            'err=' + repr(self.err) + ', ' + \
            'py_sample=' + repr(self.py_sample) + ', ' + \
            'override=' + repr(self.override) + ')'

def parse_core(s: str) -> FPCore:
    """Parses a string as an FPCore."""
    core_match = re.match(fpcore_pat, s)
    if core_match is None:
        core_match = re.match(named_fpcore_pat, s)
        if core_match is None:
            raise RuntimeError('Failed to parse FPCore from', s)
        
    arg_str = core_match.group(1).strip()
    argc = len(arg_str.split())
    # optionally extract name
    name_match = re.match(fpcore_prop_name_pat, s)
    name = None if name_match is None else name_match.group(1).strip()
    # optionally extract description
    descr_match = re.match(fpcore_prop_descr_pat, s)
    descr = None if descr_match is None else descr_match.group(1).strip()
    return FPCore(core=s, name=name, descr=descr, argc=argc)
