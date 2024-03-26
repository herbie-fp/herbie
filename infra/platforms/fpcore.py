"""FPCore utilities"""

from typing import Optional

class FPCore(object):
    """Python representation of an FPCore"""

    def __init__(
        self,
        core: str,
        name: Optional[str] = None,
        descr: Optional[str] = None,
        argc: Optional[int] = None,
        compiled: Optional[str] = None,
        cost: Optional[float] = None,
        err: Optional[float] = None,
        py_sample: bool = False
    ):
        self.core = core
        self.name = name
        self.descr = descr
        self.argc = argc
        self.compiled = compiled
        self.cost = cost
        self.err = err
        self.py_sample = py_sample
    
    def __repr__(self) -> str:
        return 'FPCore(' + \
            'core=' + repr(self.core) + ', ' + \
            'name=' + repr(self.name) + ', ' + \
            'descr=' + repr(self.descr) + ', ' + \
            'argc=' + repr(self.argc) + ', ' + \
            'compiled=' + repr(self.compiled) + ', ' + \
            'cost=' + repr(self.cost) + ', ' + \
            'err=' + repr(self.err) + ', ' + \
            'py_sample=' + repr(self.py_sample) + ')'
