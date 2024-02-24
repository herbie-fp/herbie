"""FPCore utilities"""

from typing import Optional

class FPCore(object):
    """Python representation of an FPCore"""

    def __init__(
        self,
        core: str,
        name: Optional[str] = None,
        argc: Optional[int] = None,
        compiled: Optional[str] = None
    ):
        self.core = core
        self.name = name
        self.argc = argc
        self.compiled = compiled
    
    def __repr__(self) -> str:
        return 'FPCore(' + \
            'core=' + repr(self.core) + ', ' + \
            'name=' + repr(self.name) + ', ' + \
            'argc=' + repr(self.argc) + ', ' + \
            'compiled=' + repr(self.compiled) + ')'
