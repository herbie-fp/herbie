"""FPCore utilities"""

from typing import Optional

class FPCore(object):
    """Python representation of an FPCore"""

    def __init__(
        self,
        core: str,
        compiled: Optional[str] = None
    ):
        self.core = core
        self.compiled = compiled
    
    def __repr__(self) -> str:
        return 'FPCore(' + \
            'core=' + repr(self.core) + ', ' + \
            'compiled=' + repr(self.compiled) + ')'
