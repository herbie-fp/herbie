from typing import Optional

from .c import CRunner

unary_ops = ['neg', 'fabs', 'sqrt']
binary_ops = ['+', '-', '*', '/', 'fmax', 'fmin']
ternary_ops = []

class ArithRunner(CRunner):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.unary_ops=unary_ops
        self.binary_ops=binary_ops
        self.ternary_ops=ternary_ops


