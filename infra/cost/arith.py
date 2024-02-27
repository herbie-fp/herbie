from typing import Optional

from .c import CRunner

unary_ops = ['neg']
binary_ops = ['+', '-', '*', '/']
ternary_ops = []

class ArithRunner(CRunner):
    def __init__(
        self,
        working_dir: str,
        herbie_path: str,
        num_inputs: Optional[int] = 10000,
        num_runs: int = 100,
        threads: int = 1
    ):
        super().__init__(
            working_dir=working_dir,
            herbie_path=herbie_path,
            num_inputs=num_inputs,
            num_runs=num_runs,
            threads=threads
        )

        self.name = 'arith'
        self.unary_ops = unary_ops
        self.binary_ops = binary_ops
        self.ternary_ops = ternary_ops


