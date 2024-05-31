from .c import CRunner

class VdtRunner(CRunner):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # self.unary_ops += ["fast_exp", "fast_sin", "fast_cos", "fast_tan", "fast_tanh", "fast_log", "fast_asin", "fast_acos", "fast_atan"]
        self.unary_ops += ["vdt-exp"]
        self.name = "vdt"

        # TODO: Test on hamming
        # TODO: Fiddle with set-unknown-c
