from .c import CRunner

unary_ops = ['neg', 'fabs', 'sqrt']
binary_ops = ['+', '-', '*', '/', 'fmax', 'fmin']
ternary_ops = ['fma']

class ArithFMARunner(CRunner):
    def __init__(self, **kwargs): 
        super(CRunner, self).__init__(
            name='arith-fma',
            lang='c',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops,
            time_unit='ms',
            **kwargs
        )


