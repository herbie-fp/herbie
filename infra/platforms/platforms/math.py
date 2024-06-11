from .c import CRunner

# Support operations in standard C
unary_ops = ['neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc']
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'pow', 'remainder']

class MathRunner(CRunner):
    def __init__(self, **kwargs): 
        super(CRunner, self).__init__(
            name='math',
            lang='c',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            time_unit='ms',
            **kwargs
        )
