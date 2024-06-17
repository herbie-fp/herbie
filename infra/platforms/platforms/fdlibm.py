from subprocess import Popen, CalledProcessError
from typing import List
from pathlib import Path
import os
import re

from .fpcore import FPCore
from .runner import Runner
from .util import double_to_c_str, chunks, run_subprocess

fdlibm_kernel = '''
#include <string.h>

// Code based on Sun's fdlibm

static const double
ln2_hi  =  6.93147180369123816490e-01,	/* 3fe62e42 fee00000 */
ln2_lo  =  1.90821492927058770002e-10,	/* 3dea39ef 35793c76 */
two54   =  1.80143985094819840000e+16,  /* 43500000 00000000 */
Lg1 = 6.666666666666735130e-01,  /* 3FE55555 55555593 */
Lg2 = 3.999999999940941908e-01,  /* 3FD99999 9997FA04 */
Lg3 = 2.857142874366239149e-01,  /* 3FD24924 94229359 */
Lg4 = 2.222219843214978396e-01,  /* 3FCC71C5 1D8E78AF */
Lg5 = 1.818357216161805012e-01,  /* 3FC74664 96CB03DE */
Lg6 = 1.531383769920937332e-01,  /* 3FC39A09 D078C69F */
Lg7 = 1.479819860511658591e-01;  /* 3FC2F112 DF3E5244 */

static double zero   =  0.0;

double log(double);
double log1p(double);
double log1pmd(double);

static inline __attribute__((always_inline))      
double log1p_k(double, int, int);
static inline __attribute__((always_inline))      
double log1pmd_k(double, double, int, int);

/* ASSUME LITTLE ENDIAN */
#define __HI(x) *(1+(int*)&x)
#define __LO(x) *(int*)&x

double log(double x)
{
	double f;
	int k,hx,i;
	unsigned lx;

	hx = __HI(x);		/* high word of x */
	lx = __LO(x);		/* low  word of x */

	k=0;
	if (hx < 0x00100000) {			/* x < 2**-1022  */
	    if (((hx&0x7fffffff)|lx)==0) 
		return -two54/zero;		/* log(+-0)=-inf */
	    if (hx<0) return (x-x)/zero;	/* log(-#) = NaN */
	    k -= 54; x *= two54; /* subnormal number, scale up x */
	    hx = __HI(x);		/* high word of x */
	} 
	if (hx >= 0x7ff00000) return x+x;
	k += (hx>>20)-1023;
	hx &= 0x000fffff;
	i = (hx+0x95f64)&0x100000;
	__HI(x) = hx|(i^0x3ff00000);	/* normalize x or x/2 */
	k += (i>>20);
	f = x-1.0;
        return log1p_k(f, k, hx);
}
       
static inline __attribute__((always_inline))      
double log1p_k(double f, int k, int hx)
{
	double s,R,dk;
	if((0x000fffff&(2+hx))<3) {	/* |f| < 2**-20 */
	    if(f==zero) {
              if(k==0) return zero;  else {dk=(double)k;
                return dk*ln2_hi+dk*ln2_lo;}}
	    R = f*f*(0.5-0.33333333333333333*f);
	    if(k==0) return f-R; else {dk=(double)k;
	    	     return dk*ln2_hi-((R-dk*ln2_lo)-f);}
	}
 	s = f/(2.0+f); 
        return log1pmd_k(s, f, k, hx);
}

double log1p(double f)
{
	int x = f + 1.0;
	int hx = __HI(x);		/* high word of x */
	return log1p_k(f, 0, hx);
}

static inline __attribute__((always_inline))      
double log1pmd_k(double s, double f, int k, int hx)
{
	double hfsq,z,R,w,t1,t2,dk;
	int i,j;
	dk = (double)k;
	z = s*s;
	i = hx-0x6147a;
	w = z*z;
	j = 0x6b851-hx;
	t1= w*(Lg2+w*(Lg4+w*Lg6)); 
	t2= z*(Lg1+w*(Lg3+w*(Lg5+w*Lg7))); 
	i |= j;
	R = t2+t1;
	if(i>0) {
	    hfsq=0.5*f*f;
	    if(k==0) return f-(hfsq-s*(hfsq+R)); else
		     return dk*ln2_hi-((hfsq-(s*(hfsq+R)+dk*ln2_lo))-f);
	} else {
	    if(k==0) return f-s*(f-R); else
		     return dk*ln2_hi-((s*(f-R)-dk*ln2_lo)-f);
	}
}

double log1pmd(double s)
{
	double f = 2.0*s / (1 - s);
	int x = f + 1.0;
	int hx = __HI(x);		/* high word of x */
	return log1pmd_k(s, f, 0, hx);
}
'''

# Support operations in standard C plus fdlibm
unary_ops = ['log1pmd', 'acc-log', 'neg', 'acos', 'acosh', 'asin', 'asinh', 'atan', 'atanh', 'cbrt', 'ceil', 'cos', 'cosh', 'erf', 'erfc', 'exp', 'exp2', 'expm1', 'fabs', 'floor', 'lgamma', 'log', 'log10', 'log2', 'log1p', 'logb', 'rint', 'round', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'tgamma', 'trunc','fast-exp','fast-sin','fast-cos','fast-tan','fast-tanh','fast-log','fast-asin','fast-acos','fast-atan','fast-isqrt'] 
binary_ops = ['+', '-', '*', '/', 'atan2', 'copysign', 'fdim', 'fmax', 'fmin', 'fmod', 'hypot', 'pow', 'remainder']
ternary_ops = ['fma']

# C lang
target_lang = 'c'
compiler = 'clang'
c_flags = ['-std=gnu11', '-ffp-contract=off', '-O2']
ld_flags = ['-lm']
driver_name = 'main.c'
time_unit = 'ms'

# Regex patterns
time_pat = re.compile(f'([-+]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[-+]?[0-9]+)?) {time_unit}')

class FdlibmRunner(Runner):
    """`Runner` for fdlibm"""
    def __init__(self, **kwargs):
        super().__init__(
            name='fdlibm',
            lang='c',
            unary_ops=unary_ops,
            binary_ops=binary_ops,
            ternary_ops=ternary_ops,
            time_unit='ms',
            **kwargs
        )

    def make_drivers(self, cores: List[FPCore], driver_dirs: List[str], samples: dict) -> None:
        for core, driver_dir in zip(cores, driver_dirs):
            driver_path = os.path.join(driver_dir, driver_name)
            # pull sample from cache
            sample = self.cache.get_sample(core.key, self.seed)
            input_points, _ = sample
            with open(driver_path, 'w') as f:
                print('#include <math.h>', file=f)
                print('#include <stdio.h>', file=f)
                print('#include <stdlib.h>', file=f)
                print('#include <time.h>', file=f)
                print('#define TRUE 1', file=f)
                print('#define FALSE 0', file=f)

                print(f'{fdlibm_kernel}', file=f)
                print(f'static inline {core.compiled}', file=f)

                for i, points in enumerate(input_points):
                    print(f'const double x{i}[{self.num_inputs}] = {{', file=f)
                    print(',\n'.join(map(double_to_c_str, points)), file=f)
                    print('};', file=f)

                print('int main() {', file=f)
                print(f'struct timespec ts1, ts2;', file=f)
                print(f'volatile double res;', file=f)
                print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);', file=f)

                arg_str = ', '.join(map(lambda i: f'x{i}[i]', range(core.argc)))
                app_str =  f'foo({arg_str})'
                print(f'for (long i = 0; i < {self.num_inputs}; i++) {{', file=f)
                print(f'  res = {app_str};', file=f)
                print('}', file=f)

                print(f'clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts2);', file=f)
                print(f'double diff = (1000.0 * ts2.tv_sec + 1e-6 * ts2.tv_nsec) - (1000.0 * ts1.tv_sec + 1e-6 * ts1.tv_nsec);', file=f)
                print(f'printf("%.17g {time_unit}", diff);', file=f)
                print('  return 0;', file=f)
                print('}', file=f)

        self.log(f'created drivers')

    def compile_drivers(self, driver_dirs: List[str]) -> None:
        # chunk over threads
        for driver_dirs in chunks(driver_dirs, self.threads):
            ps: List[Popen] = []
            # fork subprocesses
            for driver_dir in driver_dirs:
                driver_path = Path(os.path.join(driver_dir, driver_name))
                out_path = driver_path.parent.joinpath(driver_path.stem)
                p = Popen([compiler] + c_flags + ['-o', out_path, driver_path] + ld_flags)
                ps.append(p)
            # join processes
            for p in ps:
                rc = p.wait()
                if rc != 0:
                    raise CalledProcessError(rc, p.args, p.stdout, p.stderr)
        self.log(f'compiled drivers')

    def run_drivers(self, cores: List[FPCore], driver_dirs: List[str]) -> List[float]:
        # run processes sequentially
        times = [[] for _ in driver_dirs]
        for i, driver_dir in enumerate(driver_dirs):
            for _ in range(self.num_runs):
                driver_path = Path(os.path.join(driver_dir, driver_name))
                out_path = driver_path.parent.joinpath(driver_path.stem)
                output = run_subprocess([out_path], capture_stdout=True)
                time = re.match(time_pat, output)
                if time is None:
                    self.log("bad core: "+str(cores[i]))
                    raise RuntimeError('Unexpected error when running {out_path}: {output}')
                times[i].append(float(time.group(1)))

        times = [sum(ts) / len(ts) for ts in times]
        self.log(f'run drivers')
        return times

