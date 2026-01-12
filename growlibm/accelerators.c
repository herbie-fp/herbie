#define _GNU_SOURCE
#include <math.h>
#include <stdio.h>

#include "accelerators.h"

static inline void sincos_wrapper(double x, double* s, double* c) {
#if defined(__APPLE__)
    __sincos(x, s, c);
#else
    sincos(x, s, c);
#endif
}

double sin_xy(double x, double y) {
    double p = x * y;
    double q = fma(x, y, -p); 

    double sin_p, cos_p, sin_q, cos_q;
    sincos_wrapper(p, &sin_p, &cos_p);
    sincos_wrapper(q, &sin_q, &cos_q);

    return fma(sin_p, cos_q, cos_p * sin_q);
}

double cos_xy(double x, double y) {
    double p = x * y;
    double q = fma(x, y, -p); 

    double sin_p, cos_p, sin_q, cos_q;
    sincos_wrapper(p, &sin_p, &cos_p);
    sincos_wrapper(q, &sin_q, &cos_q);

    return fma(cos_p, cos_q, -sin_p * sin_q);
}

double sin_quotient_xy(double x, double y) {
    double p = x / y;            
    double r = fma(-p, y, x);    
    double q = r / y;             

    double sin_p, cos_p, sin_q, cos_q;
    sincos_wrapper(p, &sin_p, &cos_p); 
    sincos_wrapper(q, &sin_q, &cos_q); 

    return fma(sin_p, cos_q, cos_p * sin_q);
}

double cos_quotient_xy(double x, double y) {
    double p = x / y;
    double r = fma(-p, y, x);
    double q = r / y;

    double sin_p, cos_p, sin_q, cos_q;
    sincos_wrapper(p, &sin_p, &cos_p);
    sincos_wrapper(q, &sin_q, &cos_q);

    return fma(cos_p, cos_q, -sin_p * sin_q);
}

double log1pmd(double x) {
    static const double 
    Lp1 = 6.666666666666735130e-01,  // 3FE55555 55555593
    Lp2 = 3.999999999940941908e-01,  // 3FD99999 9997FA04
    Lp3 = 2.857142874366239149e-01,  // 3FD24924 94229359
    Lp4 = 2.222219843214978396e-01,  // 3FCC71C5 1D8E78AF
    Lp5 = 1.818357216161805012e-01,  // 3FC74664 96CB03DE
    Lp6 = 1.531383769920937332e-01,  // 3FC39A09 D078C69F
    Lp7 = 1.479819860511658591e-01;  // 3FC2F112 DF3E5244

    double z = x * x;
    
    double R = z * (Lp1 + z * (Lp2 + z * (Lp3 + z * (Lp4 + z * (Lp5 + z * (Lp6 + z * Lp7))))));

    return 2.0 * x + x * R;
}

// double approx_sin_xy(double x, double y) {
//     double p = x * y;
//     double q = fma(x, y, -p); 

//     double sin_p, cos_p;
//     __sincos(p, &sin_p, &cos_p);

//     return fma(sin_p, 1, cos_p);
// }

// double approx_cos_xy(double x, double y) {
//     double p = x * y;
//     double q = fma(x, y, -p); 

//     double sin_p, cos_p;
//     __sincos(p, &sin_p, &cos_p);

//     return fma(cos_p, 1, -sin_p);
// }
