//clang -dynamiclib -O3 -o libaccelerators.dylib accelerators.c
#include <math.h>
#include <stdio.h>

double sin_xy(double x, double y) {
    double p = x * y;
    double q = fma(x, y, -p); 

    double sin_p, cos_p, sin_q, cos_q;
    __sincos(p, &sin_p, &cos_p);
    __sincos(q, &sin_q, &cos_q);

    return fma(sin_p, cos_q, cos_p * sin_q);
}

double cos_xy(double x, double y) {
    double p = x * y;
    double q = fma(x, y, -p); 

    double sin_p, cos_p, sin_q, cos_q;
    __sincos(p, &sin_p, &cos_p);
    __sincos(q, &sin_q, &cos_q);

    return fma(cos_p, cos_q, -sin_p * sin_q);
}

double sin_quotient_xy(double x, double y) {
    double p = x / y;            
    double r = fma(-p, y, x);    
    double q = r / y;             

    double sin_p, cos_p, sin_q, cos_q;
    __sincos(p, &sin_p, &cos_p); 
    __sincos(q, &sin_q, &cos_q); 

    return fma(sin_p, cos_q, cos_p * sin_q);
}

double cos_quotient_xy(double x, double y) {
    double p = x / y;
    double r = fma(-p, y, x);
    double q = r / y;

    double sin_p, cos_p, sin_q, cos_q;
    __sincos(p, &sin_p, &cos_p);
    __sincos(q, &sin_q, &cos_q);

    return fma(cos_p, cos_q, -sin_p * sin_q);
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
