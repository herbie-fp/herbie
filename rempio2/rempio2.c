#include <math.h>
#include <stdint.h>

// Double-double constants for multiples of pi/2
static const double pi1o2_h = 1.5707963267948966;
static const double pi1o2_l = 6.123233995736766e-17;

static const double pi2o2_h = 3.141592653589793;
static const double pi2o2_l = 1.2246467991473532e-16;

// Function prototype for the internal libm remainder function
// It takes x, and an array y of size 2 to store the hi/lo remainder.
// It returns the integer quadrant n.
extern int __ieee754_rem_pio2(double x, double *y);

// A helper to add two double-doubles (x_h, x_l) + (y_h, y_l)
// and return the correctly rounded 64-bit float.
static inline double add22_to_double(double x_h, double x_l, double y_h, double y_l) {
    double sum_h = x_h + y_h;
    // Fast2Sum or basic error recovery to preserve low bits
    double err_h = sum_h - x_h;
    double err_x = x_h - (sum_h - err_h);
    double err_y = y_h - err_h;
    double sum_l = x_l + y_l + err_x + err_y;
    
    return sum_h + sum_l;
}

static inline int cmp22_zero(double x_h, double x_l) {
    if (x_h < 0.0) return -1;
    if (x_h > 0.0) return 1;
    if (x_l < 0.0) return -1;
    if (x_l > 0.0) return 1;
    return 0;
}

// Your wrapper for Herbie
double herbie_rem2pi(double x) {
    if (isnan(x) || isinf(x)) return x;
    if (fabs(x) < M_PI) return x;

    double y[2];
    int n = __ieee754_rem_pio2(x, y);
    
    // n % 4 logic
    unsigned q = (unsigned)n & 3u;

    if (q == 0) { // n % 4 == 0
        return y[0] + y[1];
    } 
    else if (q == 1) { // n % 4 == 1
        return add22_to_double(y[0], y[1], pi1o2_h, pi1o2_l);
    } 
    else if (q == 2) { // n % 4 == 2
        switch (cmp22_zero(y[0], y[1])) {
        case -1:
            return add22_to_double(y[0], y[1], pi2o2_h, pi2o2_l);
        case 1:
            return add22_to_double(y[0], y[1], -pi2o2_h, -pi2o2_l);
        default:
            return (((unsigned)n & 7u) == 2u) ? pi2o2_h : -pi2o2_h;
        }
    } 
    else { // n % 4 == 3
        return add22_to_double(y[0], y[1], -pi1o2_h, -pi1o2_l);
    }
}
