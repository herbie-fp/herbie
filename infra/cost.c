#include <math.h>
#include <stdio.h>
#include <time.h>

#define ITERS 100000000

#define benchmark(expr) \
    total = 0.0; \
    start = clock(); \
    for (i = 0; i < ITERS; i++) { \
        total += total * (expr); \
    } \
    end = clock(); \
    printf("%-30s  %-22a  %lu\n", #expr, total, (end - start) / 10000);

int
main() {
    double x = 1e-7;
    double y = 1.517;
    double z = -6.739749715401135e23;

    double total = 0;
    int i;
    clock_t start, end;

    /* baseline */
    benchmark(0);

    /* arithmetic */
    benchmark(x + y);
    benchmark(-x);
    benchmark(x - y);
    benchmark(x * y);
    benchmark(x / y);

    /* exponents */
    benchmark(sqrt(x));
    benchmark(cbrt(x));
    benchmark(exp(x));
    benchmark(expm1(x));
    benchmark(pow(x, y));
    benchmark(log(x+1));
    benchmark(log1p(x));

    /* trig */
    benchmark(sin(x));
    benchmark(cos(x));
    benchmark(tan(x));
    benchmark(1.0 / tan(x));
    benchmark(asin(x));
    benchmark(acos(x));
    benchmark(atan(x));
    benchmark(sinh(x));
    benchmark(cosh(x));
    benchmark(tanh(x));
    benchmark(atan2(x, y));

    /* misc */
    benchmark(fabs(x));
    benchmark(fma(x, y, z));
    benchmark(hypot(x, y));
    benchmark(fmod(x, y));

    /* comparison and conditional */
    benchmark((i&17) ? total + x: total - y);
    benchmark(x == total);
    benchmark(y > total);
    benchmark(y < total);
    benchmark(y >= total);
    benchmark(y <= total);
    benchmark((i & 17) && (i & 13));
    benchmark((i & 17) || (i & 13));
}
