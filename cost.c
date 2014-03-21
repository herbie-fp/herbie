#include <math.h>
#include <stdio.h>
#include <time.h>

#define benchmark(expr) \
    total = 0; \
    start = clock(); \
    for (i = 0; i < 100000000; i++) { \
        total += total * (expr); \
    } \
    end = clock(); \
    printf("%s\t%f\t%d\n", #expr, total, (end - start) / 10000);

main() {
    double x = 1e-7;
    double total = 0;
    int i;
    clock_t start, end;
    double y = 1.517;

    benchmark(0);
    benchmark(x + y);
    benchmark(x - y);
    benchmark(x * y);
    benchmark(x / y);
    benchmark(-x);
    benchmark(fabs(x));
    benchmark(sqrt(x));
    benchmark(exp(x));
    benchmark(pow(x, y));
    benchmark(log(x+1));
    benchmark(sin(x));
    benchmark(cos(x));
    benchmark(tan(x));
    benchmark(asin(x));
    benchmark(acos(x));
    benchmark(atan(x));
}
