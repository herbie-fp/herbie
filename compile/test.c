#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <gmp.h>
#include <mpfr.h>

void setup_mpfr();

double NAME_id(double);
double NAME_od(double);
double NAME_im(double);

unsigned long long int ulp(double x, double y) {
        int64_t xx = *((int64_t*) &x);
        xx = xx < 0 ? LLONG_MIN - xx : xx;

        int64_t yy = *((int64_t*)&y);
        yy = yy < 0 ? LLONG_MIN - yy : yy;

        return xx >= yy ? xx - yy : yy - xx;
}

double rand_double() {
        long long int c0 = rand()&0xffff;
        long long int c1 = rand()&0xffff;
        long long int c2 = rand()&0xffff;
        long long int c3 = rand()&0xffff;
        long long int c = ((c3<<48) | (c2<<32) | (c1<<16) | c0);
        return *(double*)&c;
}

double *get_random(int nums) {
        int i;
        double *arr = malloc(sizeof(double) * nums);
        for (i = 0; i < nums; i++) {
                arr[i] = rand_double();
        }
        return arr;
}

#define ID(x) x

#define SETUP()                                 \
        clock_t start, end;                     \
        int i;                                  \
        unsigned long long int max = 0;         \
        double total = 0;                       \
        setup_mpfr();
        

#define TEST(type, iter)                                  \
        start = clock();                                        \
        for (i = 0; i < iter; i++) {                            \
                out[i] = NAME##_##type (rands[i]);              \
        }                                                       \
        end = clock();                                          \
        printf("%s: %lu\n", #type, (end - start) / 1000);

#define CHECK(type, iter)                                         \
        max = total = 0;                                          \
        for (i = 0; i < iter; i++) {                                    \
                unsigned long long int error = ulp(out[i], correct[i]); \
                if (error > max) max = error; \
                total += log(error + 1.0) / iter / log(2);    \
        } \
        printf("%s: max %f avg %f\n", #type, log(max + 1.0) / log(2), total);

#define SAMPLE(iter) \
        srand(time(NULL)); \
        double *rands = get_random(iter); \
        double *out = malloc(sizeof(double) * iter);

#define SAVE(iter) \
        double *correct = malloc(sizeof(double) * iter); \
        memcpy((void *) correct, (void *) out, sizeof(double) * iter)

int main(int argc, char** argv) {
        SETUP();
        
        int iter = 1000000;
        if (argc > 1) iter = atoi(argv[1]);

        SAMPLE(iter);

        TEST(im, iter);
        SAVE(iter);

        mpfr_printf("2: %.20g\n", sqrttest_im(2000000.0));
        mpfr_printf("2: %.20g\n", sqrttest_id(2000000.0));
        mpfr_printf("2: %.20g\n", sqrttest_od(2000000.0));

        TEST(id, iter);
        CHECK(id, iter);

        TEST(od, iter);
        CHECK(od, iter);

        return 0;
        
}
