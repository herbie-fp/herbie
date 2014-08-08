#define _POSIX_C_SOURCE 199309L
#include <tgmath.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <gmp.h>
#include <mpfr.h>

#ifndef NARGS
#define NARGS 1
#endif

#if NARGS == 1
#define ARGS float
#elif NARGS == 2
#define ARGS float, float
#elif NARGS == 3
#define ARGS float, float, float
#elif NARGS == 4
#define ARGS float, float, float, float
#elif NARGS == 5
#define ARGS float, float, float, float, float
#elif NARGS == 6
#define ARGS float, float, float, float, float, float
#else
#define ARGS
#endif

void setup_mpfr_f_im(void);
void setup_mpfr_f_om(void);
double f_if(ARGS);
double f_id(ARGS);
double f_il(ARGS);
double f_im(ARGS);
double f_of(ARGS);
double f_od(ARGS);
double f_ol(ARGS);
double f_om(ARGS);
extern char *name;

unsigned long long ulpd(double x, double y) {
        if (x == 0) x = fabs(x); // -0 == 0
        if (y == 0) y = fabs(y); // -0 == 0

        if (x != x && y != y) return 0;
        if (x != x) return LLONG_MIN;
        if (y != y) return LLONG_MIN;

        long long xx = *((unsigned long long*) &x);
        xx = xx < 0 ? LLONG_MIN - xx : xx;

        long long yy = *((unsigned long long*)&y);
        yy = yy < 0 ? LLONG_MIN - yy : yy;

        return xx >= yy ? xx - yy : yy - xx;
}

#define ulpl ulpd
#define ulpm ulpd

unsigned int ulpf(float x, float y) {
        if (x == 0) x = fabsf(x); // -0 == 0
        if (y == 0) y = fabsf(y); // -0 == 0

        if (x != x && y != y) return 0;
        if (x != x) return INT_MIN;
        if (y != y) return INT_MIN;

        int xx = *((unsigned int*) &x);
        xx = xx < 0 ? INT_MIN - xx : xx;

        int yy = *((unsigned int*)&y);
        yy = yy < 0 ? INT_MIN - yy : yy;

        return xx >= yy ? xx - yy : yy - xx;
}

float rand_float() {
        unsigned int c0 = rand()&0xffff;
        unsigned int c1 = rand()&0xffff;
        unsigned int c = ((c1<<16) | c0);
        return *(float*)&c;
}

float *get_random(int nums) {
        int i;
        float *arr = malloc(sizeof(float) * nums * NARGS);
        for (i = 0; i < nums * NARGS; i++) {
                arr[i] = rand_float();
        }
        return arr;
}

#define SETUP()                                                         \
        struct timespec start, end;                                     \
        double rtime, zero;                                              \
        int i, maxi;                                                    \
        unsigned long long int max = 0;                                 \
        double total = 0;                                               \
        float *rands;                                                   \
        double *out, *correct;                                          \
        setup_mpfr_f_im();                                              \
        setup_mpfr_f_om();

#define CALIBRATE(iter)                                         \
        clock_gettime(CLOCK_MONOTONIC, &start);                 \
        for (i = 0; i < iter; i++) {                            \
                out[i] = 1 / rands[NARGS*i];                    \
        }                                                       \
        clock_gettime(CLOCK_MONOTONIC, &end);              \
        zero = (end.tv_sec - start.tv_sec) * 1.0e9 + (end.tv_nsec - start.tv_nsec);

#define TEST(type, iter)                                                \
        clock_gettime(CLOCK_MONOTONIC, &start);                         \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = EVAL(f_##type);                                \
        }                                                               \
        clock_gettime(CLOCK_MONOTONIC, &end);                           \
        rtime = (end.tv_sec - start.tv_sec) * 1.0e9 + (end.tv_nsec - start.tv_nsec);

#if NARGS == 1
#define EVAL(f) f(rands[i])
#elif NARGS == 2
#define EVAL(f) f(rands[2*i], rands[2*i + 1])
#elif NARGS == 3
#define EVAL(f) f(rands[3*i], rands[3*i + 1], rands[3*i + 2])
#elif NARGS == 4
#define EVAL(f) f(rands[4*i], rands[4*i + 1], rands[4*i + 2], rands[4*i + 3])
#elif NARGS == 5
#define EVAL(f) f(rands[5*i], rands[5*i + 1], rands[5*i + 2], rands[5*i + 3], rands[5*i + 4])
#elif NARGS == 6
#define EVAL(f) f(rands[6*i], rands[6*i + 1], rands[6*i + 2], rands[6*i + 3], rands[6*i + 4], rands[6*i + 5])
#else
#define EVAL(f) abort()
#endif

#define CHECK(io, type, iter)                                            \
        maxi = -1;                                                      \
        max = total = 0;                                                \
        for (i = 0; i < iter; i++) {                                    \
                /*printf("in[i] = %g; out[i] = %g; cor[i] = %g", rands[i], out[i], correct[i]);*/ \
                if (1 / correct[i] != 0 && correct[i] == correct[i]) {  \
                        unsigned long long int error = ulp##type(out[i], correct[i]); \
                        /*printf(" error = %g\n", log(error + 1.0) / log(2));*/ \
                        if (error > max) { maxi = i; max = error; }     \
                        total += log(error + 1.0) / log(2);             \
                }                                                       \
        }                                                               \
        printf("%s%s ,%15g,%15g,%15g\n", #io, #type, rtime,              \
               log(max + 1.0) / log(2), total / count);

#define SAMPLE(iter)                                                    \
        srand(time(NULL));                                              \
        rands = get_random(iter);                                       \
        out = malloc(sizeof(double) * iter);

#define SAVE(iter)                                                      \
        correct = malloc(sizeof(double) * iter);                         \
        memcpy((void *) correct, (void *) out, sizeof(double) * iter);   \
        count = 0;                                                      \
        printf("im  ,%15g,%15g,%15g\n", rtime, 0.0, 0.0);        \
        for (i = 0; i < iter; i++) {                                    \
                if (1 / correct[i] != 0 && correct[i] == correct[i]) {  \
                        count += 1;                                     \
                }                                                       \
        }

int main(int argc, char** argv) {
        int count;
        SETUP();

        int iter = 1000000;
        if (argc > 1) iter = atoi(argv[1]);

        printf("// %s\n", name);
        printf("test,           time,           max,            avg\n");

        //printf("x = 2.873151427e-15\n");
        //printf("ULP(0, 8.25e-30) = %.20g\n", log(ulp(0, 8.25e-30) + 1.0) / log(2));
        //printf("ULP(4.440892099e-16, 8.25e-30) = %.20g\n", log(ulp(4.440892099e-16, 8.25e-30) + 1.0) / log(2));
        //printf("ULP(%f, 8.25e-30) = %.20g\n", 0.0/0.0, log(ulp(0.0 / 0.0, 8.25e-30) + 1.0) / log(2));

        SAMPLE(iter);
        CALIBRATE(iter);

        TEST(im, iter);
        SAVE(iter);

        TEST(if, iter);
        CHECK(i, f, iter);

        TEST(id, iter);
        CHECK(i, d, iter);

        TEST(il, iter);
        CHECK(i, l, iter);

        TEST(om, iter);
        CHECK(o, m, iter);

        TEST(of, iter);
        CHECK(o, f, iter);

        TEST(od, iter);
        CHECK(o, d, iter);

        TEST(ol, iter);
        CHECK(o, l, iter);

        return 0;

}
