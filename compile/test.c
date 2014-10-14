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
#define ARGS(t) t
#elif NARGS == 2
#define ARGS(t) t, t
#elif NARGS == 3
#define ARGS(t) t, t, t
#elif NARGS == 4
#define ARGS(t) t, t, t, t
#elif NARGS == 5
#define ARGS(t) t, t, t, t, t
#elif NARGS == 6
#define ARGS(t) t, t, t, t, t, t
#else
#define ARGS(t) abort()
#endif

void setup_mpfr_f_im(void);
void setup_mpfr_f_fm(void);
void setup_mpfr_f_dm(void);
double f_if(ARGS(float));
double f_id(ARGS(double));
double f_im(ARGS(double));
double f_of(ARGS(float));
double f_od(ARGS(double));
double f_om(ARGS(double));
extern char *name;

typedef unsigned long long int u64;
typedef unsigned int u32;

unsigned int ulpf(float x, float y) {
        if (x == 0) x = fabsf(x); // -0 == 0
        if (y == 0) y = fabsf(y); // -0 == 0

        if (x != x && y != y) return 0;
        if (x != x) return INT_MIN; // Maximum error
        if (y != y) return INT_MIN; // Maximum error

        u32 xx = *((u32*) &x);
        xx = xx < 0 ? INT_MIN - xx : xx;

        u32 yy = *((u32*) &y);
        yy = yy < 0 ? INT_MIN - yy : yy;

        return xx >= yy ? xx - yy : yy - xx;
}

unsigned long long ulpd(double x, double y) {
        if (x == 0) x = fabs(x); // -0 == 0
        if (y == 0) y = fabs(y); // -0 == 0

        if (x != x && y != y) return 0;
        if (x != x) return LLONG_MIN; // Maximum error
        if (y != y) return LLONG_MIN; // Maximum error

        u64 xx = *((u64*) &x);
        xx = xx < 0 ? LLONG_MIN - xx : xx;

        u64 yy = *((u64*) &y);
        yy = yy < 0 ? LLONG_MIN - yy : yy;

        return xx >= yy ? xx - yy : yy - xx;
}

char ordinaryf(float x) {
        return 1 / x != 0 && x == x;
}

char ordinaryd(double x) {
        return 1 / x != 0 && x == x;
}

#define ulpl ulpd
#define ulpm ulpd

double rand_double() {
        u64 c0 = rand()&0xffff;
        u64 c1 = rand()&0xffff;
        u64 c2 = rand()&0xffff;
        u64 c3 = rand()&0xffff;
        u64 c = ((c3 << 48) | (c2 << 32) | (c1<<16) | c0);
        return *(double*)&c;
}

float rand_float() {
        u32 c0 = rand()&0xffff;
        u32 c1 = rand()&0xffff;
        u32 c = ((c1<<16) | c0);
        return *(float*)&c;
}

float *get_random_floats(int nums) {
        int i;
        float *arr = malloc(sizeof(float) * nums * NARGS);
        for (i = 0; i < nums * NARGS; i++) {
                float rand;
                do {
                        rand = rand_float();
                } while (!ordinaryf(rand));
                arr[i] = rand;
        }
        return arr;
}

double *get_random_doubles(int nums) {
        int i;
        double *arr = malloc(sizeof(double) * nums * NARGS);
        for (i = 0; i < nums * NARGS; i++) {
                double rand;
                do {
                        rand = rand_double();
                } while (!ordinaryd(rand));
                arr[i] = rand;
        }
        return arr;
}

/* Some macros to make looping a bit easier */
#define LOOP(iter)                                                      \
        clock_gettime(CLOCK_MONOTONIC, &start);                         \
        for (i = 0; i < iter; i++)

#define END()                                                           \
        clock_gettime(CLOCK_MONOTONIC, &end);                           \
        rtime = (end.tv_sec - start.tv_sec) * 1.0e9 + (end.tv_nsec - start.tv_nsec);

/* Calling a function with some number of arguments */
#if NARGS == 1
#define EVAL(rands, f) f(rands[i])
#elif NARGS == 2
#define EVAL(rands, f) f(rands[2*i], rands[2*i + 1])
#elif NARGS == 3
#define EVAL(rands, f) f(rands[3*i], rands[3*i + 1], rands[3*i + 2])
#elif NARGS == 4
#define EVAL(rands, f) f(rands[4*i], rands[4*i + 1], rands[4*i + 2], rands[4*i + 3])
#elif NARGS == 5
#define EVAL(rands, f) f(rands[5*i], rands[5*i + 1], rands[5*i + 2], rands[5*i + 3], rands[5*i + 4])
#elif NARGS == 6
#define EVAL(rands, f) f(rands[6*i], rands[6*i + 1], rands[6*i + 2], rands[6*i + 3], rands[6*i + 4], rands[6*i + 5])
#else
#define EVAL(rands, f) abort()
#endif

#define CHECK(io, type, iter)                                           \
        max = total = 0;                                                \
        for (i = 0; i < iter; i++) {                                    \
                if (ordinary##type(true##type[i])) {                    \
                        u64 error = ulp##type(out##type[i], true##type[i]); \
                                if (error > max) max = error;           \
                                total += log(error + 1.0) / log(2);     \
                }                                                       \
        }                                                               \
        printf("%s%s ,%15g,%15g,%15g\n", #io, #type, rtime,             \
               log(max + 1.0) / log(2), total / count##type);

int main(int argc, char** argv) {
        struct timespec start, end;
        int i, maxi;
        unsigned long long int max = 0;
        double rtime, total = 0;
        int countf = 0, countd = 0;
        double *ind, *outd, *trued;
        float *inf, *outf, *truef;
        setup_mpfr_f_im();

        int iter = 1000000;
        if (argc > 1) iter = atoi(argv[1]);

        inf = get_random_floats(NARGS * iter);
        ind = get_random_doubles(NARGS * iter);
        outf = malloc(sizeof(float) * iter);
        outd = malloc(sizeof(double) * iter);
        truef = malloc(sizeof(float) * iter);
        trued = malloc(sizeof(double) * iter);

        LOOP(iter) { truef[i] = (float) EVAL(inf, f_im); } END();
        LOOP(iter) { trued[i] = EVAL(ind, f_im); } END();

        LOOP(iter) { countf += (int) ordinaryf(truef[i]); } END();
        LOOP(iter) { countd += (int) ordinaryd(trued[i]); } END();

        printf("// %s\n", name);
        printf("test,           time,           max,            avg\n");

        LOOP(iter) { outf[i] = EVAL(inf, f_if); } END();
        CHECK(i, f, iter);

        LOOP(iter) { outd[i] = EVAL(ind, f_id); } END();
        CHECK(i, d, iter);

        LOOP(iter) { outf[i] = EVAL(inf, f_of); } END();
        CHECK(o, f, iter);

        LOOP(iter) { outd[i] = EVAL(ind, f_od); } END();
        CHECK(o, d, iter);

        return 0;

}
