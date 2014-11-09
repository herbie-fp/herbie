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
double f_of(ARGS);
double f_od(ARGS);
double f_ol(ARGS);
double f_im(ARGS);
double f_om(ARGS);

unsigned long long ulp(double x, double y) {
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

#define SETUP()                                   \
        clock_t start, end, zero;                 \
        int i, j, k;                              \
        double total, r1, r2, r1old;              \
        float *rands, *out, *correct;             \
        int count, mcount;                        \
        srand(time(NULL));                        \
        setup_mpfr_f_im();

#define CALIBRATE(iter)                                         \
        start = clock();                                        \
        for (i = 0; i < iter; i++) {                            \
                out[i] = 1 / rands[NARGS*i];                    \
        }                                                       \
        end = clock();                                          \
        zero = end - start;

#if NARGS == 1
#define TEST(type, iter)                                                \
        start = clock();                                                \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = f_##type (rands[i]);                           \
        }                                                               \
        end = clock();

#elif NARGS == 2
#define TEST(type, iter)                                                \
        start = clock();                                                \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = f_##type (rands[2*i], rands[2*i + 1]);         \
        }                                                               \
        end = clock();

#elif NARGS == 3
#define TEST(type, iter)                                                \
        start = clock();                                                \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = f_##type (rands[3*i], rands[3*i + 1], rands[3*i + 2]); \
        }                                                               \
        end = clock();

#elif NARGS == 4
#define TEST(type, iter)                                                \
        start = clock();                                                \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = f_##type (rands[4*i], rands[4*i + 1], rands[4*i + 2], rands[4*i + 3]); \
        }                                                               \
        end = clock();

#elif NARGS == 5
#define TEST(type, iter)                                                \
        start = clock();                                                \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = f_##type (rands[5*i], rands[5*i + 1], rands[5*i + 2], \
                                   rands[5*i + 3], rands[5*i + 4]);     \
        }                                                               \
        end = clock();

#elif NARGS == 6
#define TEST(type, iter)                                                \
        start = clock();                                                \
        for (i = 0; i < iter; i++) {                                    \
                out[i] = f_##type (rands[6*i], rands[6*i + 1], rands[6*i + 2], \
                                   rands[6*i + 3], rands[6*i + 4], rands[6*i + 5]); \
        }                                                               \
        end = clock();

#else
#define TEST(type, iter) abort();
#endif

#define SAMPLE(iter)                                                    \
        rands = get_random(iter);                                       \
        out = malloc(sizeof(float) * iter);

#define SAVE(iter)                                                      \
        correct = malloc(sizeof(float) * iter);                         \
        memcpy((void *) correct, (void *) out, sizeof(float) * iter);   \
        count = 0;                                                      \
        for (i = 0; i < iter; i++) {                                    \
                if (1 / correct[i] != 0 && correct[i] == correct[i]) {  \
                        count += 1;                                     \
                } /*else { printf("Bad point %g\n", correct[i]); }*/    \
        }

int main(int argc, char** argv) {
        SETUP();

        int iter, repet;
        iter = 1 << 15;
        repet = 100;
        if (argc > 1) iter = atoi(argv[1]);
        if (argc > 2) repet = atoi(argv[2]);

        printf("pts,repet,avg,se\n");
        for (j = 2; j < iter; j *= 2) {
                r1 = r2 = 0;
                mcount = -1;
                for (k = 0; k < repet; k++) {
                        SAMPLE(j);
                        TEST(im, j);
                        SAVE(j);

                        TEST(if, j);
                        total = 0;
                        for (i = 0; i < j; i++) {
                                if (1 / correct[i] != 0 && correct[i] == correct[i]) {
                                        unsigned long long int error = ulp(out[i], correct[i]);
                                        total += log(error + 1.0) / log(2);
                                }
                        }
                        r1old = r1;
                        r1 += (total / count - r1) / (k + 1);
                        r2 += (total / count - r1old) * (total / count - r1);
                        if (mcount == -1 || count < mcount) mcount = count;

                        free(rands);
                        free(out);
                        free(correct);
                }
                printf("%i,%i,%g,%g\n", mcount, repet,
                       r1, sqrt(r2 / (repet - 1.5)) / sqrt(repet));
        }
}
