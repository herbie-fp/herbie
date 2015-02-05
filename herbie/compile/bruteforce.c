#define _POSIX_C_SOURCE 199309L
#include <tgmath.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <gmp.h>
#include <mpfr.h>

void setup_mpfr_f_im(void);
void setup_mpfr_f_fm(void);
void setup_mpfr_f_dm(void);
double f_if(float);
double f_id(double);
double f_im(double);
double f_of(float);
double f_od(double);
double f_om(double);
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

int main(int argc, char** argv) {
        u32 x = 0;
        u32 stop = 0;
        u32 parts, part;
        float xf = 0.0, exact = 0.0, approxi = 0.0, approxo;
        u32 ei = 0, eo = 0;
        u32 maxi = 0, maxo = 0, numbad = 0, maxbad = 0;

        setup_mpfr_f_im();

        if (argc < 3) {
                printf("Usage: bfN.bin [log2(parts)] [part]\n");
                exit(1);
        }

        parts = atoi(argv[1]);
        part = atoi(argv[2]);

        x = (part << (32 - parts));
        if (part == (1 << parts)) {
                stop = ~1u + 1u; // The last float is a NaN, so we don't care about it.
        } else {
                stop = (part + 1) << (32 - parts);
        }

        fprintf(stderr, "// %s, %08x to %08x\n", name, x, stop);

        for (; x != stop; x++) {
                if ((x & 0xffff) == 0) {
                        fprintf(stderr, "%08x,%u,%u,%u,%u\n", x, maxi, maxo, numbad, maxbad);
                }
                xf = *(float*)(void*)&x;
                if (!ordinaryf(xf)) continue;
                exact = f_im(xf);
                if (!ordinaryf(exact)) continue;
                approxi = f_if(xf);
                approxo = f_of(xf);
                ei = ulpf(approxi, exact);
                eo = ulpf(approxo, exact);
                if (maxi < ei) maxi = ei;
                if (maxo < eo) maxo = eo;
                if (eo > ei) {
                        numbad++;
                        if (maxbad < eo - ei) maxbad = eo - ei;
                }
        }
        printf("%i,%u,%u,%u,%u\n", part, maxi, maxo, numbad, maxbad);
}
