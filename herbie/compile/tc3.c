#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.4";

double f_if(float x) {
        float r410 = 1.0;
        float r411 = x;
        float r412 = cos(r411);
        float r413 = r410 - r412;
        float r414 = sin(r411);
        float r415 = r413 / r414;
        return r415;
}

double f_id(double x) {
        double r416 = 1.0;
        double r417 = x;
        double r418 = cos(r417);
        double r419 = r416 - r418;
        double r420 = sin(r417);
        double r421 = r419 / r420;
        return r421;
}


double f_of(float x) {
        float r422 = x;
        float r423 = sin(r422);
        float r424 = 1.0;
        float r425 = r423 / r424;
        float r426 = r425 / r424;
        float r427 = cos(r422);
        float r428 = r427 + r424;
        float r429 = r423 / r428;
        float r430 = r429 / r423;
        float r431 = r426 * r430;
        return r431;
}

double f_od(double x) {
        double r432 = x;
        double r433 = sin(r432);
        double r434 = 1.0;
        double r435 = r433 / r434;
        double r436 = r435 / r434;
        double r437 = cos(r432);
        double r438 = r437 + r434;
        double r439 = r433 / r438;
        double r440 = r439 / r433;
        double r441 = r436 * r440;
        return r441;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r442, r443, r444, r445, r446, r447;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r442, "1", 10, MPFR_RNDN);
        mpfr_init(r443);
        mpfr_init(r444);
        mpfr_init(r445);
        mpfr_init(r446);
        mpfr_init(r447);
}

double f_im(double x) {
        ;
        mpfr_set_d(r443, x, MPFR_RNDN);
        mpfr_cos(r444, r443, MPFR_RNDN);
        mpfr_sub(r445, r442, r444, MPFR_RNDN);
        mpfr_sin(r446, r443, MPFR_RNDN);
        mpfr_div(r447, r445, r446, MPFR_RNDN);
        return mpfr_get_d(r447, MPFR_RNDN);
}

static mpfr_t r448, r449, r450, r451, r452, r453, r454, r455, r456, r457;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r448);
        mpfr_init(r449);
        mpfr_init_set_str(r450, "1", 10, MPFR_RNDN);
        mpfr_init(r451);
        mpfr_init(r452);
        mpfr_init(r453);
        mpfr_init(r454);
        mpfr_init(r455);
        mpfr_init(r456);
        mpfr_init(r457);
}

double f_fm(double x) {
        mpfr_set_d(r448, x, MPFR_RNDN);
        mpfr_sin(r449, r448, MPFR_RNDN);
        ;
        mpfr_div(r451, r449, r450, MPFR_RNDN);
        mpfr_div(r452, r451, r450, MPFR_RNDN);
        mpfr_cos(r453, r448, MPFR_RNDN);
        mpfr_add(r454, r453, r450, MPFR_RNDN);
        mpfr_div(r455, r449, r454, MPFR_RNDN);
        mpfr_div(r456, r455, r449, MPFR_RNDN);
        mpfr_mul(r457, r452, r456, MPFR_RNDN);
        return mpfr_get_d(r457, MPFR_RNDN);
}

static mpfr_t r458, r459, r460, r461, r462, r463, r464, r465, r466, r467;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r458);
        mpfr_init(r459);
        mpfr_init_set_str(r460, "1", 10, MPFR_RNDN);
        mpfr_init(r461);
        mpfr_init(r462);
        mpfr_init(r463);
        mpfr_init(r464);
        mpfr_init(r465);
        mpfr_init(r466);
        mpfr_init(r467);
}

double f_dm(double x) {
        mpfr_set_d(r458, x, MPFR_RNDN);
        mpfr_sin(r459, r458, MPFR_RNDN);
        ;
        mpfr_div(r461, r459, r460, MPFR_RNDN);
        mpfr_div(r462, r461, r460, MPFR_RNDN);
        mpfr_cos(r463, r458, MPFR_RNDN);
        mpfr_add(r464, r463, r460, MPFR_RNDN);
        mpfr_div(r465, r459, r464, MPFR_RNDN);
        mpfr_div(r466, r465, r459, MPFR_RNDN);
        mpfr_mul(r467, r462, r466, MPFR_RNDN);
        return mpfr_get_d(r467, MPFR_RNDN);
}

