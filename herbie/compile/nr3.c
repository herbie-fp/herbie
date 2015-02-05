#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.4";

double f_if(float x) {
        float r424 = 1.0;
        float r425 = x;
        float r426 = cos(r425);
        float r427 = r424 - r426;
        float r428 = sin(r425);
        float r429 = r427 / r428;
        return r429;
}

double f_id(double x) {
        double r430 = 1.0;
        double r431 = x;
        double r432 = cos(r431);
        double r433 = r430 - r432;
        double r434 = sin(r431);
        double r435 = r433 / r434;
        return r435;
}


double f_of(float x) {
        float r436 = x;
        float r437 = sin(r436);
        float r438 = 1.0;
        float r439 = r437 / r438;
        float r440 = r439 / r438;
        float r441 = cos(r436);
        float r442 = r441 + r438;
        float r443 = r437 / r442;
        float r444 = r443 / r437;
        float r445 = r440 * r444;
        return r445;
}

double f_od(double x) {
        double r446 = x;
        double r447 = sin(r446);
        double r448 = 1.0;
        double r449 = r447 / r448;
        double r450 = r449 / r448;
        double r451 = cos(r446);
        double r452 = r451 + r448;
        double r453 = r447 / r452;
        double r454 = r453 / r447;
        double r455 = r450 * r454;
        return r455;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r456, r457, r458, r459, r460, r461;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r456, "1", 10, MPFR_RNDN);
        mpfr_init(r457);
        mpfr_init(r458);
        mpfr_init(r459);
        mpfr_init(r460);
        mpfr_init(r461);
}

double f_im(double x) {
        ;
        mpfr_set_d(r457, x, MPFR_RNDN);
        mpfr_cos(r458, r457, MPFR_RNDN);
        mpfr_sub(r459, r456, r458, MPFR_RNDN);
        mpfr_sin(r460, r457, MPFR_RNDN);
        mpfr_div(r461, r459, r460, MPFR_RNDN);
        return mpfr_get_d(r461, MPFR_RNDN);
}

static mpfr_t r462, r463, r464, r465, r466, r467, r468, r469, r470, r471;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r462);
        mpfr_init(r463);
        mpfr_init_set_str(r464, "1", 10, MPFR_RNDN);
        mpfr_init(r465);
        mpfr_init(r466);
        mpfr_init(r467);
        mpfr_init(r468);
        mpfr_init(r469);
        mpfr_init(r470);
        mpfr_init(r471);
}

double f_fm(double x) {
        mpfr_set_d(r462, x, MPFR_RNDN);
        mpfr_sin(r463, r462, MPFR_RNDN);
        ;
        mpfr_div(r465, r463, r464, MPFR_RNDN);
        mpfr_div(r466, r465, r464, MPFR_RNDN);
        mpfr_cos(r467, r462, MPFR_RNDN);
        mpfr_add(r468, r467, r464, MPFR_RNDN);
        mpfr_div(r469, r463, r468, MPFR_RNDN);
        mpfr_div(r470, r469, r463, MPFR_RNDN);
        mpfr_mul(r471, r466, r470, MPFR_RNDN);
        return mpfr_get_d(r471, MPFR_RNDN);
}

static mpfr_t r472, r473, r474, r475, r476, r477, r478, r479, r480, r481;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r472);
        mpfr_init(r473);
        mpfr_init_set_str(r474, "1", 10, MPFR_RNDN);
        mpfr_init(r475);
        mpfr_init(r476);
        mpfr_init(r477);
        mpfr_init(r478);
        mpfr_init(r479);
        mpfr_init(r480);
        mpfr_init(r481);
}

double f_dm(double x) {
        mpfr_set_d(r472, x, MPFR_RNDN);
        mpfr_sin(r473, r472, MPFR_RNDN);
        ;
        mpfr_div(r475, r473, r474, MPFR_RNDN);
        mpfr_div(r476, r475, r474, MPFR_RNDN);
        mpfr_cos(r477, r472, MPFR_RNDN);
        mpfr_add(r478, r477, r474, MPFR_RNDN);
        mpfr_div(r479, r473, r478, MPFR_RNDN);
        mpfr_div(r480, r479, r473, MPFR_RNDN);
        mpfr_mul(r481, r476, r480, MPFR_RNDN);
        return mpfr_get_d(r481, MPFR_RNDN);
}

