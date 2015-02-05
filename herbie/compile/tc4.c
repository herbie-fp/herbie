#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.5";

double f_if(float N) {
        float r468 = N;
        float r469 = 1.0;
        float r470 = r468 + r469;
        float r471 = atan(r470);
        float r472 = atan(r468);
        float r473 = r471 - r472;
        return r473;
}

double f_id(double N) {
        double r474 = N;
        double r475 = 1.0;
        double r476 = r474 + r475;
        double r477 = atan(r476);
        double r478 = atan(r474);
        double r479 = r477 - r478;
        return r479;
}


double f_of(float N) {
        float r480 = 1.0;
        float r481 = N;
        float r482 = r481 * r481;
        float r483 = r480 + r481;
        float r484 = r482 + r483;
        float r485 = atan2(r480, r484);
        return r485;
}

double f_od(double N) {
        double r486 = 1.0;
        double r487 = r486 / r486;
        double r488 = r486 * r487;
        double r489 = N;
        double r490 = r486 / r489;
        double r491 = r486 / r490;
        double r492 = r486 * r491;
        double r493 = r489 * r489;
        double r494 = r486 / r493;
        double r495 = r486 / r494;
        double r496 = r486 * r495;
        double r497 = r492 + r496;
        double r498 = r488 + r497;
        double r499 = atan2(r486, r498);
        return r499;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r500, r501, r502, r503, r504, r505;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init(r500);
        mpfr_init_set_str(r501, "1", 10, MPFR_RNDN);
        mpfr_init(r502);
        mpfr_init(r503);
        mpfr_init(r504);
        mpfr_init(r505);
}

double f_im(double N) {
        mpfr_set_d(r500, N, MPFR_RNDN);
        ;
        mpfr_add(r502, r500, r501, MPFR_RNDN);
        mpfr_atan(r503, r502, MPFR_RNDN);
        mpfr_atan(r504, r500, MPFR_RNDN);
        mpfr_sub(r505, r503, r504, MPFR_RNDN);
        return mpfr_get_d(r505, MPFR_RNDN);
}

static mpfr_t r506, r507, r508, r509, r510, r511;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r506, "1", 10, MPFR_RNDN);
        mpfr_init(r507);
        mpfr_init(r508);
        mpfr_init(r509);
        mpfr_init(r510);
        mpfr_init(r511);
}

double f_fm(double N) {
        ;
        mpfr_set_d(r507, N, MPFR_RNDN);
        mpfr_mul(r508, r507, r507, MPFR_RNDN);
        mpfr_add(r509, r506, r507, MPFR_RNDN);
        mpfr_add(r510, r508, r509, MPFR_RNDN);
        mpfr_atan2(r511, r506, r510, MPFR_RNDN);
        return mpfr_get_d(r511, MPFR_RNDN);
}

static mpfr_t r512, r513, r514, r515, r516, r517, r518, r519, r520, r521, r522, r523, r524, r525;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r512, "1", 10, MPFR_RNDN);
        mpfr_init(r513);
        mpfr_init(r514);
        mpfr_init(r515);
        mpfr_init(r516);
        mpfr_init(r517);
        mpfr_init(r518);
        mpfr_init(r519);
        mpfr_init(r520);
        mpfr_init(r521);
        mpfr_init(r522);
        mpfr_init(r523);
        mpfr_init(r524);
        mpfr_init(r525);
}

double f_dm(double N) {
        ;
        mpfr_div(r513, r512, r512, MPFR_RNDN);
        mpfr_mul(r514, r512, r513, MPFR_RNDN);
        mpfr_set_d(r515, N, MPFR_RNDN);
        mpfr_div(r516, r512, r515, MPFR_RNDN);
        mpfr_div(r517, r512, r516, MPFR_RNDN);
        mpfr_mul(r518, r512, r517, MPFR_RNDN);
        mpfr_mul(r519, r515, r515, MPFR_RNDN);
        mpfr_div(r520, r512, r519, MPFR_RNDN);
        mpfr_div(r521, r512, r520, MPFR_RNDN);
        mpfr_mul(r522, r512, r521, MPFR_RNDN);
        mpfr_add(r523, r518, r522, MPFR_RNDN);
        mpfr_add(r524, r514, r523, MPFR_RNDN);
        mpfr_atan2(r525, r512, r524, MPFR_RNDN);
        return mpfr_get_d(r525, MPFR_RNDN);
}

