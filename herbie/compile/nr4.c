#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.5";

double f_if(float N) {
        float r482 = N;
        float r483 = 1.0;
        float r484 = r482 + r483;
        float r485 = atan(r484);
        float r486 = atan(r482);
        float r487 = r485 - r486;
        return r487;
}

double f_id(double N) {
        double r488 = N;
        double r489 = 1.0;
        double r490 = r488 + r489;
        double r491 = atan(r490);
        double r492 = atan(r488);
        double r493 = r491 - r492;
        return r493;
}


double f_of(float N) {
        float r494 = 1.0;
        float r495 = r494 / r494;
        float r496 = r494 * r495;
        float r497 = N;
        float r498 = r494 / r497;
        float r499 = r494 / r498;
        float r500 = r494 * r499;
        float r501 = r497 * r497;
        float r502 = r494 / r501;
        float r503 = r494 / r502;
        float r504 = r494 * r503;
        float r505 = r500 + r504;
        float r506 = r496 + r505;
        float r507 = atan2(r494, r506);
        return r507;
}

double f_od(double N) {
        double r508 = 1.0;
        double r509 = r508 / r508;
        double r510 = r508 * r509;
        double r511 = N;
        double r512 = r508 / r511;
        double r513 = r508 / r512;
        double r514 = r508 * r513;
        double r515 = r511 * r511;
        double r516 = r508 / r515;
        double r517 = r508 / r516;
        double r518 = r508 * r517;
        double r519 = r514 + r518;
        double r520 = r510 + r519;
        double r521 = atan2(r508, r520);
        return r521;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r522, r523, r524, r525, r526, r527;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init(r522);
        mpfr_init_set_str(r523, "1", 10, MPFR_RNDN);
        mpfr_init(r524);
        mpfr_init(r525);
        mpfr_init(r526);
        mpfr_init(r527);
}

double f_im(double N) {
        mpfr_set_d(r522, N, MPFR_RNDN);
        ;
        mpfr_add(r524, r522, r523, MPFR_RNDN);
        mpfr_atan(r525, r524, MPFR_RNDN);
        mpfr_atan(r526, r522, MPFR_RNDN);
        mpfr_sub(r527, r525, r526, MPFR_RNDN);
        return mpfr_get_d(r527, MPFR_RNDN);
}

static mpfr_t r528, r529, r530, r531, r532, r533, r534, r535, r536, r537, r538, r539, r540, r541;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r528, "1", 10, MPFR_RNDN);
        mpfr_init(r529);
        mpfr_init(r530);
        mpfr_init(r531);
        mpfr_init(r532);
        mpfr_init(r533);
        mpfr_init(r534);
        mpfr_init(r535);
        mpfr_init(r536);
        mpfr_init(r537);
        mpfr_init(r538);
        mpfr_init(r539);
        mpfr_init(r540);
        mpfr_init(r541);
}

double f_fm(double N) {
        ;
        mpfr_div(r529, r528, r528, MPFR_RNDN);
        mpfr_mul(r530, r528, r529, MPFR_RNDN);
        mpfr_set_d(r531, N, MPFR_RNDN);
        mpfr_div(r532, r528, r531, MPFR_RNDN);
        mpfr_div(r533, r528, r532, MPFR_RNDN);
        mpfr_mul(r534, r528, r533, MPFR_RNDN);
        mpfr_mul(r535, r531, r531, MPFR_RNDN);
        mpfr_div(r536, r528, r535, MPFR_RNDN);
        mpfr_div(r537, r528, r536, MPFR_RNDN);
        mpfr_mul(r538, r528, r537, MPFR_RNDN);
        mpfr_add(r539, r534, r538, MPFR_RNDN);
        mpfr_add(r540, r530, r539, MPFR_RNDN);
        mpfr_atan2(r541, r528, r540, MPFR_RNDN);
        return mpfr_get_d(r541, MPFR_RNDN);
}

static mpfr_t r542, r543, r544, r545, r546, r547, r548, r549, r550, r551, r552, r553, r554, r555;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r542, "1", 10, MPFR_RNDN);
        mpfr_init(r543);
        mpfr_init(r544);
        mpfr_init(r545);
        mpfr_init(r546);
        mpfr_init(r547);
        mpfr_init(r548);
        mpfr_init(r549);
        mpfr_init(r550);
        mpfr_init(r551);
        mpfr_init(r552);
        mpfr_init(r553);
        mpfr_init(r554);
        mpfr_init(r555);
}

double f_dm(double N) {
        ;
        mpfr_div(r543, r542, r542, MPFR_RNDN);
        mpfr_mul(r544, r542, r543, MPFR_RNDN);
        mpfr_set_d(r545, N, MPFR_RNDN);
        mpfr_div(r546, r542, r545, MPFR_RNDN);
        mpfr_div(r547, r542, r546, MPFR_RNDN);
        mpfr_mul(r548, r542, r547, MPFR_RNDN);
        mpfr_mul(r549, r545, r545, MPFR_RNDN);
        mpfr_div(r550, r542, r549, MPFR_RNDN);
        mpfr_div(r551, r542, r550, MPFR_RNDN);
        mpfr_mul(r552, r542, r551, MPFR_RNDN);
        mpfr_add(r553, r548, r552, MPFR_RNDN);
        mpfr_add(r554, r544, r553, MPFR_RNDN);
        mpfr_atan2(r555, r542, r554, MPFR_RNDN);
        return mpfr_get_d(r555, MPFR_RNDN);
}

