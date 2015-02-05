#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.3";

double f_if(float x, float eps) {
        float r346 = x;
        float r347 = eps;
        float r348 = r346 + r347;
        float r349 = sin(r348);
        float r350 = sin(r346);
        float r351 = r349 - r350;
        return r351;
}

double f_id(double x, double eps) {
        double r352 = x;
        double r353 = eps;
        double r354 = r352 + r353;
        double r355 = sin(r354);
        double r356 = sin(r352);
        double r357 = r355 - r356;
        return r357;
}


double f_of(float x, float eps) {
        float r358 = eps;
        float r359 = sin(r358);
        float r360 = x;
        float r361 = cos(r360);
        float r362 = r359 * r361;
        float r363 = cos(r358);
        float r364 = sin(r360);
        float r365 = r363 * r364;
        float r366 = r365 - r364;
        float r367 = r362 + r366;
        return r367;
}

double f_od(double x, double eps) {
        double r368 = eps;
        double r369 = sin(r368);
        double r370 = x;
        double r371 = cos(r370);
        double r372 = r369 * r371;
        double r373 = sin(r370);
        double r374 = cos(r368);
        double r375 = 1.0;
        double r376 = r374 - r375;
        double r377 = exp(r376);
        double r378 = log(r377);
        double r379 = r373 * r378;
        double r380 = r372 + r379;
        return r380;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r381, r382, r383, r384, r385, r386;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r381);
        mpfr_init(r382);
        mpfr_init(r383);
        mpfr_init(r384);
        mpfr_init(r385);
        mpfr_init(r386);
}

double f_im(double x, double eps) {
        mpfr_set_d(r381, x, MPFR_RNDN);
        mpfr_set_d(r382, eps, MPFR_RNDN);
        mpfr_add(r383, r381, r382, MPFR_RNDN);
        mpfr_sin(r384, r383, MPFR_RNDN);
        mpfr_sin(r385, r381, MPFR_RNDN);
        mpfr_sub(r386, r384, r385, MPFR_RNDN);
        return mpfr_get_d(r386, MPFR_RNDN);
}

static mpfr_t r387, r388, r389, r390, r391, r392, r393, r394, r395, r396;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r387);
        mpfr_init(r388);
        mpfr_init(r389);
        mpfr_init(r390);
        mpfr_init(r391);
        mpfr_init(r392);
        mpfr_init(r393);
        mpfr_init(r394);
        mpfr_init(r395);
        mpfr_init(r396);
}

double f_fm(double x, double eps) {
        mpfr_set_d(r387, eps, MPFR_RNDN);
        mpfr_sin(r388, r387, MPFR_RNDN);
        mpfr_set_d(r389, x, MPFR_RNDN);
        mpfr_cos(r390, r389, MPFR_RNDN);
        mpfr_mul(r391, r388, r390, MPFR_RNDN);
        mpfr_cos(r392, r387, MPFR_RNDN);
        mpfr_sin(r393, r389, MPFR_RNDN);
        mpfr_mul(r394, r392, r393, MPFR_RNDN);
        mpfr_sub(r395, r394, r393, MPFR_RNDN);
        mpfr_add(r396, r391, r395, MPFR_RNDN);
        return mpfr_get_d(r396, MPFR_RNDN);
}

static mpfr_t r397, r398, r399, r400, r401, r402, r403, r404, r405, r406, r407, r408, r409;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r397);
        mpfr_init(r398);
        mpfr_init(r399);
        mpfr_init(r400);
        mpfr_init(r401);
        mpfr_init(r402);
        mpfr_init(r403);
        mpfr_init_set_str(r404, "1", 10, MPFR_RNDN);
        mpfr_init(r405);
        mpfr_init(r406);
        mpfr_init(r407);
        mpfr_init(r408);
        mpfr_init(r409);
}

double f_dm(double x, double eps) {
        mpfr_set_d(r397, eps, MPFR_RNDN);
        mpfr_sin(r398, r397, MPFR_RNDN);
        mpfr_set_d(r399, x, MPFR_RNDN);
        mpfr_cos(r400, r399, MPFR_RNDN);
        mpfr_mul(r401, r398, r400, MPFR_RNDN);
        mpfr_sin(r402, r399, MPFR_RNDN);
        mpfr_cos(r403, r397, MPFR_RNDN);
        ;
        mpfr_sub(r405, r403, r404, MPFR_RNDN);
        mpfr_exp(r406, r405, MPFR_RNDN);
        mpfr_log(r407, r406, MPFR_RNDN);
        mpfr_mul(r408, r402, r407, MPFR_RNDN);
        mpfr_add(r409, r401, r408, MPFR_RNDN);
        return mpfr_get_d(r409, MPFR_RNDN);
}

