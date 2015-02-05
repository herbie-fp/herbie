#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.3";

double f_if(float x, float eps) {
        float r354 = x;
        float r355 = eps;
        float r356 = r354 + r355;
        float r357 = sin(r356);
        float r358 = sin(r354);
        float r359 = r357 - r358;
        return r359;
}

double f_id(double x, double eps) {
        double r360 = x;
        double r361 = eps;
        double r362 = r360 + r361;
        double r363 = sin(r362);
        double r364 = sin(r360);
        double r365 = r363 - r364;
        return r365;
}


double f_of(float x, float eps) {
        float r366 = eps;
        float r367 = sin(r366);
        float r368 = x;
        float r369 = cos(r368);
        float r370 = r367 * r369;
        float r371 = sin(r368);
        float r372 = cos(r366);
        float r373 = 1.0;
        float r374 = r372 - r373;
        float r375 = exp(r374);
        float r376 = log(r375);
        float r377 = r371 * r376;
        float r378 = r370 + r377;
        return r378;
}

double f_od(double x, double eps) {
        double r379 = eps;
        double r380 = sin(r379);
        double r381 = x;
        double r382 = cos(r381);
        double r383 = r380 * r382;
        double r384 = sin(r381);
        double r385 = cos(r379);
        double r386 = 1.0;
        double r387 = r385 - r386;
        double r388 = exp(r387);
        double r389 = log(r388);
        double r390 = r384 * r389;
        double r391 = r383 + r390;
        return r391;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r392, r393, r394, r395, r396, r397;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r392);
        mpfr_init(r393);
        mpfr_init(r394);
        mpfr_init(r395);
        mpfr_init(r396);
        mpfr_init(r397);
}

double f_im(double x, double eps) {
        mpfr_set_d(r392, x, MPFR_RNDN);
        mpfr_set_d(r393, eps, MPFR_RNDN);
        mpfr_add(r394, r392, r393, MPFR_RNDN);
        mpfr_sin(r395, r394, MPFR_RNDN);
        mpfr_sin(r396, r392, MPFR_RNDN);
        mpfr_sub(r397, r395, r396, MPFR_RNDN);
        return mpfr_get_d(r397, MPFR_RNDN);
}

static mpfr_t r398, r399, r400, r401, r402, r403, r404, r405, r406, r407, r408, r409, r410;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r398);
        mpfr_init(r399);
        mpfr_init(r400);
        mpfr_init(r401);
        mpfr_init(r402);
        mpfr_init(r403);
        mpfr_init(r404);
        mpfr_init_set_str(r405, "1", 10, MPFR_RNDN);
        mpfr_init(r406);
        mpfr_init(r407);
        mpfr_init(r408);
        mpfr_init(r409);
        mpfr_init(r410);
}

double f_fm(double x, double eps) {
        mpfr_set_d(r398, eps, MPFR_RNDN);
        mpfr_sin(r399, r398, MPFR_RNDN);
        mpfr_set_d(r400, x, MPFR_RNDN);
        mpfr_cos(r401, r400, MPFR_RNDN);
        mpfr_mul(r402, r399, r401, MPFR_RNDN);
        mpfr_sin(r403, r400, MPFR_RNDN);
        mpfr_cos(r404, r398, MPFR_RNDN);
        ;
        mpfr_sub(r406, r404, r405, MPFR_RNDN);
        mpfr_exp(r407, r406, MPFR_RNDN);
        mpfr_log(r408, r407, MPFR_RNDN);
        mpfr_mul(r409, r403, r408, MPFR_RNDN);
        mpfr_add(r410, r402, r409, MPFR_RNDN);
        return mpfr_get_d(r410, MPFR_RNDN);
}

static mpfr_t r411, r412, r413, r414, r415, r416, r417, r418, r419, r420, r421, r422, r423;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r411);
        mpfr_init(r412);
        mpfr_init(r413);
        mpfr_init(r414);
        mpfr_init(r415);
        mpfr_init(r416);
        mpfr_init(r417);
        mpfr_init_set_str(r418, "1", 10, MPFR_RNDN);
        mpfr_init(r419);
        mpfr_init(r420);
        mpfr_init(r421);
        mpfr_init(r422);
        mpfr_init(r423);
}

double f_dm(double x, double eps) {
        mpfr_set_d(r411, eps, MPFR_RNDN);
        mpfr_sin(r412, r411, MPFR_RNDN);
        mpfr_set_d(r413, x, MPFR_RNDN);
        mpfr_cos(r414, r413, MPFR_RNDN);
        mpfr_mul(r415, r412, r414, MPFR_RNDN);
        mpfr_sin(r416, r413, MPFR_RNDN);
        mpfr_cos(r417, r411, MPFR_RNDN);
        ;
        mpfr_sub(r419, r417, r418, MPFR_RNDN);
        mpfr_exp(r420, r419, MPFR_RNDN);
        mpfr_log(r421, r420, MPFR_RNDN);
        mpfr_mul(r422, r416, r421, MPFR_RNDN);
        mpfr_add(r423, r415, r422, MPFR_RNDN);
        return mpfr_get_d(r423, MPFR_RNDN);
}

