#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.10";

double f_if(float x) {
        float r281 = 1.0;
        float r282 = x;
        float r283 = r281 - r282;
        float r284 = log(r283);
        float r285 = r281 + r282;
        float r286 = log(r285);
        float r287 = r284 / r286;
        return r287;
}

double f_id(double x) {
        double r288 = 1.0;
        double r289 = x;
        double r290 = r288 - r289;
        double r291 = log(r290);
        double r292 = r288 + r289;
        double r293 = log(r292);
        double r294 = r291 / r293;
        return r294;
}


double f_of(float x) {
        float r295 = -0.5;
        float r296 = x;
        float r297 = r296 * r296;
        float r298 = r295 * r297;
        float r299 = -1.0;
        float r300 = r299 * r296;
        float r301 = r298 + r300;
        float r302 = 1.0;
        float r303 = r301 - r302;
        return r303;
}

double f_od(double x) {
        double r304 = -0.5;
        double r305 = x;
        double r306 = r305 * r305;
        double r307 = 1.0;
        double r308 = r306 * r307;
        double r309 = r304 * r308;
        double r310 = -1.0;
        double r311 = r305 * r307;
        double r312 = r310 * r311;
        double r313 = r307 * r307;
        double r314 = r310 * r313;
        double r315 = r312 + r314;
        double r316 = r309 + r315;
        return r316;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r317, r318, r319, r320, r321, r322, r323;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r317, "1", 10, MPFR_RNDN);
        mpfr_init(r318);
        mpfr_init(r319);
        mpfr_init(r320);
        mpfr_init(r321);
        mpfr_init(r322);
        mpfr_init(r323);
}

double f_im(double x) {
        ;
        mpfr_set_d(r318, x, MPFR_RNDN);
        mpfr_sub(r319, r317, r318, MPFR_RNDN);
        mpfr_log(r320, r319, MPFR_RNDN);
        mpfr_add(r321, r317, r318, MPFR_RNDN);
        mpfr_log(r322, r321, MPFR_RNDN);
        mpfr_div(r323, r320, r322, MPFR_RNDN);
        return mpfr_get_d(r323, MPFR_RNDN);
}

static mpfr_t r324, r325, r326, r327, r328, r329, r330, r331, r332;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r324, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r325);
        mpfr_init(r326);
        mpfr_init(r327);
        mpfr_init_set_str(r328, "-1", 10, MPFR_RNDN);
        mpfr_init(r329);
        mpfr_init(r330);
        mpfr_init_set_str(r331, "1", 10, MPFR_RNDN);
        mpfr_init(r332);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r325, x, MPFR_RNDN);
        mpfr_mul(r326, r325, r325, MPFR_RNDN);
        mpfr_mul(r327, r324, r326, MPFR_RNDN);
        ;
        mpfr_mul(r329, r328, r325, MPFR_RNDN);
        mpfr_add(r330, r327, r329, MPFR_RNDN);
        ;
        mpfr_sub(r332, r330, r331, MPFR_RNDN);
        return mpfr_get_d(r332, MPFR_RNDN);
}

static mpfr_t r333, r334, r335, r336, r337, r338, r339, r340, r341, r342, r343, r344, r345;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r333, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r334);
        mpfr_init(r335);
        mpfr_init_set_str(r336, "1", 10, MPFR_RNDN);
        mpfr_init(r337);
        mpfr_init(r338);
        mpfr_init_set_str(r339, "-1", 10, MPFR_RNDN);
        mpfr_init(r340);
        mpfr_init(r341);
        mpfr_init(r342);
        mpfr_init(r343);
        mpfr_init(r344);
        mpfr_init(r345);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r334, x, MPFR_RNDN);
        mpfr_mul(r335, r334, r334, MPFR_RNDN);
        ;
        mpfr_mul(r337, r335, r336, MPFR_RNDN);
        mpfr_mul(r338, r333, r337, MPFR_RNDN);
        ;
        mpfr_mul(r340, r334, r336, MPFR_RNDN);
        mpfr_mul(r341, r339, r340, MPFR_RNDN);
        mpfr_mul(r342, r336, r336, MPFR_RNDN);
        mpfr_mul(r343, r339, r342, MPFR_RNDN);
        mpfr_add(r344, r341, r343, MPFR_RNDN);
        mpfr_add(r345, r338, r344, MPFR_RNDN);
        return mpfr_get_d(r345, MPFR_RNDN);
}

