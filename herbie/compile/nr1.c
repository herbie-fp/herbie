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
        float r298 = 1.0;
        float r299 = r297 * r298;
        float r300 = r295 * r299;
        float r301 = -1.0;
        float r302 = r296 * r298;
        float r303 = r301 * r302;
        float r304 = r298 * r298;
        float r305 = r301 * r304;
        float r306 = r303 + r305;
        float r307 = r300 + r306;
        return r307;
}

double f_od(double x) {
        double r308 = -0.5;
        double r309 = x;
        double r310 = r309 * r309;
        double r311 = 1.0;
        double r312 = r310 * r311;
        double r313 = r308 * r312;
        double r314 = -1.0;
        double r315 = r309 * r311;
        double r316 = r314 * r315;
        double r317 = r311 * r311;
        double r318 = r314 * r317;
        double r319 = r316 + r318;
        double r320 = r313 + r319;
        return r320;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r321, r322, r323, r324, r325, r326, r327;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r321, "1", 10, MPFR_RNDN);
        mpfr_init(r322);
        mpfr_init(r323);
        mpfr_init(r324);
        mpfr_init(r325);
        mpfr_init(r326);
        mpfr_init(r327);
}

double f_im(double x) {
        ;
        mpfr_set_d(r322, x, MPFR_RNDN);
        mpfr_sub(r323, r321, r322, MPFR_RNDN);
        mpfr_log(r324, r323, MPFR_RNDN);
        mpfr_add(r325, r321, r322, MPFR_RNDN);
        mpfr_log(r326, r325, MPFR_RNDN);
        mpfr_div(r327, r324, r326, MPFR_RNDN);
        return mpfr_get_d(r327, MPFR_RNDN);
}

static mpfr_t r328, r329, r330, r331, r332, r333, r334, r335, r336, r337, r338, r339, r340;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r328, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r329);
        mpfr_init(r330);
        mpfr_init_set_str(r331, "1", 10, MPFR_RNDN);
        mpfr_init(r332);
        mpfr_init(r333);
        mpfr_init_set_str(r334, "-1", 10, MPFR_RNDN);
        mpfr_init(r335);
        mpfr_init(r336);
        mpfr_init(r337);
        mpfr_init(r338);
        mpfr_init(r339);
        mpfr_init(r340);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r329, x, MPFR_RNDN);
        mpfr_mul(r330, r329, r329, MPFR_RNDN);
        ;
        mpfr_mul(r332, r330, r331, MPFR_RNDN);
        mpfr_mul(r333, r328, r332, MPFR_RNDN);
        ;
        mpfr_mul(r335, r329, r331, MPFR_RNDN);
        mpfr_mul(r336, r334, r335, MPFR_RNDN);
        mpfr_mul(r337, r331, r331, MPFR_RNDN);
        mpfr_mul(r338, r334, r337, MPFR_RNDN);
        mpfr_add(r339, r336, r338, MPFR_RNDN);
        mpfr_add(r340, r333, r339, MPFR_RNDN);
        return mpfr_get_d(r340, MPFR_RNDN);
}

static mpfr_t r341, r342, r343, r344, r345, r346, r347, r348, r349, r350, r351, r352, r353;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r341, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r342);
        mpfr_init(r343);
        mpfr_init_set_str(r344, "1", 10, MPFR_RNDN);
        mpfr_init(r345);
        mpfr_init(r346);
        mpfr_init_set_str(r347, "-1", 10, MPFR_RNDN);
        mpfr_init(r348);
        mpfr_init(r349);
        mpfr_init(r350);
        mpfr_init(r351);
        mpfr_init(r352);
        mpfr_init(r353);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r342, x, MPFR_RNDN);
        mpfr_mul(r343, r342, r342, MPFR_RNDN);
        ;
        mpfr_mul(r345, r343, r344, MPFR_RNDN);
        mpfr_mul(r346, r341, r345, MPFR_RNDN);
        ;
        mpfr_mul(r348, r342, r344, MPFR_RNDN);
        mpfr_mul(r349, r347, r348, MPFR_RNDN);
        mpfr_mul(r350, r344, r344, MPFR_RNDN);
        mpfr_mul(r351, r347, r350, MPFR_RNDN);
        mpfr_add(r352, r349, r351, MPFR_RNDN);
        mpfr_add(r353, r346, r352, MPFR_RNDN);
        return mpfr_get_d(r353, MPFR_RNDN);
}

