#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.1";

double f_if(float x) {
        float r235 = x;
        float r236 = 1.0;
        float r237 = r235 + r236;
        float r238 = sqrt(r237);
        float r239 = sqrt(r235);
        float r240 = r238 - r239;
        return r240;
}

double f_id(double x) {
        double r241 = x;
        double r242 = 1.0;
        double r243 = r241 + r242;
        double r244 = sqrt(r243);
        double r245 = sqrt(r241);
        double r246 = r244 - r245;
        return r246;
}


double f_of(float x) {
        float r247 = 1.0;
        float r248 = x;
        float r249 = r248 + r247;
        float r250 = sqrt(r249);
        float r251 = sqrt(r248);
        float r252 = r250 + r251;
        float r253 = r247 / r252;
        return r253;
}

double f_od(double x) {
        double r254 = 1.0;
        double r255 = x;
        double r256 = r255 + r254;
        double r257 = sqrt(r256);
        double r258 = sqrt(r255);
        double r259 = r257 + r258;
        double r260 = r254 / r259;
        return r260;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r261, r262, r263, r264, r265, r266;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init(r261);
        mpfr_init_set_str(r262, "1", 10, MPFR_RNDN);
        mpfr_init(r263);
        mpfr_init(r264);
        mpfr_init(r265);
        mpfr_init(r266);
}

double f_im(double x) {
        mpfr_set_d(r261, x, MPFR_RNDN);
        ;
        mpfr_add(r263, r261, r262, MPFR_RNDN);
        mpfr_sqrt(r264, r263, MPFR_RNDN);
        mpfr_sqrt(r265, r261, MPFR_RNDN);
        mpfr_sub(r266, r264, r265, MPFR_RNDN);
        return mpfr_get_d(r266, MPFR_RNDN);
}

static mpfr_t r267, r268, r269, r270, r271, r272, r273;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r267, "1", 10, MPFR_RNDN);
        mpfr_init(r268);
        mpfr_init(r269);
        mpfr_init(r270);
        mpfr_init(r271);
        mpfr_init(r272);
        mpfr_init(r273);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r268, x, MPFR_RNDN);
        mpfr_add(r269, r268, r267, MPFR_RNDN);
        mpfr_sqrt(r270, r269, MPFR_RNDN);
        mpfr_sqrt(r271, r268, MPFR_RNDN);
        mpfr_add(r272, r270, r271, MPFR_RNDN);
        mpfr_div(r273, r267, r272, MPFR_RNDN);
        return mpfr_get_d(r273, MPFR_RNDN);
}

static mpfr_t r274, r275, r276, r277, r278, r279, r280;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init_set_str(r274, "1", 10, MPFR_RNDN);
        mpfr_init(r275);
        mpfr_init(r276);
        mpfr_init(r277);
        mpfr_init(r278);
        mpfr_init(r279);
        mpfr_init(r280);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r275, x, MPFR_RNDN);
        mpfr_add(r276, r275, r274, MPFR_RNDN);
        mpfr_sqrt(r277, r276, MPFR_RNDN);
        mpfr_sqrt(r278, r275, MPFR_RNDN);
        mpfr_add(r279, r277, r278, MPFR_RNDN);
        mpfr_div(r280, r274, r279, MPFR_RNDN);
        return mpfr_get_d(r280, MPFR_RNDN);
}

