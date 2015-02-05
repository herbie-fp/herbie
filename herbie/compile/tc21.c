#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.1";

double f_if(float x) {
        float r2753 = 1.0;
        float r2754 = x;
        float r2755 = r2754 + r2753;
        float r2756 = r2753 / r2755;
        float r2757 = r2753 / r2754;
        float r2758 = r2756 - r2757;
        return r2758;
}

double f_id(double x) {
        double r2759 = 1.0;
        double r2760 = x;
        double r2761 = r2760 + r2759;
        double r2762 = r2759 / r2761;
        double r2763 = r2759 / r2760;
        double r2764 = r2762 - r2763;
        return r2764;
}


double f_of(float x) {
        float r2765 = -1.0;
        float r2766 = x;
        float r2767 = r2766 * r2766;
        float r2768 = r2766 + r2767;
        float r2769 = r2765 / r2768;
        return r2769;
}

double f_od(double x) {
        double r2770 = -1.0;
        double r2771 = x;
        double r2772 = r2771 * r2771;
        double r2773 = r2771 + r2772;
        double r2774 = r2770 / r2773;
        return r2774;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2775, r2776, r2777, r2778, r2779, r2780;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2775, "1", 10, MPFR_RNDN);
        mpfr_init(r2776);
        mpfr_init(r2777);
        mpfr_init(r2778);
        mpfr_init(r2779);
        mpfr_init(r2780);
}

double f_im(double x) {
        ;
        mpfr_set_d(r2776, x, MPFR_RNDN);
        mpfr_add(r2777, r2776, r2775, MPFR_RNDN);
        mpfr_div(r2778, r2775, r2777, MPFR_RNDN);
        mpfr_div(r2779, r2775, r2776, MPFR_RNDN);
        mpfr_sub(r2780, r2778, r2779, MPFR_RNDN);
        return mpfr_get_d(r2780, MPFR_RNDN);
}

static mpfr_t r2781, r2782, r2783, r2784, r2785;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2781, "-1", 10, MPFR_RNDN);
        mpfr_init(r2782);
        mpfr_init(r2783);
        mpfr_init(r2784);
        mpfr_init(r2785);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r2782, x, MPFR_RNDN);
        mpfr_mul(r2783, r2782, r2782, MPFR_RNDN);
        mpfr_add(r2784, r2782, r2783, MPFR_RNDN);
        mpfr_div(r2785, r2781, r2784, MPFR_RNDN);
        return mpfr_get_d(r2785, MPFR_RNDN);
}

static mpfr_t r2786, r2787, r2788, r2789, r2790;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2786, "-1", 10, MPFR_RNDN);
        mpfr_init(r2787);
        mpfr_init(r2788);
        mpfr_init(r2789);
        mpfr_init(r2790);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r2787, x, MPFR_RNDN);
        mpfr_mul(r2788, r2787, r2787, MPFR_RNDN);
        mpfr_add(r2789, r2787, r2788, MPFR_RNDN);
        mpfr_div(r2790, r2786, r2789, MPFR_RNDN);
        return mpfr_get_d(r2790, MPFR_RNDN);
}

