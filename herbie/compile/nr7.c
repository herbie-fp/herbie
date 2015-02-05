#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.8";

double f_if(float N) {
        float r700 = N;
        float r701 = 1.0;
        float r702 = r700 + r701;
        float r703 = log(r702);
        float r704 = r702 * r703;
        float r705 = log(r700);
        float r706 = r700 * r705;
        float r707 = r704 - r706;
        float r708 = r707 - r701;
        return r708;
}

double f_id(double N) {
        double r709 = N;
        double r710 = 1.0;
        double r711 = r709 + r710;
        double r712 = log(r711);
        double r713 = r711 * r712;
        double r714 = log(r709);
        double r715 = r709 * r714;
        double r716 = r713 - r715;
        double r717 = r716 - r710;
        return r717;
}


double f_of(float N) {
        float r718 = N;
        float r719 = 1.0;
        float r720 = r718 + r719;
        float r721 = log(r720);
        float r722 = r721 * r720;
        float r723 = r722 * r722;
        float r724 = log(r718);
        float r725 = r718 * r724;
        float r726 = r725 * r725;
        float r727 = r723 - r726;
        float r728 = r719 + r718;
        float r729 = log(r728);
        float r730 = r729 * r728;
        float r731 = r725 + r730;
        float r732 = r727 / r731;
        float r733 = r732 - r719;
        return r733;
}

double f_od(double N) {
        double r734 = N;
        double r735 = 1.0;
        double r736 = r734 + r735;
        double r737 = log(r736);
        double r738 = r737 * r736;
        double r739 = r738 * r738;
        double r740 = log(r734);
        double r741 = r734 * r740;
        double r742 = r741 * r741;
        double r743 = r739 - r742;
        double r744 = r735 + r734;
        double r745 = log(r744);
        double r746 = r745 * r744;
        double r747 = r741 + r746;
        double r748 = r743 / r747;
        double r749 = r748 - r735;
        return r749;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r750, r751, r752, r753, r754, r755, r756, r757, r758;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init(r750);
        mpfr_init_set_str(r751, "1", 10, MPFR_RNDN);
        mpfr_init(r752);
        mpfr_init(r753);
        mpfr_init(r754);
        mpfr_init(r755);
        mpfr_init(r756);
        mpfr_init(r757);
        mpfr_init(r758);
}

double f_im(double N) {
        mpfr_set_d(r750, N, MPFR_RNDN);
        ;
        mpfr_add(r752, r750, r751, MPFR_RNDN);
        mpfr_log(r753, r752, MPFR_RNDN);
        mpfr_mul(r754, r752, r753, MPFR_RNDN);
        mpfr_log(r755, r750, MPFR_RNDN);
        mpfr_mul(r756, r750, r755, MPFR_RNDN);
        mpfr_sub(r757, r754, r756, MPFR_RNDN);
        mpfr_sub(r758, r757, r751, MPFR_RNDN);
        return mpfr_get_d(r758, MPFR_RNDN);
}

static mpfr_t r759, r760, r761, r762, r763, r764, r765, r766, r767, r768, r769, r770, r771, r772, r773, r774;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init(r759);
        mpfr_init_set_str(r760, "1", 10, MPFR_RNDN);
        mpfr_init(r761);
        mpfr_init(r762);
        mpfr_init(r763);
        mpfr_init(r764);
        mpfr_init(r765);
        mpfr_init(r766);
        mpfr_init(r767);
        mpfr_init(r768);
        mpfr_init(r769);
        mpfr_init(r770);
        mpfr_init(r771);
        mpfr_init(r772);
        mpfr_init(r773);
        mpfr_init(r774);
}

double f_fm(double N) {
        mpfr_set_d(r759, N, MPFR_RNDN);
        ;
        mpfr_add(r761, r759, r760, MPFR_RNDN);
        mpfr_log(r762, r761, MPFR_RNDN);
        mpfr_mul(r763, r762, r761, MPFR_RNDN);
        mpfr_mul(r764, r763, r763, MPFR_RNDN);
        mpfr_log(r765, r759, MPFR_RNDN);
        mpfr_mul(r766, r759, r765, MPFR_RNDN);
        mpfr_mul(r767, r766, r766, MPFR_RNDN);
        mpfr_sub(r768, r764, r767, MPFR_RNDN);
        mpfr_add(r769, r760, r759, MPFR_RNDN);
        mpfr_log(r770, r769, MPFR_RNDN);
        mpfr_mul(r771, r770, r769, MPFR_RNDN);
        mpfr_add(r772, r766, r771, MPFR_RNDN);
        mpfr_div(r773, r768, r772, MPFR_RNDN);
        mpfr_sub(r774, r773, r760, MPFR_RNDN);
        return mpfr_get_d(r774, MPFR_RNDN);
}

static mpfr_t r775, r776, r777, r778, r779, r780, r781, r782, r783, r784, r785, r786, r787, r788, r789, r790;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init(r775);
        mpfr_init_set_str(r776, "1", 10, MPFR_RNDN);
        mpfr_init(r777);
        mpfr_init(r778);
        mpfr_init(r779);
        mpfr_init(r780);
        mpfr_init(r781);
        mpfr_init(r782);
        mpfr_init(r783);
        mpfr_init(r784);
        mpfr_init(r785);
        mpfr_init(r786);
        mpfr_init(r787);
        mpfr_init(r788);
        mpfr_init(r789);
        mpfr_init(r790);
}

double f_dm(double N) {
        mpfr_set_d(r775, N, MPFR_RNDN);
        ;
        mpfr_add(r777, r775, r776, MPFR_RNDN);
        mpfr_log(r778, r777, MPFR_RNDN);
        mpfr_mul(r779, r778, r777, MPFR_RNDN);
        mpfr_mul(r780, r779, r779, MPFR_RNDN);
        mpfr_log(r781, r775, MPFR_RNDN);
        mpfr_mul(r782, r775, r781, MPFR_RNDN);
        mpfr_mul(r783, r782, r782, MPFR_RNDN);
        mpfr_sub(r784, r780, r783, MPFR_RNDN);
        mpfr_add(r785, r776, r775, MPFR_RNDN);
        mpfr_log(r786, r785, MPFR_RNDN);
        mpfr_mul(r787, r786, r785, MPFR_RNDN);
        mpfr_add(r788, r782, r787, MPFR_RNDN);
        mpfr_div(r789, r784, r788, MPFR_RNDN);
        mpfr_sub(r790, r789, r776, MPFR_RNDN);
        return mpfr_get_d(r790, MPFR_RNDN);
}

