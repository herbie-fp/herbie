#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.9";

double f_if(float x) {
        float r791 = 1.0;
        float r792 = x;
        float r793 = r791 / r792;
        float r794 = 1.0 / tan(r792);
        float r795 = r793 - r794;
        return r795;
}

double f_id(double x) {
        double r796 = 1.0;
        double r797 = x;
        double r798 = r796 / r797;
        double r799 = 1.0 / tan(r797);
        double r800 = r798 - r799;
        return r800;
}


double f_of(float x) {
        float r801 = 0.0021164021164021165;
        float r802 = x;
        float r803 = r802 * r802;
        float r804 = r802 * r803;
        float r805 = r803 * r804;
        float r806 = 1.0;
        float r807 = r805 * r806;
        float r808 = r801 * r807;
        float r809 = 0.022222222222222223;
        float r810 = r804 * r806;
        float r811 = r809 * r810;
        float r812 = 0.3333333333333333;
        float r813 = r802 * r806;
        float r814 = r812 * r813;
        float r815 = r811 + r814;
        float r816 = r808 + r815;
        return r816;
}

double f_od(double x) {
        double r817 = 0.0021164021164021165;
        double r818 = x;
        double r819 = r818 * r818;
        double r820 = r818 * r819;
        double r821 = r819 * r820;
        double r822 = 1.0;
        double r823 = r821 * r822;
        double r824 = r817 * r823;
        double r825 = 0.022222222222222223;
        double r826 = r820 * r822;
        double r827 = r825 * r826;
        double r828 = 0.3333333333333333;
        double r829 = r818 * r822;
        double r830 = r828 * r829;
        double r831 = r827 + r830;
        double r832 = r824 + r831;
        return r832;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r833, r834, r835, r836, r837;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r833, "1", 10, MPFR_RNDN);
        mpfr_init(r834);
        mpfr_init(r835);
        mpfr_init(r836);
        mpfr_init(r837);
}

double f_im(double x) {
        ;
        mpfr_set_d(r834, x, MPFR_RNDN);
        mpfr_div(r835, r833, r834, MPFR_RNDN);
        mpfr_cot(r836, r834, MPFR_RNDN);
        mpfr_sub(r837, r835, r836, MPFR_RNDN);
        return mpfr_get_d(r837, MPFR_RNDN);
}

static mpfr_t r838, r839, r840, r841, r842, r843, r844, r845, r846, r847, r848, r849, r850, r851, r852, r853;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r838, "2/945", 10, MPFR_RNDN);
        mpfr_init(r839);
        mpfr_init(r840);
        mpfr_init(r841);
        mpfr_init(r842);
        mpfr_init_set_str(r843, "1", 10, MPFR_RNDN);
        mpfr_init(r844);
        mpfr_init(r845);
        mpfr_init_set_str(r846, "1/45", 10, MPFR_RNDN);
        mpfr_init(r847);
        mpfr_init(r848);
        mpfr_init_set_str(r849, "1/3", 10, MPFR_RNDN);
        mpfr_init(r850);
        mpfr_init(r851);
        mpfr_init(r852);
        mpfr_init(r853);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r839, x, MPFR_RNDN);
        mpfr_mul(r840, r839, r839, MPFR_RNDN);
        mpfr_mul(r841, r839, r840, MPFR_RNDN);
        mpfr_mul(r842, r840, r841, MPFR_RNDN);
        ;
        mpfr_mul(r844, r842, r843, MPFR_RNDN);
        mpfr_mul(r845, r838, r844, MPFR_RNDN);
        ;
        mpfr_mul(r847, r841, r843, MPFR_RNDN);
        mpfr_mul(r848, r846, r847, MPFR_RNDN);
        ;
        mpfr_mul(r850, r839, r843, MPFR_RNDN);
        mpfr_mul(r851, r849, r850, MPFR_RNDN);
        mpfr_add(r852, r848, r851, MPFR_RNDN);
        mpfr_add(r853, r845, r852, MPFR_RNDN);
        return mpfr_get_d(r853, MPFR_RNDN);
}

static mpfr_t r854, r855, r856, r857, r858, r859, r860, r861, r862, r863, r864, r865, r866, r867, r868, r869;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r854, "2/945", 10, MPFR_RNDN);
        mpfr_init(r855);
        mpfr_init(r856);
        mpfr_init(r857);
        mpfr_init(r858);
        mpfr_init_set_str(r859, "1", 10, MPFR_RNDN);
        mpfr_init(r860);
        mpfr_init(r861);
        mpfr_init_set_str(r862, "1/45", 10, MPFR_RNDN);
        mpfr_init(r863);
        mpfr_init(r864);
        mpfr_init_set_str(r865, "1/3", 10, MPFR_RNDN);
        mpfr_init(r866);
        mpfr_init(r867);
        mpfr_init(r868);
        mpfr_init(r869);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r855, x, MPFR_RNDN);
        mpfr_mul(r856, r855, r855, MPFR_RNDN);
        mpfr_mul(r857, r855, r856, MPFR_RNDN);
        mpfr_mul(r858, r856, r857, MPFR_RNDN);
        ;
        mpfr_mul(r860, r858, r859, MPFR_RNDN);
        mpfr_mul(r861, r854, r860, MPFR_RNDN);
        ;
        mpfr_mul(r863, r857, r859, MPFR_RNDN);
        mpfr_mul(r864, r862, r863, MPFR_RNDN);
        ;
        mpfr_mul(r866, r855, r859, MPFR_RNDN);
        mpfr_mul(r867, r865, r866, MPFR_RNDN);
        mpfr_add(r868, r864, r867, MPFR_RNDN);
        mpfr_add(r869, r861, r868, MPFR_RNDN);
        return mpfr_get_d(r869, MPFR_RNDN);
}

