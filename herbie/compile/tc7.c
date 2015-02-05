#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.8";

double f_if(float N) {
        float r734 = N;
        float r735 = 1.0;
        float r736 = r734 + r735;
        float r737 = log(r736);
        float r738 = r736 * r737;
        float r739 = log(r734);
        float r740 = r734 * r739;
        float r741 = r738 - r740;
        float r742 = r741 - r735;
        return r742;
}

double f_id(double N) {
        double r743 = N;
        double r744 = 1.0;
        double r745 = r743 + r744;
        double r746 = log(r745);
        double r747 = r745 * r746;
        double r748 = log(r743);
        double r749 = r743 * r748;
        double r750 = r747 - r749;
        double r751 = r750 - r744;
        return r751;
}


double f_of(float N) {
        float r752 = N;
        float r753 = 15350702.5;
        bool r754 = r752 < r753;
        float r755 = log(r752);
        float r756 = r752 * r755;
        float r757 = 1.0;
        float r758 = r757 + r752;
        float r759 = log(r758);
        float r760 = r758 * r759;
        float r761 = r756 + r760;
        float r762 = r760 - r756;
        float r763 = r761 * r762;
        float r764 = r759 * r758;
        float r765 = r756 + r764;
        float r766 = r763 / r765;
        float r767 = r766 - r757;
        float r768 = r755 * r755;
        float r769 = 0.6666666666666666;
        float r770 = r769 * r755;
        float r771 = r757 - r770;
        float r772 = r771 / r752;
        float r773 = 2.0;
        float r774 = r773 * r768;
        float r775 = r773 * r755;
        float r776 = r774 - r775;
        float r777 = r752 * r776;
        float r778 = r772 + r777;
        float r779 = r757 + r778;
        float r780 = r768 + r779;
        float r781 = 3.0;
        float r782 = r781 * r755;
        float r783 = r780 - r782;
        float r784 = log(r783);
        float r785 = r755 * r752;
        float r786 = r764 + r785;
        float r787 = log(r786);
        float r788 = r784 - r787;
        float r789 = exp(r788);
        float r790 = r789 - r757;
        float r791 = r754 ? r767 : r790;
        return r791;
}

double f_od(double N) {
        double r792 = N;
        double r793 = 936680027968248.0;
        bool r794 = r792 < r793;
        double r795 = 1.0;
        double r796 = r792 + r795;
        double r797 = log(r796);
        double r798 = r797 * r796;
        double r799 = log(r792);
        double r800 = r792 * r799;
        double r801 = r798 + r800;
        double r802 = r798 - r800;
        double r803 = r801 * r802;
        double r804 = r799 * r792;
        double r805 = r798 + r804;
        double r806 = r803 / r805;
        double r807 = r806 - r795;
        double r808 = 0.6666666666666666;
        double r809 = r808 * r799;
        double r810 = r795 - r809;
        double r811 = r795 / r792;
        double r812 = r810 * r811;
        double r813 = r799 * r799;
        double r814 = r813 + r795;
        double r815 = 3.0;
        double r816 = r815 * r799;
        double r817 = r814 - r816;
        double r818 = r795 / r795;
        double r819 = r817 * r818;
        double r820 = 2.0;
        double r821 = r820 * r813;
        double r822 = r820 * r799;
        double r823 = r821 - r822;
        double r824 = r795 / r811;
        double r825 = r823 * r824;
        double r826 = r819 + r825;
        double r827 = r812 + r826;
        double r828 = r795 + r792;
        double r829 = log(r828);
        double r830 = r829 * r828;
        double r831 = r800 + r830;
        double r832 = r827 / r831;
        double r833 = r832 - r795;
        double r834 = r794 ? r807 : r833;
        return r834;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r835, r836, r837, r838, r839, r840, r841, r842, r843;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(1424);
        mpfr_init(r835);
        mpfr_init_set_str(r836, "1", 10, MPFR_RNDN);
        mpfr_init(r837);
        mpfr_init(r838);
        mpfr_init(r839);
        mpfr_init(r840);
        mpfr_init(r841);
        mpfr_init(r842);
        mpfr_init(r843);
}

double f_im(double N) {
        mpfr_set_d(r835, N, MPFR_RNDN);
        ;
        mpfr_add(r837, r835, r836, MPFR_RNDN);
        mpfr_log(r838, r837, MPFR_RNDN);
        mpfr_mul(r839, r837, r838, MPFR_RNDN);
        mpfr_log(r840, r835, MPFR_RNDN);
        mpfr_mul(r841, r835, r840, MPFR_RNDN);
        mpfr_sub(r842, r839, r841, MPFR_RNDN);
        mpfr_sub(r843, r842, r836, MPFR_RNDN);
        return mpfr_get_d(r843, MPFR_RNDN);
}

static mpfr_t r844, r845, r846, r847, r848, r849, r850, r851, r852, r853, r854, r855, r856, r857, r858, r859, r860, r861, r862, r863, r864, r865, r866, r867, r868, r869, r870, r871, r872, r873, r874, r875, r876, r877, r878, r879, r880, r881, r882, r883;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(1424);
        mpfr_init(r844);
        mpfr_init_set_str(r845, "15350702.5", 10, MPFR_RNDN);
        mpfr_init(r846);
        mpfr_init(r847);
        mpfr_init(r848);
        mpfr_init_set_str(r849, "1", 10, MPFR_RNDN);
        mpfr_init(r850);
        mpfr_init(r851);
        mpfr_init(r852);
        mpfr_init(r853);
        mpfr_init(r854);
        mpfr_init(r855);
        mpfr_init(r856);
        mpfr_init(r857);
        mpfr_init(r858);
        mpfr_init(r859);
        mpfr_init(r860);
        mpfr_init_set_str(r861, "2/3", 10, MPFR_RNDN);
        mpfr_init(r862);
        mpfr_init(r863);
        mpfr_init(r864);
        mpfr_init_set_str(r865, "2", 10, MPFR_RNDN);
        mpfr_init(r866);
        mpfr_init(r867);
        mpfr_init(r868);
        mpfr_init(r869);
        mpfr_init(r870);
        mpfr_init(r871);
        mpfr_init(r872);
        mpfr_init_set_str(r873, "3", 10, MPFR_RNDN);
        mpfr_init(r874);
        mpfr_init(r875);
        mpfr_init(r876);
        mpfr_init(r877);
        mpfr_init(r878);
        mpfr_init(r879);
        mpfr_init(r880);
        mpfr_init(r881);
        mpfr_init(r882);
        mpfr_init(r883);
}

double f_fm(double N) {
        mpfr_set_d(r844, N, MPFR_RNDN);
        ;
        mpfr_set_si(r846, mpfr_cmp(r844, r845) < 0, MPFR_RNDN);
        mpfr_log(r847, r844, MPFR_RNDN);
        mpfr_mul(r848, r844, r847, MPFR_RNDN);
        ;
        mpfr_add(r850, r849, r844, MPFR_RNDN);
        mpfr_log(r851, r850, MPFR_RNDN);
        mpfr_mul(r852, r850, r851, MPFR_RNDN);
        mpfr_add(r853, r848, r852, MPFR_RNDN);
        mpfr_sub(r854, r852, r848, MPFR_RNDN);
        mpfr_mul(r855, r853, r854, MPFR_RNDN);
        mpfr_mul(r856, r851, r850, MPFR_RNDN);
        mpfr_add(r857, r848, r856, MPFR_RNDN);
        mpfr_div(r858, r855, r857, MPFR_RNDN);
        mpfr_sub(r859, r858, r849, MPFR_RNDN);
        mpfr_mul(r860, r847, r847, MPFR_RNDN);
        ;
        mpfr_mul(r862, r861, r847, MPFR_RNDN);
        mpfr_sub(r863, r849, r862, MPFR_RNDN);
        mpfr_div(r864, r863, r844, MPFR_RNDN);
        ;
        mpfr_mul(r866, r865, r860, MPFR_RNDN);
        mpfr_mul(r867, r865, r847, MPFR_RNDN);
        mpfr_sub(r868, r866, r867, MPFR_RNDN);
        mpfr_mul(r869, r844, r868, MPFR_RNDN);
        mpfr_add(r870, r864, r869, MPFR_RNDN);
        mpfr_add(r871, r849, r870, MPFR_RNDN);
        mpfr_add(r872, r860, r871, MPFR_RNDN);
        ;
        mpfr_mul(r874, r873, r847, MPFR_RNDN);
        mpfr_sub(r875, r872, r874, MPFR_RNDN);
        mpfr_log(r876, r875, MPFR_RNDN);
        mpfr_mul(r877, r847, r844, MPFR_RNDN);
        mpfr_add(r878, r856, r877, MPFR_RNDN);
        mpfr_log(r879, r878, MPFR_RNDN);
        mpfr_sub(r880, r876, r879, MPFR_RNDN);
        mpfr_exp(r881, r880, MPFR_RNDN);
        mpfr_sub(r882, r881, r849, MPFR_RNDN);
        if (mpfr_get_si(r846, MPFR_RNDN)) { mpfr_set(r883, r859, MPFR_RNDN); } else { mpfr_set(r883, r882, MPFR_RNDN); };
        return mpfr_get_d(r883, MPFR_RNDN);
}

static mpfr_t r884, r885, r886, r887, r888, r889, r890, r891, r892, r893, r894, r895, r896, r897, r898, r899, r900, r901, r902, r903, r904, r905, r906, r907, r908, r909, r910, r911, r912, r913, r914, r915, r916, r917, r918, r919, r920, r921, r922, r923, r924, r925, r926;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(1424);
        mpfr_init(r884);
        mpfr_init_set_str(r885, "936680027968248.0", 10, MPFR_RNDN);
        mpfr_init(r886);
        mpfr_init_set_str(r887, "1", 10, MPFR_RNDN);
        mpfr_init(r888);
        mpfr_init(r889);
        mpfr_init(r890);
        mpfr_init(r891);
        mpfr_init(r892);
        mpfr_init(r893);
        mpfr_init(r894);
        mpfr_init(r895);
        mpfr_init(r896);
        mpfr_init(r897);
        mpfr_init(r898);
        mpfr_init(r899);
        mpfr_init_set_str(r900, "2/3", 10, MPFR_RNDN);
        mpfr_init(r901);
        mpfr_init(r902);
        mpfr_init(r903);
        mpfr_init(r904);
        mpfr_init(r905);
        mpfr_init(r906);
        mpfr_init_set_str(r907, "3", 10, MPFR_RNDN);
        mpfr_init(r908);
        mpfr_init(r909);
        mpfr_init(r910);
        mpfr_init(r911);
        mpfr_init_set_str(r912, "2", 10, MPFR_RNDN);
        mpfr_init(r913);
        mpfr_init(r914);
        mpfr_init(r915);
        mpfr_init(r916);
        mpfr_init(r917);
        mpfr_init(r918);
        mpfr_init(r919);
        mpfr_init(r920);
        mpfr_init(r921);
        mpfr_init(r922);
        mpfr_init(r923);
        mpfr_init(r924);
        mpfr_init(r925);
        mpfr_init(r926);
}

double f_dm(double N) {
        mpfr_set_d(r884, N, MPFR_RNDN);
        ;
        mpfr_set_si(r886, mpfr_cmp(r884, r885) < 0, MPFR_RNDN);
        ;
        mpfr_add(r888, r884, r887, MPFR_RNDN);
        mpfr_log(r889, r888, MPFR_RNDN);
        mpfr_mul(r890, r889, r888, MPFR_RNDN);
        mpfr_log(r891, r884, MPFR_RNDN);
        mpfr_mul(r892, r884, r891, MPFR_RNDN);
        mpfr_add(r893, r890, r892, MPFR_RNDN);
        mpfr_sub(r894, r890, r892, MPFR_RNDN);
        mpfr_mul(r895, r893, r894, MPFR_RNDN);
        mpfr_mul(r896, r891, r884, MPFR_RNDN);
        mpfr_add(r897, r890, r896, MPFR_RNDN);
        mpfr_div(r898, r895, r897, MPFR_RNDN);
        mpfr_sub(r899, r898, r887, MPFR_RNDN);
        ;
        mpfr_mul(r901, r900, r891, MPFR_RNDN);
        mpfr_sub(r902, r887, r901, MPFR_RNDN);
        mpfr_div(r903, r887, r884, MPFR_RNDN);
        mpfr_mul(r904, r902, r903, MPFR_RNDN);
        mpfr_mul(r905, r891, r891, MPFR_RNDN);
        mpfr_add(r906, r905, r887, MPFR_RNDN);
        ;
        mpfr_mul(r908, r907, r891, MPFR_RNDN);
        mpfr_sub(r909, r906, r908, MPFR_RNDN);
        mpfr_div(r910, r887, r887, MPFR_RNDN);
        mpfr_mul(r911, r909, r910, MPFR_RNDN);
        ;
        mpfr_mul(r913, r912, r905, MPFR_RNDN);
        mpfr_mul(r914, r912, r891, MPFR_RNDN);
        mpfr_sub(r915, r913, r914, MPFR_RNDN);
        mpfr_div(r916, r887, r903, MPFR_RNDN);
        mpfr_mul(r917, r915, r916, MPFR_RNDN);
        mpfr_add(r918, r911, r917, MPFR_RNDN);
        mpfr_add(r919, r904, r918, MPFR_RNDN);
        mpfr_add(r920, r887, r884, MPFR_RNDN);
        mpfr_log(r921, r920, MPFR_RNDN);
        mpfr_mul(r922, r921, r920, MPFR_RNDN);
        mpfr_add(r923, r892, r922, MPFR_RNDN);
        mpfr_div(r924, r919, r923, MPFR_RNDN);
        mpfr_sub(r925, r924, r887, MPFR_RNDN);
        if (mpfr_get_si(r886, MPFR_RNDN)) { mpfr_set(r926, r899, MPFR_RNDN); } else { mpfr_set(r926, r925, MPFR_RNDN); };
        return mpfr_get_d(r926, MPFR_RNDN);
}

