#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE p42";

double f_if(float a, float b, float c) {
        float r870 = b;
        float r871 = -r870;
        float r872 = r870 * r870;
        float r873 = 4.0;
        float r874 = a;
        float r875 = c;
        float r876 = r874 * r875;
        float r877 = r873 * r876;
        float r878 = r872 - r877;
        float r879 = sqrt(r878);
        float r880 = r871 - r879;
        float r881 = 2.0;
        float r882 = r881 * r874;
        float r883 = r880 / r882;
        return r883;
}

double f_id(double a, double b, double c) {
        double r884 = b;
        double r885 = -r884;
        double r886 = r884 * r884;
        double r887 = 4.0;
        double r888 = a;
        double r889 = c;
        double r890 = r888 * r889;
        double r891 = r887 * r890;
        double r892 = r886 - r891;
        double r893 = sqrt(r892);
        double r894 = r885 - r893;
        double r895 = 2.0;
        double r896 = r895 * r888;
        double r897 = r894 / r896;
        return r897;
}


double f_of(float a, float b, float c) {
        float r898 = 1.0;
        float r899 = a;
        float r900 = 2.0;
        float r901 = r899 * r900;
        float r902 = b;
        float r903 = -r902;
        float r904 = r902 * r902;
        float r905 = c;
        float r906 = 4.0;
        float r907 = r905 * r906;
        float r908 = r907 * r899;
        float r909 = r904 - r908;
        float r910 = sqrt(r909);
        float r911 = r903 - r910;
        float r912 = r901 / r911;
        float r913 = r898 / r912;
        return r913;
}

double f_od(double a, double b, double c) {
        double r914 = 1.0;
        double r915 = a;
        double r916 = 2.0;
        double r917 = r915 * r916;
        double r918 = b;
        double r919 = -r918;
        double r920 = r918 * r918;
        double r921 = c;
        double r922 = 4.0;
        double r923 = r921 * r922;
        double r924 = r923 * r915;
        double r925 = r920 - r924;
        double r926 = sqrt(r925);
        double r927 = r919 - r926;
        double r928 = r917 / r927;
        double r929 = r914 / r928;
        return r929;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r930, r931, r932, r933, r934, r935, r936, r937, r938, r939, r940, r941, r942, r943;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r930);
        mpfr_init(r931);
        mpfr_init(r932);
        mpfr_init_set_str(r933, "4", 10, MPFR_RNDN);
        mpfr_init(r934);
        mpfr_init(r935);
        mpfr_init(r936);
        mpfr_init(r937);
        mpfr_init(r938);
        mpfr_init(r939);
        mpfr_init(r940);
        mpfr_init_set_str(r941, "2", 10, MPFR_RNDN);
        mpfr_init(r942);
        mpfr_init(r943);
}

double f_im(double a, double b, double c) {
        mpfr_set_d(r930, b, MPFR_RNDN);
        mpfr_neg(r931, r930, MPFR_RNDN);
        mpfr_mul(r932, r930, r930, MPFR_RNDN);
        ;
        mpfr_set_d(r934, a, MPFR_RNDN);
        mpfr_set_d(r935, c, MPFR_RNDN);
        mpfr_mul(r936, r934, r935, MPFR_RNDN);
        mpfr_mul(r937, r933, r936, MPFR_RNDN);
        mpfr_sub(r938, r932, r937, MPFR_RNDN);
        mpfr_sqrt(r939, r938, MPFR_RNDN);
        mpfr_sub(r940, r931, r939, MPFR_RNDN);
        ;
        mpfr_mul(r942, r941, r934, MPFR_RNDN);
        mpfr_div(r943, r940, r942, MPFR_RNDN);
        return mpfr_get_d(r943, MPFR_RNDN);
}

static mpfr_t r944, r945, r946, r947, r948, r949, r950, r951, r952, r953, r954, r955, r956, r957, r958, r959;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r944, "1", 10, MPFR_RNDN);
        mpfr_init(r945);
        mpfr_init_set_str(r946, "2", 10, MPFR_RNDN);
        mpfr_init(r947);
        mpfr_init(r948);
        mpfr_init(r949);
        mpfr_init(r950);
        mpfr_init(r951);
        mpfr_init_set_str(r952, "4", 10, MPFR_RNDN);
        mpfr_init(r953);
        mpfr_init(r954);
        mpfr_init(r955);
        mpfr_init(r956);
        mpfr_init(r957);
        mpfr_init(r958);
        mpfr_init(r959);
}

double f_fm(double a, double b, double c) {
        ;
        mpfr_set_d(r945, a, MPFR_RNDN);
        ;
        mpfr_mul(r947, r945, r946, MPFR_RNDN);
        mpfr_set_d(r948, b, MPFR_RNDN);
        mpfr_neg(r949, r948, MPFR_RNDN);
        mpfr_mul(r950, r948, r948, MPFR_RNDN);
        mpfr_set_d(r951, c, MPFR_RNDN);
        ;
        mpfr_mul(r953, r951, r952, MPFR_RNDN);
        mpfr_mul(r954, r953, r945, MPFR_RNDN);
        mpfr_sub(r955, r950, r954, MPFR_RNDN);
        mpfr_sqrt(r956, r955, MPFR_RNDN);
        mpfr_sub(r957, r949, r956, MPFR_RNDN);
        mpfr_div(r958, r947, r957, MPFR_RNDN);
        mpfr_div(r959, r944, r958, MPFR_RNDN);
        return mpfr_get_d(r959, MPFR_RNDN);
}

static mpfr_t r960, r961, r962, r963, r964, r965, r966, r967, r968, r969, r970, r971, r972, r973, r974, r975;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r960, "1", 10, MPFR_RNDN);
        mpfr_init(r961);
        mpfr_init_set_str(r962, "2", 10, MPFR_RNDN);
        mpfr_init(r963);
        mpfr_init(r964);
        mpfr_init(r965);
        mpfr_init(r966);
        mpfr_init(r967);
        mpfr_init_set_str(r968, "4", 10, MPFR_RNDN);
        mpfr_init(r969);
        mpfr_init(r970);
        mpfr_init(r971);
        mpfr_init(r972);
        mpfr_init(r973);
        mpfr_init(r974);
        mpfr_init(r975);
}

double f_dm(double a, double b, double c) {
        ;
        mpfr_set_d(r961, a, MPFR_RNDN);
        ;
        mpfr_mul(r963, r961, r962, MPFR_RNDN);
        mpfr_set_d(r964, b, MPFR_RNDN);
        mpfr_neg(r965, r964, MPFR_RNDN);
        mpfr_mul(r966, r964, r964, MPFR_RNDN);
        mpfr_set_d(r967, c, MPFR_RNDN);
        ;
        mpfr_mul(r969, r967, r968, MPFR_RNDN);
        mpfr_mul(r970, r969, r961, MPFR_RNDN);
        mpfr_sub(r971, r966, r970, MPFR_RNDN);
        mpfr_sqrt(r972, r971, MPFR_RNDN);
        mpfr_sub(r973, r965, r972, MPFR_RNDN);
        mpfr_div(r974, r963, r973, MPFR_RNDN);
        mpfr_div(r975, r960, r974, MPFR_RNDN);
        return mpfr_get_d(r975, MPFR_RNDN);
}

