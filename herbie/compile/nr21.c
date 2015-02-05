#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.1";

double f_if(float x) {
        float r1955 = 1.0;
        float r1956 = x;
        float r1957 = r1956 + r1955;
        float r1958 = r1955 / r1957;
        float r1959 = r1955 / r1956;
        float r1960 = r1958 - r1959;
        return r1960;
}

double f_id(double x) {
        double r1961 = 1.0;
        double r1962 = x;
        double r1963 = r1962 + r1961;
        double r1964 = r1961 / r1963;
        double r1965 = r1961 / r1962;
        double r1966 = r1964 - r1965;
        return r1966;
}


double f_of(float x) {
        float r1967 = -1.0;
        float r1968 = x;
        float r1969 = r1968 * r1968;
        float r1970 = r1968 + r1969;
        float r1971 = r1967 / r1970;
        return r1971;
}

double f_od(double x) {
        double r1972 = -1.0;
        double r1973 = x;
        double r1974 = r1973 * r1973;
        double r1975 = r1973 + r1974;
        double r1976 = r1972 / r1975;
        return r1976;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1977, r1978, r1979, r1980, r1981, r1982;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1977, "1", 10, MPFR_RNDN);
        mpfr_init(r1978);
        mpfr_init(r1979);
        mpfr_init(r1980);
        mpfr_init(r1981);
        mpfr_init(r1982);
}

double f_im(double x) {
        ;
        mpfr_set_d(r1978, x, MPFR_RNDN);
        mpfr_add(r1979, r1978, r1977, MPFR_RNDN);
        mpfr_div(r1980, r1977, r1979, MPFR_RNDN);
        mpfr_div(r1981, r1977, r1978, MPFR_RNDN);
        mpfr_sub(r1982, r1980, r1981, MPFR_RNDN);
        return mpfr_get_d(r1982, MPFR_RNDN);
}

static mpfr_t r1983, r1984, r1985, r1986, r1987;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1983, "-1", 10, MPFR_RNDN);
        mpfr_init(r1984);
        mpfr_init(r1985);
        mpfr_init(r1986);
        mpfr_init(r1987);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r1984, x, MPFR_RNDN);
        mpfr_mul(r1985, r1984, r1984, MPFR_RNDN);
        mpfr_add(r1986, r1984, r1985, MPFR_RNDN);
        mpfr_div(r1987, r1983, r1986, MPFR_RNDN);
        return mpfr_get_d(r1987, MPFR_RNDN);
}

static mpfr_t r1988, r1989, r1990, r1991, r1992;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1988, "-1", 10, MPFR_RNDN);
        mpfr_init(r1989);
        mpfr_init(r1990);
        mpfr_init(r1991);
        mpfr_init(r1992);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r1989, x, MPFR_RNDN);
        mpfr_mul(r1990, r1989, r1989, MPFR_RNDN);
        mpfr_add(r1991, r1989, r1990, MPFR_RNDN);
        mpfr_div(r1992, r1988, r1991, MPFR_RNDN);
        return mpfr_get_d(r1992, MPFR_RNDN);
}

