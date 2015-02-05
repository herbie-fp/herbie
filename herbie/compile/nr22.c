#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.4";

double f_if(float x) {
        float r1993 = x;
        float r1994 = 1.0;
        float r1995 = r1993 + r1994;
        float r1996 = 3.0;
        float r1997 = r1994 / r1996;
        float r1998 = pow(r1995, r1997);
        float r1999 = pow(r1993, r1997);
        float r2000 = r1998 - r1999;
        return r2000;
}

double f_id(double x) {
        double r2001 = x;
        double r2002 = 1.0;
        double r2003 = r2001 + r2002;
        double r2004 = 3.0;
        double r2005 = r2002 / r2004;
        double r2006 = pow(r2003, r2005);
        double r2007 = pow(r2001, r2005);
        double r2008 = r2006 - r2007;
        return r2008;
}


double f_of(float x) {
        float r2009 = x;
        float r2010 = 1.0;
        float r2011 = r2009 + r2010;
        float r2012 = 0.3333333333333333;
        float r2013 = pow(r2011, r2012);
        float r2014 = 3.0;
        float r2015 = pow(r2013, r2014);
        float r2016 = pow(r2009, r2012);
        float r2017 = pow(r2016, r2014);
        float r2018 = sqrt(r2017);
        float r2019 = r2018 * r2018;
        float r2020 = r2015 - r2019;
        float r2021 = r2013 * r2013;
        float r2022 = log(r2009);
        float r2023 = r2022 * r2012;
        float r2024 = exp(r2023);
        float r2025 = r2024 * r2024;
        float r2026 = r2013 * r2016;
        float r2027 = r2025 + r2026;
        float r2028 = r2021 + r2027;
        float r2029 = r2020 / r2028;
        return r2029;
}

double f_od(double x) {
        double r2030 = x;
        double r2031 = 1.0;
        double r2032 = r2030 + r2031;
        double r2033 = 0.3333333333333333;
        double r2034 = pow(r2032, r2033);
        double r2035 = 3.0;
        double r2036 = pow(r2034, r2035);
        double r2037 = pow(r2030, r2033);
        double r2038 = pow(r2037, r2035);
        double r2039 = sqrt(r2038);
        double r2040 = r2039 * r2039;
        double r2041 = r2036 - r2040;
        double r2042 = r2034 * r2034;
        double r2043 = log(r2030);
        double r2044 = r2043 * r2033;
        double r2045 = exp(r2044);
        double r2046 = r2045 * r2045;
        double r2047 = r2034 * r2037;
        double r2048 = r2046 + r2047;
        double r2049 = r2042 + r2048;
        double r2050 = r2041 / r2049;
        return r2050;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2051, r2052, r2053, r2054, r2055, r2056, r2057, r2058;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2051);
        mpfr_init_set_str(r2052, "1", 10, MPFR_RNDN);
        mpfr_init(r2053);
        mpfr_init_set_str(r2054, "3", 10, MPFR_RNDN);
        mpfr_init(r2055);
        mpfr_init(r2056);
        mpfr_init(r2057);
        mpfr_init(r2058);
}

double f_im(double x) {
        mpfr_set_d(r2051, x, MPFR_RNDN);
        ;
        mpfr_add(r2053, r2051, r2052, MPFR_RNDN);
        ;
        mpfr_div(r2055, r2052, r2054, MPFR_RNDN);
        mpfr_pow(r2056, r2053, r2055, MPFR_RNDN);
        mpfr_pow(r2057, r2051, r2055, MPFR_RNDN);
        mpfr_sub(r2058, r2056, r2057, MPFR_RNDN);
        return mpfr_get_d(r2058, MPFR_RNDN);
}

static mpfr_t r2059, r2060, r2061, r2062, r2063, r2064, r2065, r2066, r2067, r2068, r2069, r2070, r2071, r2072, r2073, r2074, r2075, r2076, r2077, r2078, r2079;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2059);
        mpfr_init_set_str(r2060, "1", 10, MPFR_RNDN);
        mpfr_init(r2061);
        mpfr_init_set_str(r2062, "1/3", 10, MPFR_RNDN);
        mpfr_init(r2063);
        mpfr_init_set_str(r2064, "3", 10, MPFR_RNDN);
        mpfr_init(r2065);
        mpfr_init(r2066);
        mpfr_init(r2067);
        mpfr_init(r2068);
        mpfr_init(r2069);
        mpfr_init(r2070);
        mpfr_init(r2071);
        mpfr_init(r2072);
        mpfr_init(r2073);
        mpfr_init(r2074);
        mpfr_init(r2075);
        mpfr_init(r2076);
        mpfr_init(r2077);
        mpfr_init(r2078);
        mpfr_init(r2079);
}

double f_fm(double x) {
        mpfr_set_d(r2059, x, MPFR_RNDN);
        ;
        mpfr_add(r2061, r2059, r2060, MPFR_RNDN);
        ;
        mpfr_pow(r2063, r2061, r2062, MPFR_RNDN);
        ;
        mpfr_pow(r2065, r2063, r2064, MPFR_RNDN);
        mpfr_pow(r2066, r2059, r2062, MPFR_RNDN);
        mpfr_pow(r2067, r2066, r2064, MPFR_RNDN);
        mpfr_sqrt(r2068, r2067, MPFR_RNDN);
        mpfr_mul(r2069, r2068, r2068, MPFR_RNDN);
        mpfr_sub(r2070, r2065, r2069, MPFR_RNDN);
        mpfr_mul(r2071, r2063, r2063, MPFR_RNDN);
        mpfr_log(r2072, r2059, MPFR_RNDN);
        mpfr_mul(r2073, r2072, r2062, MPFR_RNDN);
        mpfr_exp(r2074, r2073, MPFR_RNDN);
        mpfr_mul(r2075, r2074, r2074, MPFR_RNDN);
        mpfr_mul(r2076, r2063, r2066, MPFR_RNDN);
        mpfr_add(r2077, r2075, r2076, MPFR_RNDN);
        mpfr_add(r2078, r2071, r2077, MPFR_RNDN);
        mpfr_div(r2079, r2070, r2078, MPFR_RNDN);
        return mpfr_get_d(r2079, MPFR_RNDN);
}

static mpfr_t r2080, r2081, r2082, r2083, r2084, r2085, r2086, r2087, r2088, r2089, r2090, r2091, r2092, r2093, r2094, r2095, r2096, r2097, r2098, r2099, r2100;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2080);
        mpfr_init_set_str(r2081, "1", 10, MPFR_RNDN);
        mpfr_init(r2082);
        mpfr_init_set_str(r2083, "1/3", 10, MPFR_RNDN);
        mpfr_init(r2084);
        mpfr_init_set_str(r2085, "3", 10, MPFR_RNDN);
        mpfr_init(r2086);
        mpfr_init(r2087);
        mpfr_init(r2088);
        mpfr_init(r2089);
        mpfr_init(r2090);
        mpfr_init(r2091);
        mpfr_init(r2092);
        mpfr_init(r2093);
        mpfr_init(r2094);
        mpfr_init(r2095);
        mpfr_init(r2096);
        mpfr_init(r2097);
        mpfr_init(r2098);
        mpfr_init(r2099);
        mpfr_init(r2100);
}

double f_dm(double x) {
        mpfr_set_d(r2080, x, MPFR_RNDN);
        ;
        mpfr_add(r2082, r2080, r2081, MPFR_RNDN);
        ;
        mpfr_pow(r2084, r2082, r2083, MPFR_RNDN);
        ;
        mpfr_pow(r2086, r2084, r2085, MPFR_RNDN);
        mpfr_pow(r2087, r2080, r2083, MPFR_RNDN);
        mpfr_pow(r2088, r2087, r2085, MPFR_RNDN);
        mpfr_sqrt(r2089, r2088, MPFR_RNDN);
        mpfr_mul(r2090, r2089, r2089, MPFR_RNDN);
        mpfr_sub(r2091, r2086, r2090, MPFR_RNDN);
        mpfr_mul(r2092, r2084, r2084, MPFR_RNDN);
        mpfr_log(r2093, r2080, MPFR_RNDN);
        mpfr_mul(r2094, r2093, r2083, MPFR_RNDN);
        mpfr_exp(r2095, r2094, MPFR_RNDN);
        mpfr_mul(r2096, r2095, r2095, MPFR_RNDN);
        mpfr_mul(r2097, r2084, r2087, MPFR_RNDN);
        mpfr_add(r2098, r2096, r2097, MPFR_RNDN);
        mpfr_add(r2099, r2092, r2098, MPFR_RNDN);
        mpfr_div(r2100, r2091, r2099, MPFR_RNDN);
        return mpfr_get_d(r2100, MPFR_RNDN);
}

