#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.5";

double f_if(float x, float eps) {
        float r2101 = x;
        float r2102 = eps;
        float r2103 = r2101 + r2102;
        float r2104 = cos(r2103);
        float r2105 = cos(r2101);
        float r2106 = r2104 - r2105;
        return r2106;
}

double f_id(double x, double eps) {
        double r2107 = x;
        double r2108 = eps;
        double r2109 = r2107 + r2108;
        double r2110 = cos(r2109);
        double r2111 = cos(r2107);
        double r2112 = r2110 - r2111;
        return r2112;
}


double f_of(float x, float eps) {
        float r2113 = eps;
        float r2114 = cos(r2113);
        float r2115 = x;
        float r2116 = cos(r2115);
        float r2117 = r2114 * r2116;
        float r2118 = sin(r2113);
        float r2119 = sin(r2115);
        float r2120 = r2118 * r2119;
        float r2121 = exp(r2120);
        float r2122 = log(r2121);
        float r2123 = -r2122;
        float r2124 = r2123 - r2116;
        float r2125 = r2117 + r2124;
        return r2125;
}

double f_od(double x, double eps) {
        double r2126 = eps;
        double r2127 = cos(r2126);
        double r2128 = x;
        double r2129 = cos(r2128);
        double r2130 = r2127 * r2129;
        double r2131 = sin(r2126);
        double r2132 = sin(r2128);
        double r2133 = r2131 * r2132;
        double r2134 = exp(r2133);
        double r2135 = log(r2134);
        double r2136 = -r2135;
        double r2137 = r2136 - r2129;
        double r2138 = r2130 + r2137;
        return r2138;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2139, r2140, r2141, r2142, r2143, r2144;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2139);
        mpfr_init(r2140);
        mpfr_init(r2141);
        mpfr_init(r2142);
        mpfr_init(r2143);
        mpfr_init(r2144);
}

double f_im(double x, double eps) {
        mpfr_set_d(r2139, x, MPFR_RNDN);
        mpfr_set_d(r2140, eps, MPFR_RNDN);
        mpfr_add(r2141, r2139, r2140, MPFR_RNDN);
        mpfr_cos(r2142, r2141, MPFR_RNDN);
        mpfr_cos(r2143, r2139, MPFR_RNDN);
        mpfr_sub(r2144, r2142, r2143, MPFR_RNDN);
        return mpfr_get_d(r2144, MPFR_RNDN);
}

static mpfr_t r2145, r2146, r2147, r2148, r2149, r2150, r2151, r2152, r2153, r2154, r2155, r2156, r2157;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2145);
        mpfr_init(r2146);
        mpfr_init(r2147);
        mpfr_init(r2148);
        mpfr_init(r2149);
        mpfr_init(r2150);
        mpfr_init(r2151);
        mpfr_init(r2152);
        mpfr_init(r2153);
        mpfr_init(r2154);
        mpfr_init(r2155);
        mpfr_init(r2156);
        mpfr_init(r2157);
}

double f_fm(double x, double eps) {
        mpfr_set_d(r2145, eps, MPFR_RNDN);
        mpfr_cos(r2146, r2145, MPFR_RNDN);
        mpfr_set_d(r2147, x, MPFR_RNDN);
        mpfr_cos(r2148, r2147, MPFR_RNDN);
        mpfr_mul(r2149, r2146, r2148, MPFR_RNDN);
        mpfr_sin(r2150, r2145, MPFR_RNDN);
        mpfr_sin(r2151, r2147, MPFR_RNDN);
        mpfr_mul(r2152, r2150, r2151, MPFR_RNDN);
        mpfr_exp(r2153, r2152, MPFR_RNDN);
        mpfr_log(r2154, r2153, MPFR_RNDN);
        mpfr_neg(r2155, r2154, MPFR_RNDN);
        mpfr_sub(r2156, r2155, r2148, MPFR_RNDN);
        mpfr_add(r2157, r2149, r2156, MPFR_RNDN);
        return mpfr_get_d(r2157, MPFR_RNDN);
}

static mpfr_t r2158, r2159, r2160, r2161, r2162, r2163, r2164, r2165, r2166, r2167, r2168, r2169, r2170;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2158);
        mpfr_init(r2159);
        mpfr_init(r2160);
        mpfr_init(r2161);
        mpfr_init(r2162);
        mpfr_init(r2163);
        mpfr_init(r2164);
        mpfr_init(r2165);
        mpfr_init(r2166);
        mpfr_init(r2167);
        mpfr_init(r2168);
        mpfr_init(r2169);
        mpfr_init(r2170);
}

double f_dm(double x, double eps) {
        mpfr_set_d(r2158, eps, MPFR_RNDN);
        mpfr_cos(r2159, r2158, MPFR_RNDN);
        mpfr_set_d(r2160, x, MPFR_RNDN);
        mpfr_cos(r2161, r2160, MPFR_RNDN);
        mpfr_mul(r2162, r2159, r2161, MPFR_RNDN);
        mpfr_sin(r2163, r2158, MPFR_RNDN);
        mpfr_sin(r2164, r2160, MPFR_RNDN);
        mpfr_mul(r2165, r2163, r2164, MPFR_RNDN);
        mpfr_exp(r2166, r2165, MPFR_RNDN);
        mpfr_log(r2167, r2166, MPFR_RNDN);
        mpfr_neg(r2168, r2167, MPFR_RNDN);
        mpfr_sub(r2169, r2168, r2161, MPFR_RNDN);
        mpfr_add(r2170, r2162, r2169, MPFR_RNDN);
        return mpfr_get_d(r2170, MPFR_RNDN);
}

