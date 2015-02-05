#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.3";

double f_if(float eps) {
        float r2088 = 1.0;
        float r2089 = eps;
        float r2090 = r2088 - r2089;
        float r2091 = r2088 + r2089;
        float r2092 = r2090 / r2091;
        float r2093 = log(r2092);
        return r2093;
}

double f_id(double eps) {
        double r2094 = 1.0;
        double r2095 = eps;
        double r2096 = r2094 - r2095;
        double r2097 = r2094 + r2095;
        double r2098 = r2096 / r2097;
        double r2099 = log(r2098);
        return r2099;
}


double f_of(float eps) {
        float r2100 = -0.4;
        float r2101 = eps;
        float r2102 = 5.0;
        float r2103 = pow(r2101, r2102);
        float r2104 = r2100 * r2103;
        float r2105 = -2.0;
        float r2106 = r2105 * r2101;
        float r2107 = -0.6666666666666666;
        float r2108 = 3.0;
        float r2109 = pow(r2101, r2108);
        float r2110 = r2107 * r2109;
        float r2111 = r2106 + r2110;
        float r2112 = r2104 + r2111;
        return r2112;
}

double f_od(double eps) {
        double r2113 = -0.4;
        double r2114 = eps;
        double r2115 = r2114 * r2114;
        double r2116 = r2114 * r2115;
        double r2117 = r2115 * r2116;
        double r2118 = 1.0;
        double r2119 = r2117 * r2118;
        double r2120 = r2113 * r2119;
        double r2121 = -0.6666666666666666;
        double r2122 = r2116 * r2118;
        double r2123 = r2121 * r2122;
        double r2124 = -2.0;
        double r2125 = r2114 * r2118;
        double r2126 = r2124 * r2125;
        double r2127 = r2123 + r2126;
        double r2128 = r2120 + r2127;
        return r2128;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2129, r2130, r2131, r2132, r2133, r2134;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2129, "1", 10, MPFR_RNDN);
        mpfr_init(r2130);
        mpfr_init(r2131);
        mpfr_init(r2132);
        mpfr_init(r2133);
        mpfr_init(r2134);
}

double f_im(double eps) {
        ;
        mpfr_set_d(r2130, eps, MPFR_RNDN);
        mpfr_sub(r2131, r2129, r2130, MPFR_RNDN);
        mpfr_add(r2132, r2129, r2130, MPFR_RNDN);
        mpfr_div(r2133, r2131, r2132, MPFR_RNDN);
        mpfr_log(r2134, r2133, MPFR_RNDN);
        return mpfr_get_d(r2134, MPFR_RNDN);
}

static mpfr_t r2135, r2136, r2137, r2138, r2139, r2140, r2141, r2142, r2143, r2144, r2145, r2146, r2147;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2135, "-2/5", 10, MPFR_RNDN);
        mpfr_init(r2136);
        mpfr_init_set_str(r2137, "5", 10, MPFR_RNDN);
        mpfr_init(r2138);
        mpfr_init(r2139);
        mpfr_init_set_str(r2140, "-2", 10, MPFR_RNDN);
        mpfr_init(r2141);
        mpfr_init_set_str(r2142, "-2/3", 10, MPFR_RNDN);
        mpfr_init_set_str(r2143, "3", 10, MPFR_RNDN);
        mpfr_init(r2144);
        mpfr_init(r2145);
        mpfr_init(r2146);
        mpfr_init(r2147);
}

double f_fm(double eps) {
        ;
        mpfr_set_d(r2136, eps, MPFR_RNDN);
        ;
        mpfr_pow(r2138, r2136, r2137, MPFR_RNDN);
        mpfr_mul(r2139, r2135, r2138, MPFR_RNDN);
        ;
        mpfr_mul(r2141, r2140, r2136, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r2144, r2136, r2143, MPFR_RNDN);
        mpfr_mul(r2145, r2142, r2144, MPFR_RNDN);
        mpfr_add(r2146, r2141, r2145, MPFR_RNDN);
        mpfr_add(r2147, r2139, r2146, MPFR_RNDN);
        return mpfr_get_d(r2147, MPFR_RNDN);
}

static mpfr_t r2148, r2149, r2150, r2151, r2152, r2153, r2154, r2155, r2156, r2157, r2158, r2159, r2160, r2161, r2162, r2163;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2148, "-2/5", 10, MPFR_RNDN);
        mpfr_init(r2149);
        mpfr_init(r2150);
        mpfr_init(r2151);
        mpfr_init(r2152);
        mpfr_init_set_str(r2153, "1", 10, MPFR_RNDN);
        mpfr_init(r2154);
        mpfr_init(r2155);
        mpfr_init_set_str(r2156, "-2/3", 10, MPFR_RNDN);
        mpfr_init(r2157);
        mpfr_init(r2158);
        mpfr_init_set_str(r2159, "-2", 10, MPFR_RNDN);
        mpfr_init(r2160);
        mpfr_init(r2161);
        mpfr_init(r2162);
        mpfr_init(r2163);
}

double f_dm(double eps) {
        ;
        mpfr_set_d(r2149, eps, MPFR_RNDN);
        mpfr_mul(r2150, r2149, r2149, MPFR_RNDN);
        mpfr_mul(r2151, r2149, r2150, MPFR_RNDN);
        mpfr_mul(r2152, r2150, r2151, MPFR_RNDN);
        ;
        mpfr_mul(r2154, r2152, r2153, MPFR_RNDN);
        mpfr_mul(r2155, r2148, r2154, MPFR_RNDN);
        ;
        mpfr_mul(r2157, r2151, r2153, MPFR_RNDN);
        mpfr_mul(r2158, r2156, r2157, MPFR_RNDN);
        ;
        mpfr_mul(r2160, r2149, r2153, MPFR_RNDN);
        mpfr_mul(r2161, r2159, r2160, MPFR_RNDN);
        mpfr_add(r2162, r2158, r2161, MPFR_RNDN);
        mpfr_add(r2163, r2155, r2162, MPFR_RNDN);
        return mpfr_get_d(r2163, MPFR_RNDN);
}

