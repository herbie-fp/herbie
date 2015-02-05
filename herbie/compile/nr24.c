#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.6";

double f_if(float N) {
        float r2171 = N;
        float r2172 = 1.0;
        float r2173 = r2171 + r2172;
        float r2174 = log(r2173);
        float r2175 = log(r2171);
        float r2176 = r2174 - r2175;
        return r2176;
}

double f_id(double N) {
        double r2177 = N;
        double r2178 = 1.0;
        double r2179 = r2177 + r2178;
        double r2180 = log(r2179);
        double r2181 = log(r2177);
        double r2182 = r2180 - r2181;
        return r2182;
}


double f_of(float N) {
        float r2183 = 0.3333333333333333;
        float r2184 = 1.0;
        float r2185 = N;
        float r2186 = r2185 * r2185;
        float r2187 = r2185 * r2186;
        float r2188 = r2184 / r2187;
        float r2189 = r2183 * r2188;
        float r2190 = -0.5;
        float r2191 = r2185 * r2185;
        float r2192 = r2184 / r2191;
        float r2193 = r2190 * r2192;
        float r2194 = r2184 / r2185;
        float r2195 = r2184 * r2194;
        float r2196 = r2193 + r2195;
        float r2197 = r2189 + r2196;
        return r2197;
}

double f_od(double N) {
        double r2198 = 0.3333333333333333;
        double r2199 = 1.0;
        double r2200 = N;
        double r2201 = r2200 * r2200;
        double r2202 = r2200 * r2201;
        double r2203 = r2199 / r2202;
        double r2204 = r2198 * r2203;
        double r2205 = -0.5;
        double r2206 = r2200 * r2200;
        double r2207 = r2199 / r2206;
        double r2208 = r2205 * r2207;
        double r2209 = r2199 / r2200;
        double r2210 = r2199 * r2209;
        double r2211 = r2208 + r2210;
        double r2212 = r2204 + r2211;
        return r2212;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2213, r2214, r2215, r2216, r2217, r2218;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init(r2213);
        mpfr_init_set_str(r2214, "1", 10, MPFR_RNDN);
        mpfr_init(r2215);
        mpfr_init(r2216);
        mpfr_init(r2217);
        mpfr_init(r2218);
}

double f_im(double N) {
        mpfr_set_d(r2213, N, MPFR_RNDN);
        ;
        mpfr_add(r2215, r2213, r2214, MPFR_RNDN);
        mpfr_log(r2216, r2215, MPFR_RNDN);
        mpfr_log(r2217, r2213, MPFR_RNDN);
        mpfr_sub(r2218, r2216, r2217, MPFR_RNDN);
        return mpfr_get_d(r2218, MPFR_RNDN);
}

static mpfr_t r2219, r2220, r2221, r2222, r2223, r2224, r2225, r2226, r2227, r2228, r2229, r2230, r2231, r2232, r2233;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2219, "1/3", 10, MPFR_RNDN);
        mpfr_init_set_str(r2220, "1", 10, MPFR_RNDN);
        mpfr_init(r2221);
        mpfr_init(r2222);
        mpfr_init(r2223);
        mpfr_init(r2224);
        mpfr_init(r2225);
        mpfr_init_set_str(r2226, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2227);
        mpfr_init(r2228);
        mpfr_init(r2229);
        mpfr_init(r2230);
        mpfr_init(r2231);
        mpfr_init(r2232);
        mpfr_init(r2233);
}

double f_fm(double N) {
        ;
        ;
        mpfr_set_d(r2221, N, MPFR_RNDN);
        mpfr_mul(r2222, r2221, r2221, MPFR_RNDN);
        mpfr_mul(r2223, r2221, r2222, MPFR_RNDN);
        mpfr_div(r2224, r2220, r2223, MPFR_RNDN);
        mpfr_mul(r2225, r2219, r2224, MPFR_RNDN);
        ;
        mpfr_mul(r2227, r2221, r2221, MPFR_RNDN);
        mpfr_div(r2228, r2220, r2227, MPFR_RNDN);
        mpfr_mul(r2229, r2226, r2228, MPFR_RNDN);
        mpfr_div(r2230, r2220, r2221, MPFR_RNDN);
        mpfr_mul(r2231, r2220, r2230, MPFR_RNDN);
        mpfr_add(r2232, r2229, r2231, MPFR_RNDN);
        mpfr_add(r2233, r2225, r2232, MPFR_RNDN);
        return mpfr_get_d(r2233, MPFR_RNDN);
}

static mpfr_t r2234, r2235, r2236, r2237, r2238, r2239, r2240, r2241, r2242, r2243, r2244, r2245, r2246, r2247, r2248;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2234, "1/3", 10, MPFR_RNDN);
        mpfr_init_set_str(r2235, "1", 10, MPFR_RNDN);
        mpfr_init(r2236);
        mpfr_init(r2237);
        mpfr_init(r2238);
        mpfr_init(r2239);
        mpfr_init(r2240);
        mpfr_init_set_str(r2241, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2242);
        mpfr_init(r2243);
        mpfr_init(r2244);
        mpfr_init(r2245);
        mpfr_init(r2246);
        mpfr_init(r2247);
        mpfr_init(r2248);
}

double f_dm(double N) {
        ;
        ;
        mpfr_set_d(r2236, N, MPFR_RNDN);
        mpfr_mul(r2237, r2236, r2236, MPFR_RNDN);
        mpfr_mul(r2238, r2236, r2237, MPFR_RNDN);
        mpfr_div(r2239, r2235, r2238, MPFR_RNDN);
        mpfr_mul(r2240, r2234, r2239, MPFR_RNDN);
        ;
        mpfr_mul(r2242, r2236, r2236, MPFR_RNDN);
        mpfr_div(r2243, r2235, r2242, MPFR_RNDN);
        mpfr_mul(r2244, r2241, r2243, MPFR_RNDN);
        mpfr_div(r2245, r2235, r2236, MPFR_RNDN);
        mpfr_mul(r2246, r2235, r2245, MPFR_RNDN);
        mpfr_add(r2247, r2244, r2246, MPFR_RNDN);
        mpfr_add(r2248, r2240, r2247, MPFR_RNDN);
        return mpfr_get_d(r2248, MPFR_RNDN);
}

