#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.4";

double f_if(float x) {
        float r2249 = 2.0;
        float r2250 = x;
        float r2251 = r2249 * r2250;
        float r2252 = exp(r2251);
        float r2253 = 1.0;
        float r2254 = r2252 - r2253;
        float r2255 = exp(r2250);
        float r2256 = r2255 - r2253;
        float r2257 = r2254 / r2256;
        float r2258 = sqrt(r2257);
        return r2258;
}

double f_id(double x) {
        double r2259 = 2.0;
        double r2260 = x;
        double r2261 = r2259 * r2260;
        double r2262 = exp(r2261);
        double r2263 = 1.0;
        double r2264 = r2262 - r2263;
        double r2265 = exp(r2260);
        double r2266 = r2265 - r2263;
        double r2267 = r2264 / r2266;
        double r2268 = sqrt(r2267);
        return r2268;
}


double f_of(float x) {
        float r2269 = 0.5;
        float r2270 = x;
        float r2271 = r2270 * r2270;
        float r2272 = 1.0;
        float r2273 = r2271 * r2272;
        float r2274 = r2269 * r2273;
        float r2275 = r2270 * r2272;
        float r2276 = r2272 * r2275;
        float r2277 = 2.0;
        float r2278 = r2272 * r2272;
        float r2279 = r2277 * r2278;
        float r2280 = r2276 + r2279;
        float r2281 = r2274 + r2280;
        float r2282 = sqrt(r2281);
        return r2282;
}

double f_od(double x) {
        double r2283 = 0.5;
        double r2284 = x;
        double r2285 = r2284 * r2284;
        double r2286 = 1.0;
        double r2287 = r2285 * r2286;
        double r2288 = r2283 * r2287;
        double r2289 = r2284 * r2286;
        double r2290 = r2286 * r2289;
        double r2291 = 2.0;
        double r2292 = r2286 * r2286;
        double r2293 = r2291 * r2292;
        double r2294 = r2290 + r2293;
        double r2295 = r2288 + r2294;
        double r2296 = sqrt(r2295);
        return r2296;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2297, r2298, r2299, r2300, r2301, r2302, r2303, r2304, r2305, r2306;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2297, "2", 10, MPFR_RNDN);
        mpfr_init(r2298);
        mpfr_init(r2299);
        mpfr_init(r2300);
        mpfr_init_set_str(r2301, "1", 10, MPFR_RNDN);
        mpfr_init(r2302);
        mpfr_init(r2303);
        mpfr_init(r2304);
        mpfr_init(r2305);
        mpfr_init(r2306);
}

double f_im(double x) {
        ;
        mpfr_set_d(r2298, x, MPFR_RNDN);
        mpfr_mul(r2299, r2297, r2298, MPFR_RNDN);
        mpfr_exp(r2300, r2299, MPFR_RNDN);
        ;
        mpfr_sub(r2302, r2300, r2301, MPFR_RNDN);
        mpfr_exp(r2303, r2298, MPFR_RNDN);
        mpfr_sub(r2304, r2303, r2301, MPFR_RNDN);
        mpfr_div(r2305, r2302, r2304, MPFR_RNDN);
        mpfr_sqrt(r2306, r2305, MPFR_RNDN);
        return mpfr_get_d(r2306, MPFR_RNDN);
}

static mpfr_t r2307, r2308, r2309, r2310, r2311, r2312, r2313, r2314, r2315, r2316, r2317, r2318, r2319, r2320;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2307, "1/2", 10, MPFR_RNDN);
        mpfr_init(r2308);
        mpfr_init(r2309);
        mpfr_init_set_str(r2310, "1", 10, MPFR_RNDN);
        mpfr_init(r2311);
        mpfr_init(r2312);
        mpfr_init(r2313);
        mpfr_init(r2314);
        mpfr_init_set_str(r2315, "2", 10, MPFR_RNDN);
        mpfr_init(r2316);
        mpfr_init(r2317);
        mpfr_init(r2318);
        mpfr_init(r2319);
        mpfr_init(r2320);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r2308, x, MPFR_RNDN);
        mpfr_mul(r2309, r2308, r2308, MPFR_RNDN);
        ;
        mpfr_mul(r2311, r2309, r2310, MPFR_RNDN);
        mpfr_mul(r2312, r2307, r2311, MPFR_RNDN);
        mpfr_mul(r2313, r2308, r2310, MPFR_RNDN);
        mpfr_mul(r2314, r2310, r2313, MPFR_RNDN);
        ;
        mpfr_mul(r2316, r2310, r2310, MPFR_RNDN);
        mpfr_mul(r2317, r2315, r2316, MPFR_RNDN);
        mpfr_add(r2318, r2314, r2317, MPFR_RNDN);
        mpfr_add(r2319, r2312, r2318, MPFR_RNDN);
        mpfr_sqrt(r2320, r2319, MPFR_RNDN);
        return mpfr_get_d(r2320, MPFR_RNDN);
}

static mpfr_t r2321, r2322, r2323, r2324, r2325, r2326, r2327, r2328, r2329, r2330, r2331, r2332, r2333, r2334;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2321, "1/2", 10, MPFR_RNDN);
        mpfr_init(r2322);
        mpfr_init(r2323);
        mpfr_init_set_str(r2324, "1", 10, MPFR_RNDN);
        mpfr_init(r2325);
        mpfr_init(r2326);
        mpfr_init(r2327);
        mpfr_init(r2328);
        mpfr_init_set_str(r2329, "2", 10, MPFR_RNDN);
        mpfr_init(r2330);
        mpfr_init(r2331);
        mpfr_init(r2332);
        mpfr_init(r2333);
        mpfr_init(r2334);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r2322, x, MPFR_RNDN);
        mpfr_mul(r2323, r2322, r2322, MPFR_RNDN);
        ;
        mpfr_mul(r2325, r2323, r2324, MPFR_RNDN);
        mpfr_mul(r2326, r2321, r2325, MPFR_RNDN);
        mpfr_mul(r2327, r2322, r2324, MPFR_RNDN);
        mpfr_mul(r2328, r2324, r2327, MPFR_RNDN);
        ;
        mpfr_mul(r2330, r2324, r2324, MPFR_RNDN);
        mpfr_mul(r2331, r2329, r2330, MPFR_RNDN);
        mpfr_add(r2332, r2328, r2331, MPFR_RNDN);
        mpfr_add(r2333, r2326, r2332, MPFR_RNDN);
        mpfr_sqrt(r2334, r2333, MPFR_RNDN);
        return mpfr_get_d(r2334, MPFR_RNDN);
}

