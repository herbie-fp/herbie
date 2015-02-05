#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.5";

double f_if(float x) {
        float r2335 = x;
        float r2336 = sin(r2335);
        float r2337 = r2335 - r2336;
        float r2338 = tan(r2335);
        float r2339 = r2335 - r2338;
        float r2340 = r2337 / r2339;
        return r2340;
}

double f_id(double x) {
        double r2341 = x;
        double r2342 = sin(r2341);
        double r2343 = r2341 - r2342;
        double r2344 = tan(r2341);
        double r2345 = r2341 - r2344;
        double r2346 = r2343 / r2345;
        return r2346;
}


double f_of(float x) {
        float r2347 = 0.015669642857142858;
        float r2348 = -0.5;
        float r2349 = exp(r2348);
        float r2350 = r2347 * r2349;
        float r2351 = x;
        float r2352 = r2351 * r2351;
        float r2353 = r2352 * r2352;
        float r2354 = 1.0;
        float r2355 = r2353 * r2354;
        float r2356 = r2350 * r2355;
        float r2357 = 0.225;
        float r2358 = r2357 * r2349;
        float r2359 = r2351 * r2351;
        float r2360 = r2359 * r2354;
        float r2361 = r2358 * r2360;
        float r2362 = r2354 * r2354;
        float r2363 = r2349 * r2362;
        float r2364 = r2361 + r2363;
        float r2365 = r2356 + r2364;
        float r2366 = log(r2365);
        return r2366;
}

double f_od(double x) {
        double r2367 = 0.015669642857142858;
        double r2368 = -0.5;
        double r2369 = exp(r2368);
        double r2370 = r2367 * r2369;
        double r2371 = x;
        double r2372 = r2371 * r2371;
        double r2373 = r2372 * r2372;
        double r2374 = 1.0;
        double r2375 = r2373 * r2374;
        double r2376 = r2370 * r2375;
        double r2377 = 0.225;
        double r2378 = r2377 * r2369;
        double r2379 = r2371 * r2371;
        double r2380 = r2379 * r2374;
        double r2381 = r2378 * r2380;
        double r2382 = r2374 * r2374;
        double r2383 = r2369 * r2382;
        double r2384 = r2381 + r2383;
        double r2385 = r2376 + r2384;
        double r2386 = log(r2385);
        return r2386;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2387, r2388, r2389, r2390, r2391, r2392;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2387);
        mpfr_init(r2388);
        mpfr_init(r2389);
        mpfr_init(r2390);
        mpfr_init(r2391);
        mpfr_init(r2392);
}

double f_im(double x) {
        mpfr_set_d(r2387, x, MPFR_RNDN);
        mpfr_sin(r2388, r2387, MPFR_RNDN);
        mpfr_sub(r2389, r2387, r2388, MPFR_RNDN);
        mpfr_tan(r2390, r2387, MPFR_RNDN);
        mpfr_sub(r2391, r2387, r2390, MPFR_RNDN);
        mpfr_div(r2392, r2389, r2391, MPFR_RNDN);
        return mpfr_get_d(r2392, MPFR_RNDN);
}

static mpfr_t r2393, r2394, r2395, r2396, r2397, r2398, r2399, r2400, r2401, r2402, r2403, r2404, r2405, r2406, r2407, r2408, r2409, r2410, r2411, r2412;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2393, "351/22400", 10, MPFR_RNDN);
        mpfr_init_set_str(r2394, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2395);
        mpfr_init(r2396);
        mpfr_init(r2397);
        mpfr_init(r2398);
        mpfr_init(r2399);
        mpfr_init_set_str(r2400, "1", 10, MPFR_RNDN);
        mpfr_init(r2401);
        mpfr_init(r2402);
        mpfr_init_set_str(r2403, "9/40", 10, MPFR_RNDN);
        mpfr_init(r2404);
        mpfr_init(r2405);
        mpfr_init(r2406);
        mpfr_init(r2407);
        mpfr_init(r2408);
        mpfr_init(r2409);
        mpfr_init(r2410);
        mpfr_init(r2411);
        mpfr_init(r2412);
}

double f_fm(double x) {
        ;
        ;
        mpfr_exp(r2395, r2394, MPFR_RNDN);
        mpfr_mul(r2396, r2393, r2395, MPFR_RNDN);
        mpfr_set_d(r2397, x, MPFR_RNDN);
        mpfr_mul(r2398, r2397, r2397, MPFR_RNDN);
        mpfr_mul(r2399, r2398, r2398, MPFR_RNDN);
        ;
        mpfr_mul(r2401, r2399, r2400, MPFR_RNDN);
        mpfr_mul(r2402, r2396, r2401, MPFR_RNDN);
        ;
        mpfr_mul(r2404, r2403, r2395, MPFR_RNDN);
        mpfr_mul(r2405, r2397, r2397, MPFR_RNDN);
        mpfr_mul(r2406, r2405, r2400, MPFR_RNDN);
        mpfr_mul(r2407, r2404, r2406, MPFR_RNDN);
        mpfr_mul(r2408, r2400, r2400, MPFR_RNDN);
        mpfr_mul(r2409, r2395, r2408, MPFR_RNDN);
        mpfr_add(r2410, r2407, r2409, MPFR_RNDN);
        mpfr_add(r2411, r2402, r2410, MPFR_RNDN);
        mpfr_log(r2412, r2411, MPFR_RNDN);
        return mpfr_get_d(r2412, MPFR_RNDN);
}

static mpfr_t r2413, r2414, r2415, r2416, r2417, r2418, r2419, r2420, r2421, r2422, r2423, r2424, r2425, r2426, r2427, r2428, r2429, r2430, r2431, r2432;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r2413, "351/22400", 10, MPFR_RNDN);
        mpfr_init_set_str(r2414, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2415);
        mpfr_init(r2416);
        mpfr_init(r2417);
        mpfr_init(r2418);
        mpfr_init(r2419);
        mpfr_init_set_str(r2420, "1", 10, MPFR_RNDN);
        mpfr_init(r2421);
        mpfr_init(r2422);
        mpfr_init_set_str(r2423, "9/40", 10, MPFR_RNDN);
        mpfr_init(r2424);
        mpfr_init(r2425);
        mpfr_init(r2426);
        mpfr_init(r2427);
        mpfr_init(r2428);
        mpfr_init(r2429);
        mpfr_init(r2430);
        mpfr_init(r2431);
        mpfr_init(r2432);
}

double f_dm(double x) {
        ;
        ;
        mpfr_exp(r2415, r2414, MPFR_RNDN);
        mpfr_mul(r2416, r2413, r2415, MPFR_RNDN);
        mpfr_set_d(r2417, x, MPFR_RNDN);
        mpfr_mul(r2418, r2417, r2417, MPFR_RNDN);
        mpfr_mul(r2419, r2418, r2418, MPFR_RNDN);
        ;
        mpfr_mul(r2421, r2419, r2420, MPFR_RNDN);
        mpfr_mul(r2422, r2416, r2421, MPFR_RNDN);
        ;
        mpfr_mul(r2424, r2423, r2415, MPFR_RNDN);
        mpfr_mul(r2425, r2417, r2417, MPFR_RNDN);
        mpfr_mul(r2426, r2425, r2420, MPFR_RNDN);
        mpfr_mul(r2427, r2424, r2426, MPFR_RNDN);
        mpfr_mul(r2428, r2420, r2420, MPFR_RNDN);
        mpfr_mul(r2429, r2415, r2428, MPFR_RNDN);
        mpfr_add(r2430, r2427, r2429, MPFR_RNDN);
        mpfr_add(r2431, r2422, r2430, MPFR_RNDN);
        mpfr_log(r2432, r2431, MPFR_RNDN);
        return mpfr_get_d(r2432, MPFR_RNDN);
}

