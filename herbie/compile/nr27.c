#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.6";

double f_if(float x, float n) {
        float r2433 = x;
        float r2434 = 1.0;
        float r2435 = r2433 + r2434;
        float r2436 = n;
        float r2437 = r2434 / r2436;
        float r2438 = pow(r2435, r2437);
        float r2439 = pow(r2433, r2437);
        float r2440 = r2438 - r2439;
        return r2440;
}

double f_id(double x, double n) {
        double r2441 = x;
        double r2442 = 1.0;
        double r2443 = r2441 + r2442;
        double r2444 = n;
        double r2445 = r2442 / r2444;
        double r2446 = pow(r2443, r2445);
        double r2447 = pow(r2441, r2445);
        double r2448 = r2446 - r2447;
        return r2448;
}


double f_of(float x, float n) {
        float r2449 = 1.0;
        float r2450 = x;
        float r2451 = r2449 + r2450;
        float r2452 = n;
        float r2453 = r2449 / r2452;
        float r2454 = pow(r2451, r2453);
        float r2455 = pow(r2450, r2453);
        float r2456 = r2454 - r2455;
        return r2456;
}

double f_od(double x, double n) {
        double r2457 = 1.0;
        double r2458 = x;
        double r2459 = r2457 + r2458;
        double r2460 = n;
        double r2461 = r2457 / r2460;
        double r2462 = pow(r2459, r2461);
        double r2463 = pow(r2458, r2461);
        double r2464 = r2462 - r2463;
        return r2464;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2465, r2466, r2467, r2468, r2469, r2470, r2471, r2472;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init(r2465);
        mpfr_init_set_str(r2466, "1", 10, MPFR_RNDN);
        mpfr_init(r2467);
        mpfr_init(r2468);
        mpfr_init(r2469);
        mpfr_init(r2470);
        mpfr_init(r2471);
        mpfr_init(r2472);
}

double f_im(double x, double n) {
        mpfr_set_d(r2465, x, MPFR_RNDN);
        ;
        mpfr_add(r2467, r2465, r2466, MPFR_RNDN);
        mpfr_set_d(r2468, n, MPFR_RNDN);
        mpfr_div(r2469, r2466, r2468, MPFR_RNDN);
        mpfr_pow(r2470, r2467, r2469, MPFR_RNDN);
        mpfr_pow(r2471, r2465, r2469, MPFR_RNDN);
        mpfr_sub(r2472, r2470, r2471, MPFR_RNDN);
        return mpfr_get_d(r2472, MPFR_RNDN);
}

static mpfr_t r2473, r2474, r2475, r2476, r2477, r2478, r2479, r2480;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2473, "1", 10, MPFR_RNDN);
        mpfr_init(r2474);
        mpfr_init(r2475);
        mpfr_init(r2476);
        mpfr_init(r2477);
        mpfr_init(r2478);
        mpfr_init(r2479);
        mpfr_init(r2480);
}

double f_fm(double x, double n) {
        ;
        mpfr_set_d(r2474, x, MPFR_RNDN);
        mpfr_add(r2475, r2473, r2474, MPFR_RNDN);
        mpfr_set_d(r2476, n, MPFR_RNDN);
        mpfr_div(r2477, r2473, r2476, MPFR_RNDN);
        mpfr_pow(r2478, r2475, r2477, MPFR_RNDN);
        mpfr_pow(r2479, r2474, r2477, MPFR_RNDN);
        mpfr_sub(r2480, r2478, r2479, MPFR_RNDN);
        return mpfr_get_d(r2480, MPFR_RNDN);
}

static mpfr_t r2481, r2482, r2483, r2484, r2485, r2486, r2487, r2488;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r2481, "1", 10, MPFR_RNDN);
        mpfr_init(r2482);
        mpfr_init(r2483);
        mpfr_init(r2484);
        mpfr_init(r2485);
        mpfr_init(r2486);
        mpfr_init(r2487);
        mpfr_init(r2488);
}

double f_dm(double x, double n) {
        ;
        mpfr_set_d(r2482, x, MPFR_RNDN);
        mpfr_add(r2483, r2481, r2482, MPFR_RNDN);
        mpfr_set_d(r2484, n, MPFR_RNDN);
        mpfr_div(r2485, r2481, r2484, MPFR_RNDN);
        mpfr_pow(r2486, r2483, r2485, MPFR_RNDN);
        mpfr_pow(r2487, r2482, r2485, MPFR_RNDN);
        mpfr_sub(r2488, r2486, r2487, MPFR_RNDN);
        return mpfr_get_d(r2488, MPFR_RNDN);
}

