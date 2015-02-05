#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.2.1";

double f_if(float a, float b_2F2, float c) {
        float r2451 = b_2F2;
        float r2452 = -r2451;
        float r2453 = r2451 * r2451;
        float r2454 = a;
        float r2455 = c;
        float r2456 = r2454 * r2455;
        float r2457 = r2453 - r2456;
        float r2458 = sqrt(r2457);
        float r2459 = r2452 + r2458;
        float r2460 = r2459 / r2454;
        return r2460;
}

double f_id(double a, double b_2F2, double c) {
        double r2461 = b_2F2;
        double r2462 = -r2461;
        double r2463 = r2461 * r2461;
        double r2464 = a;
        double r2465 = c;
        double r2466 = r2464 * r2465;
        double r2467 = r2463 - r2466;
        double r2468 = sqrt(r2467);
        double r2469 = r2462 + r2468;
        double r2470 = r2469 / r2464;
        return r2470;
}


double f_of(float a, float b_2F2, float c) {
        float r2471 = b_2F2;
        float r2472 = 6.856087962983648e-13;
        bool r2473 = r2471 < r2472;
        float r2474 = -r2471;
        float r2475 = r2471 * r2471;
        float r2476 = c;
        float r2477 = a;
        float r2478 = r2476 * r2477;
        float r2479 = r2475 - r2478;
        float r2480 = sqrt(r2479);
        float r2481 = r2474 + r2480;
        float r2482 = 1.0;
        float r2483 = r2482 / r2477;
        float r2484 = r2481 * r2483;
        float r2485 = -0.5;
        float r2486 = r2476 / r2471;
        float r2487 = r2485 * r2486;
        float r2488 = r2473 ? r2484 : r2487;
        return r2488;
}

double f_od(double a, double b_2F2, double c) {
        double r2489 = b_2F2;
        double r2490 = 2.0569927817214236e-282;
        bool r2491 = r2489 < r2490;
        double r2492 = -r2489;
        double r2493 = r2489 * r2489;
        double r2494 = c;
        double r2495 = a;
        double r2496 = r2494 * r2495;
        double r2497 = r2493 - r2496;
        double r2498 = sqrt(r2497);
        double r2499 = r2492 + r2498;
        double r2500 = r2499 / r2495;
        double r2501 = 4.4464930377598826e+102;
        bool r2502 = r2489 < r2501;
        double r2503 = 1.0;
        double r2504 = r2503 * r2503;
        double r2505 = r2495 * r2504;
        double r2506 = r2494 * r2505;
        double r2507 = r2503 * r2506;
        double r2508 = r2495 * r2494;
        double r2509 = r2493 - r2508;
        double r2510 = sqrt(r2509);
        double r2511 = r2492 - r2510;
        double r2512 = r2507 / r2511;
        double r2513 = r2503 / r2495;
        double r2514 = r2512 * r2513;
        double r2515 = -0.5;
        double r2516 = r2503 / r2489;
        double r2517 = r2503 / r2494;
        double r2518 = r2516 / r2517;
        double r2519 = r2518 / r2503;
        double r2520 = r2515 * r2519;
        double r2521 = r2502 ? r2514 : r2520;
        double r2522 = r2491 ? r2500 : r2521;
        return r2522;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2523, r2524, r2525, r2526, r2527, r2528, r2529, r2530, r2531, r2532;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2523);
        mpfr_init(r2524);
        mpfr_init(r2525);
        mpfr_init(r2526);
        mpfr_init(r2527);
        mpfr_init(r2528);
        mpfr_init(r2529);
        mpfr_init(r2530);
        mpfr_init(r2531);
        mpfr_init(r2532);
}

double f_im(double a, double b_2F2, double c) {
        mpfr_set_d(r2523, b_2F2, MPFR_RNDN);
        mpfr_neg(r2524, r2523, MPFR_RNDN);
        mpfr_mul(r2525, r2523, r2523, MPFR_RNDN);
        mpfr_set_d(r2526, a, MPFR_RNDN);
        mpfr_set_d(r2527, c, MPFR_RNDN);
        mpfr_mul(r2528, r2526, r2527, MPFR_RNDN);
        mpfr_sub(r2529, r2525, r2528, MPFR_RNDN);
        mpfr_sqrt(r2530, r2529, MPFR_RNDN);
        mpfr_add(r2531, r2524, r2530, MPFR_RNDN);
        mpfr_div(r2532, r2531, r2526, MPFR_RNDN);
        return mpfr_get_d(r2532, MPFR_RNDN);
}

static mpfr_t r2533, r2534, r2535, r2536, r2537, r2538, r2539, r2540, r2541, r2542, r2543, r2544, r2545, r2546, r2547, r2548, r2549, r2550;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2533);
        mpfr_init_set_str(r2534, "6.856087962983648e-13", 10, MPFR_RNDN);
        mpfr_init(r2535);
        mpfr_init(r2536);
        mpfr_init(r2537);
        mpfr_init(r2538);
        mpfr_init(r2539);
        mpfr_init(r2540);
        mpfr_init(r2541);
        mpfr_init(r2542);
        mpfr_init(r2543);
        mpfr_init_set_str(r2544, "1", 10, MPFR_RNDN);
        mpfr_init(r2545);
        mpfr_init(r2546);
        mpfr_init_set_str(r2547, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2548);
        mpfr_init(r2549);
        mpfr_init(r2550);
}

double f_fm(double a, double b_2F2, double c) {
        mpfr_set_d(r2533, b_2F2, MPFR_RNDN);
        ;
        mpfr_set_si(r2535, mpfr_cmp(r2533, r2534) < 0, MPFR_RNDN);
        mpfr_neg(r2536, r2533, MPFR_RNDN);
        mpfr_mul(r2537, r2533, r2533, MPFR_RNDN);
        mpfr_set_d(r2538, c, MPFR_RNDN);
        mpfr_set_d(r2539, a, MPFR_RNDN);
        mpfr_mul(r2540, r2538, r2539, MPFR_RNDN);
        mpfr_sub(r2541, r2537, r2540, MPFR_RNDN);
        mpfr_sqrt(r2542, r2541, MPFR_RNDN);
        mpfr_add(r2543, r2536, r2542, MPFR_RNDN);
        ;
        mpfr_div(r2545, r2544, r2539, MPFR_RNDN);
        mpfr_mul(r2546, r2543, r2545, MPFR_RNDN);
        ;
        mpfr_div(r2548, r2538, r2533, MPFR_RNDN);
        mpfr_mul(r2549, r2547, r2548, MPFR_RNDN);
        if (mpfr_get_si(r2535, MPFR_RNDN)) { mpfr_set(r2550, r2546, MPFR_RNDN); } else { mpfr_set(r2550, r2549, MPFR_RNDN); };
        return mpfr_get_d(r2550, MPFR_RNDN);
}

static mpfr_t r2551, r2552, r2553, r2554, r2555, r2556, r2557, r2558, r2559, r2560, r2561, r2562, r2563, r2564, r2565, r2566, r2567, r2568, r2569, r2570, r2571, r2572, r2573, r2574, r2575, r2576, r2577, r2578, r2579, r2580, r2581, r2582, r2583, r2584;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2551);
        mpfr_init_set_str(r2552, "2.0569927817214236e-282", 10, MPFR_RNDN);
        mpfr_init(r2553);
        mpfr_init(r2554);
        mpfr_init(r2555);
        mpfr_init(r2556);
        mpfr_init(r2557);
        mpfr_init(r2558);
        mpfr_init(r2559);
        mpfr_init(r2560);
        mpfr_init(r2561);
        mpfr_init(r2562);
        mpfr_init_set_str(r2563, "4.4464930377598826e+102", 10, MPFR_RNDN);
        mpfr_init(r2564);
        mpfr_init_set_str(r2565, "1", 10, MPFR_RNDN);
        mpfr_init(r2566);
        mpfr_init(r2567);
        mpfr_init(r2568);
        mpfr_init(r2569);
        mpfr_init(r2570);
        mpfr_init(r2571);
        mpfr_init(r2572);
        mpfr_init(r2573);
        mpfr_init(r2574);
        mpfr_init(r2575);
        mpfr_init(r2576);
        mpfr_init_set_str(r2577, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2578);
        mpfr_init(r2579);
        mpfr_init(r2580);
        mpfr_init(r2581);
        mpfr_init(r2582);
        mpfr_init(r2583);
        mpfr_init(r2584);
}

double f_dm(double a, double b_2F2, double c) {
        mpfr_set_d(r2551, b_2F2, MPFR_RNDN);
        ;
        mpfr_set_si(r2553, mpfr_cmp(r2551, r2552) < 0, MPFR_RNDN);
        mpfr_neg(r2554, r2551, MPFR_RNDN);
        mpfr_mul(r2555, r2551, r2551, MPFR_RNDN);
        mpfr_set_d(r2556, c, MPFR_RNDN);
        mpfr_set_d(r2557, a, MPFR_RNDN);
        mpfr_mul(r2558, r2556, r2557, MPFR_RNDN);
        mpfr_sub(r2559, r2555, r2558, MPFR_RNDN);
        mpfr_sqrt(r2560, r2559, MPFR_RNDN);
        mpfr_add(r2561, r2554, r2560, MPFR_RNDN);
        mpfr_div(r2562, r2561, r2557, MPFR_RNDN);
        ;
        mpfr_set_si(r2564, mpfr_cmp(r2551, r2563) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r2566, r2565, r2565, MPFR_RNDN);
        mpfr_mul(r2567, r2557, r2566, MPFR_RNDN);
        mpfr_mul(r2568, r2556, r2567, MPFR_RNDN);
        mpfr_mul(r2569, r2565, r2568, MPFR_RNDN);
        mpfr_mul(r2570, r2557, r2556, MPFR_RNDN);
        mpfr_sub(r2571, r2555, r2570, MPFR_RNDN);
        mpfr_sqrt(r2572, r2571, MPFR_RNDN);
        mpfr_sub(r2573, r2554, r2572, MPFR_RNDN);
        mpfr_div(r2574, r2569, r2573, MPFR_RNDN);
        mpfr_div(r2575, r2565, r2557, MPFR_RNDN);
        mpfr_mul(r2576, r2574, r2575, MPFR_RNDN);
        ;
        mpfr_div(r2578, r2565, r2551, MPFR_RNDN);
        mpfr_div(r2579, r2565, r2556, MPFR_RNDN);
        mpfr_div(r2580, r2578, r2579, MPFR_RNDN);
        mpfr_div(r2581, r2580, r2565, MPFR_RNDN);
        mpfr_mul(r2582, r2577, r2581, MPFR_RNDN);
        if (mpfr_get_si(r2564, MPFR_RNDN)) { mpfr_set(r2583, r2576, MPFR_RNDN); } else { mpfr_set(r2583, r2582, MPFR_RNDN); };
        if (mpfr_get_si(r2553, MPFR_RNDN)) { mpfr_set(r2584, r2562, MPFR_RNDN); } else { mpfr_set(r2584, r2583, MPFR_RNDN); };
        return mpfr_get_d(r2584, MPFR_RNDN);
}

