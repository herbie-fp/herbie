#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE section 3.5";

double f_if(float a, float x) {
        float r2281 = a;
        float r2282 = x;
        float r2283 = r2281 * r2282;
        float r2284 = exp(r2283);
        float r2285 = 1.0;
        float r2286 = r2284 - r2285;
        return r2286;
}

double f_id(double a, double x) {
        double r2287 = a;
        double r2288 = x;
        double r2289 = r2287 * r2288;
        double r2290 = exp(r2289);
        double r2291 = 1.0;
        double r2292 = r2290 - r2291;
        return r2292;
}


double f_of(float a, float x) {
        float r2293 = a;
        float r2294 = -1956171.625;
        bool r2295 = r2293 < r2294;
        float r2296 = x;
        float r2297 = r2293 * r2296;
        float r2298 = exp(r2297);
        float r2299 = 1.0;
        float r2300 = r2298 + r2299;
        float r2301 = sqrt(r2300);
        float r2302 = r2298 / r2301;
        float r2303 = r2302 * r2302;
        float r2304 = r2299 + r2298;
        float r2305 = r2299 / r2304;
        float r2306 = r2303 - r2305;
        float r2307 = 132056928.0;
        bool r2308 = r2293 < r2307;
        float r2309 = r2298 * r2298;
        float r2310 = r2309 / r2300;
        float r2311 = sqrt(r2310);
        float r2312 = r2299 / r2300;
        float r2313 = sqrt(r2312);
        float r2314 = r2311 + r2313;
        float r2315 = 0.7071067811865475;
        float r2316 = r2296 * r2293;
        float r2317 = r2315 * r2316;
        float r2318 = 0.007365695637359847;
        float r2319 = 3.0;
        float r2320 = pow(r2296, r2319);
        float r2321 = pow(r2293, r2319);
        float r2322 = r2320 * r2321;
        float r2323 = r2318 * r2322;
        float r2324 = -0.0018414239093399564;
        float r2325 = 4.0;
        float r2326 = pow(r2296, r2325);
        float r2327 = pow(r2293, r2325);
        float r2328 = r2326 * r2327;
        float r2329 = r2324 * r2328;
        float r2330 = r2323 + r2329;
        float r2331 = r2317 + r2330;
        float r2332 = r2314 * r2331;
        float r2333 = r2308 ? r2332 : r2306;
        float r2334 = r2295 ? r2306 : r2333;
        return r2334;
}

double f_od(double a, double x) {
        double r2335 = x;
        double r2336 = -6.650817902801456e+47;
        bool r2337 = r2335 < r2336;
        double r2338 = 1.0;
        double r2339 = a;
        double r2340 = r2339 * r2335;
        double r2341 = exp(r2340);
        double r2342 = sqrt(r2341);
        double r2343 = r2338 + r2342;
        double r2344 = r2342 - r2338;
        double r2345 = r2343 * r2344;
        double r2346 = 1.5300927357873491e+74;
        bool r2347 = r2335 < r2346;
        double r2348 = 0.041666666666666664;
        double r2349 = r2335 * r2335;
        double r2350 = r2349 * r2349;
        double r2351 = r2339 * r2339;
        double r2352 = r2351 * r2351;
        double r2353 = r2352 * r2338;
        double r2354 = r2350 * r2353;
        double r2355 = r2348 * r2354;
        double r2356 = 0.16666666666666666;
        double r2357 = r2335 * r2349;
        double r2358 = r2339 * r2351;
        double r2359 = r2358 * r2338;
        double r2360 = r2357 * r2359;
        double r2361 = r2356 * r2360;
        double r2362 = r2339 * r2338;
        double r2363 = r2335 * r2362;
        double r2364 = r2338 * r2363;
        double r2365 = r2361 + r2364;
        double r2366 = r2355 + r2365;
        double r2367 = r2347 ? r2366 : r2345;
        double r2368 = r2337 ? r2345 : r2367;
        return r2368;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r2369, r2370, r2371, r2372, r2373, r2374;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r2369);
        mpfr_init(r2370);
        mpfr_init(r2371);
        mpfr_init(r2372);
        mpfr_init_set_str(r2373, "1", 10, MPFR_RNDN);
        mpfr_init(r2374);
}

double f_im(double a, double x) {
        mpfr_set_d(r2369, a, MPFR_RNDN);
        mpfr_set_d(r2370, x, MPFR_RNDN);
        mpfr_mul(r2371, r2369, r2370, MPFR_RNDN);
        mpfr_exp(r2372, r2371, MPFR_RNDN);
        ;
        mpfr_sub(r2374, r2372, r2373, MPFR_RNDN);
        return mpfr_get_d(r2374, MPFR_RNDN);
}

static mpfr_t r2375, r2376, r2377, r2378, r2379, r2380, r2381, r2382, r2383, r2384, r2385, r2386, r2387, r2388, r2389, r2390, r2391, r2392, r2393, r2394, r2395, r2396, r2397, r2398, r2399, r2400, r2401, r2402, r2403, r2404, r2405, r2406, r2407, r2408, r2409, r2410, r2411, r2412, r2413, r2414, r2415, r2416;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r2375);
        mpfr_init_set_str(r2376, "-1956171.625", 10, MPFR_RNDN);
        mpfr_init(r2377);
        mpfr_init(r2378);
        mpfr_init(r2379);
        mpfr_init(r2380);
        mpfr_init_set_str(r2381, "1", 10, MPFR_RNDN);
        mpfr_init(r2382);
        mpfr_init(r2383);
        mpfr_init(r2384);
        mpfr_init(r2385);
        mpfr_init(r2386);
        mpfr_init(r2387);
        mpfr_init(r2388);
        mpfr_init_set_str(r2389, "132056928.0", 10, MPFR_RNDN);
        mpfr_init(r2390);
        mpfr_init(r2391);
        mpfr_init(r2392);
        mpfr_init(r2393);
        mpfr_init(r2394);
        mpfr_init(r2395);
        mpfr_init(r2396);
        mpfr_init_set_str(r2397, "0.7071067811865475", 10, MPFR_RNDN);
        mpfr_init(r2398);
        mpfr_init(r2399);
        mpfr_init_set_str(r2400, "0.007365695637359847", 10, MPFR_RNDN);
        mpfr_init_set_str(r2401, "3", 10, MPFR_RNDN);
        mpfr_init(r2402);
        mpfr_init(r2403);
        mpfr_init(r2404);
        mpfr_init(r2405);
        mpfr_init_set_str(r2406, "-0.0018414239093399564", 10, MPFR_RNDN);
        mpfr_init_set_str(r2407, "4", 10, MPFR_RNDN);
        mpfr_init(r2408);
        mpfr_init(r2409);
        mpfr_init(r2410);
        mpfr_init(r2411);
        mpfr_init(r2412);
        mpfr_init(r2413);
        mpfr_init(r2414);
        mpfr_init(r2415);
        mpfr_init(r2416);
}

double f_fm(double a, double x) {
        mpfr_set_d(r2375, a, MPFR_RNDN);
        ;
        mpfr_set_si(r2377, mpfr_cmp(r2375, r2376) < 0, MPFR_RNDN);
        mpfr_set_d(r2378, x, MPFR_RNDN);
        mpfr_mul(r2379, r2375, r2378, MPFR_RNDN);
        mpfr_exp(r2380, r2379, MPFR_RNDN);
        ;
        mpfr_add(r2382, r2380, r2381, MPFR_RNDN);
        mpfr_sqrt(r2383, r2382, MPFR_RNDN);
        mpfr_div(r2384, r2380, r2383, MPFR_RNDN);
        mpfr_mul(r2385, r2384, r2384, MPFR_RNDN);
        mpfr_add(r2386, r2381, r2380, MPFR_RNDN);
        mpfr_div(r2387, r2381, r2386, MPFR_RNDN);
        mpfr_sub(r2388, r2385, r2387, MPFR_RNDN);
        ;
        mpfr_set_si(r2390, mpfr_cmp(r2375, r2389) < 0, MPFR_RNDN);
        mpfr_mul(r2391, r2380, r2380, MPFR_RNDN);
        mpfr_div(r2392, r2391, r2382, MPFR_RNDN);
        mpfr_sqrt(r2393, r2392, MPFR_RNDN);
        mpfr_div(r2394, r2381, r2382, MPFR_RNDN);
        mpfr_sqrt(r2395, r2394, MPFR_RNDN);
        mpfr_add(r2396, r2393, r2395, MPFR_RNDN);
        ;
        mpfr_mul(r2398, r2378, r2375, MPFR_RNDN);
        mpfr_mul(r2399, r2397, r2398, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r2402, r2378, r2401, MPFR_RNDN);
        mpfr_pow(r2403, r2375, r2401, MPFR_RNDN);
        mpfr_mul(r2404, r2402, r2403, MPFR_RNDN);
        mpfr_mul(r2405, r2400, r2404, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r2408, r2378, r2407, MPFR_RNDN);
        mpfr_pow(r2409, r2375, r2407, MPFR_RNDN);
        mpfr_mul(r2410, r2408, r2409, MPFR_RNDN);
        mpfr_mul(r2411, r2406, r2410, MPFR_RNDN);
        mpfr_add(r2412, r2405, r2411, MPFR_RNDN);
        mpfr_add(r2413, r2399, r2412, MPFR_RNDN);
        mpfr_mul(r2414, r2396, r2413, MPFR_RNDN);
        if (mpfr_get_si(r2390, MPFR_RNDN)) { mpfr_set(r2415, r2414, MPFR_RNDN); } else { mpfr_set(r2415, r2388, MPFR_RNDN); };
        if (mpfr_get_si(r2377, MPFR_RNDN)) { mpfr_set(r2416, r2388, MPFR_RNDN); } else { mpfr_set(r2416, r2415, MPFR_RNDN); };
        return mpfr_get_d(r2416, MPFR_RNDN);
}

static mpfr_t r2417, r2418, r2419, r2420, r2421, r2422, r2423, r2424, r2425, r2426, r2427, r2428, r2429, r2430, r2431, r2432, r2433, r2434, r2435, r2436, r2437, r2438, r2439, r2440, r2441, r2442, r2443, r2444, r2445, r2446, r2447, r2448, r2449, r2450;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r2417);
        mpfr_init_set_str(r2418, "-6.650817902801456e+47", 10, MPFR_RNDN);
        mpfr_init(r2419);
        mpfr_init_set_str(r2420, "1", 10, MPFR_RNDN);
        mpfr_init(r2421);
        mpfr_init(r2422);
        mpfr_init(r2423);
        mpfr_init(r2424);
        mpfr_init(r2425);
        mpfr_init(r2426);
        mpfr_init(r2427);
        mpfr_init_set_str(r2428, "1.5300927357873491e+74", 10, MPFR_RNDN);
        mpfr_init(r2429);
        mpfr_init_set_str(r2430, "1/24", 10, MPFR_RNDN);
        mpfr_init(r2431);
        mpfr_init(r2432);
        mpfr_init(r2433);
        mpfr_init(r2434);
        mpfr_init(r2435);
        mpfr_init(r2436);
        mpfr_init(r2437);
        mpfr_init_set_str(r2438, "1/6", 10, MPFR_RNDN);
        mpfr_init(r2439);
        mpfr_init(r2440);
        mpfr_init(r2441);
        mpfr_init(r2442);
        mpfr_init(r2443);
        mpfr_init(r2444);
        mpfr_init(r2445);
        mpfr_init(r2446);
        mpfr_init(r2447);
        mpfr_init(r2448);
        mpfr_init(r2449);
        mpfr_init(r2450);
}

double f_dm(double a, double x) {
        mpfr_set_d(r2417, x, MPFR_RNDN);
        ;
        mpfr_set_si(r2419, mpfr_cmp(r2417, r2418) < 0, MPFR_RNDN);
        ;
        mpfr_set_d(r2421, a, MPFR_RNDN);
        mpfr_mul(r2422, r2421, r2417, MPFR_RNDN);
        mpfr_exp(r2423, r2422, MPFR_RNDN);
        mpfr_sqrt(r2424, r2423, MPFR_RNDN);
        mpfr_add(r2425, r2420, r2424, MPFR_RNDN);
        mpfr_sub(r2426, r2424, r2420, MPFR_RNDN);
        mpfr_mul(r2427, r2425, r2426, MPFR_RNDN);
        ;
        mpfr_set_si(r2429, mpfr_cmp(r2417, r2428) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r2431, r2417, r2417, MPFR_RNDN);
        mpfr_mul(r2432, r2431, r2431, MPFR_RNDN);
        mpfr_mul(r2433, r2421, r2421, MPFR_RNDN);
        mpfr_mul(r2434, r2433, r2433, MPFR_RNDN);
        mpfr_mul(r2435, r2434, r2420, MPFR_RNDN);
        mpfr_mul(r2436, r2432, r2435, MPFR_RNDN);
        mpfr_mul(r2437, r2430, r2436, MPFR_RNDN);
        ;
        mpfr_mul(r2439, r2417, r2431, MPFR_RNDN);
        mpfr_mul(r2440, r2421, r2433, MPFR_RNDN);
        mpfr_mul(r2441, r2440, r2420, MPFR_RNDN);
        mpfr_mul(r2442, r2439, r2441, MPFR_RNDN);
        mpfr_mul(r2443, r2438, r2442, MPFR_RNDN);
        mpfr_mul(r2444, r2421, r2420, MPFR_RNDN);
        mpfr_mul(r2445, r2417, r2444, MPFR_RNDN);
        mpfr_mul(r2446, r2420, r2445, MPFR_RNDN);
        mpfr_add(r2447, r2443, r2446, MPFR_RNDN);
        mpfr_add(r2448, r2437, r2447, MPFR_RNDN);
        if (mpfr_get_si(r2429, MPFR_RNDN)) { mpfr_set(r2449, r2448, MPFR_RNDN); } else { mpfr_set(r2449, r2427, MPFR_RNDN); };
        if (mpfr_get_si(r2419, MPFR_RNDN)) { mpfr_set(r2450, r2427, MPFR_RNDN); } else { mpfr_set(r2450, r2449, MPFR_RNDN); };
        return mpfr_get_d(r2450, MPFR_RNDN);
}

