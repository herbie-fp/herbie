#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.4";

double f_if(float x) {
        float r3221 = 2.0;
        float r3222 = x;
        float r3223 = r3221 * r3222;
        float r3224 = exp(r3223);
        float r3225 = 1.0;
        float r3226 = r3224 - r3225;
        float r3227 = exp(r3222);
        float r3228 = r3227 - r3225;
        float r3229 = r3226 / r3228;
        float r3230 = sqrt(r3229);
        return r3230;
}

double f_id(double x) {
        double r3231 = 2.0;
        double r3232 = x;
        double r3233 = r3231 * r3232;
        double r3234 = exp(r3233);
        double r3235 = 1.0;
        double r3236 = r3234 - r3235;
        double r3237 = exp(r3232);
        double r3238 = r3237 - r3235;
        double r3239 = r3236 / r3238;
        double r3240 = sqrt(r3239);
        return r3240;
}


double f_of(float x) {
        float r3241 = x;
        float r3242 = -0.005459911422803998;
        bool r3243 = r3241 < r3242;
        float r3244 = 1.0;
        float r3245 = exp(r3241);
        float r3246 = r3245 - r3244;
        float r3247 = 2.0;
        float r3248 = r3241 * r3247;
        float r3249 = exp(r3248);
        float r3250 = sqrt(r3249);
        float r3251 = r3250 - r3244;
        float r3252 = r3250 + r3244;
        float r3253 = r3251 * r3252;
        float r3254 = r3246 / r3253;
        float r3255 = r3244 / r3254;
        float r3256 = sqrt(r3255);
        float r3257 = 0.5;
        float r3258 = r3241 * r3241;
        float r3259 = r3257 * r3258;
        float r3260 = r3247 + r3259;
        float r3261 = r3241 + r3260;
        float r3262 = sqrt(r3261);
        float r3263 = r3243 ? r3256 : r3262;
        return r3263;
}

double f_od(double x) {
        double r3264 = x;
        double r3265 = -1.1677212521264974e-05;
        bool r3266 = r3264 < r3265;
        double r3267 = 1.0;
        double r3268 = 2.0;
        double r3269 = r3268 * r3264;
        double r3270 = exp(r3269);
        double r3271 = sqrt(r3270);
        double r3272 = r3267 + r3271;
        double r3273 = r3271 - r3267;
        double r3274 = r3272 * r3273;
        double r3275 = exp(r3264);
        double r3276 = r3275 - r3267;
        double r3277 = r3274 / r3276;
        double r3278 = sqrt(r3277);
        double r3279 = 0.5;
        double r3280 = r3264 * r3264;
        double r3281 = r3280 * r3267;
        double r3282 = r3279 * r3281;
        double r3283 = r3264 * r3267;
        double r3284 = r3267 * r3283;
        double r3285 = r3267 * r3267;
        double r3286 = r3268 * r3285;
        double r3287 = r3284 + r3286;
        double r3288 = r3282 + r3287;
        double r3289 = sqrt(r3288);
        double r3290 = r3266 ? r3278 : r3289;
        return r3290;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r3291, r3292, r3293, r3294, r3295, r3296, r3297, r3298, r3299, r3300;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init_set_str(r3291, "2", 10, MPFR_RNDN);
        mpfr_init(r3292);
        mpfr_init(r3293);
        mpfr_init(r3294);
        mpfr_init_set_str(r3295, "1", 10, MPFR_RNDN);
        mpfr_init(r3296);
        mpfr_init(r3297);
        mpfr_init(r3298);
        mpfr_init(r3299);
        mpfr_init(r3300);
}

double f_im(double x) {
        ;
        mpfr_set_d(r3292, x, MPFR_RNDN);
        mpfr_mul(r3293, r3291, r3292, MPFR_RNDN);
        mpfr_exp(r3294, r3293, MPFR_RNDN);
        ;
        mpfr_sub(r3296, r3294, r3295, MPFR_RNDN);
        mpfr_exp(r3297, r3292, MPFR_RNDN);
        mpfr_sub(r3298, r3297, r3295, MPFR_RNDN);
        mpfr_div(r3299, r3296, r3298, MPFR_RNDN);
        mpfr_sqrt(r3300, r3299, MPFR_RNDN);
        return mpfr_get_d(r3300, MPFR_RNDN);
}

static mpfr_t r3301, r3302, r3303, r3304, r3305, r3306, r3307, r3308, r3309, r3310, r3311, r3312, r3313, r3314, r3315, r3316, r3317, r3318, r3319, r3320, r3321, r3322, r3323;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3301);
        mpfr_init_set_str(r3302, "-0.005459911422803998", 10, MPFR_RNDN);
        mpfr_init(r3303);
        mpfr_init_set_str(r3304, "1", 10, MPFR_RNDN);
        mpfr_init(r3305);
        mpfr_init(r3306);
        mpfr_init_set_str(r3307, "2", 10, MPFR_RNDN);
        mpfr_init(r3308);
        mpfr_init(r3309);
        mpfr_init(r3310);
        mpfr_init(r3311);
        mpfr_init(r3312);
        mpfr_init(r3313);
        mpfr_init(r3314);
        mpfr_init(r3315);
        mpfr_init(r3316);
        mpfr_init_set_str(r3317, "1/2", 10, MPFR_RNDN);
        mpfr_init(r3318);
        mpfr_init(r3319);
        mpfr_init(r3320);
        mpfr_init(r3321);
        mpfr_init(r3322);
        mpfr_init(r3323);
}

double f_fm(double x) {
        mpfr_set_d(r3301, x, MPFR_RNDN);
        ;
        mpfr_set_si(r3303, mpfr_cmp(r3301, r3302) < 0, MPFR_RNDN);
        ;
        mpfr_exp(r3305, r3301, MPFR_RNDN);
        mpfr_sub(r3306, r3305, r3304, MPFR_RNDN);
        ;
        mpfr_mul(r3308, r3301, r3307, MPFR_RNDN);
        mpfr_exp(r3309, r3308, MPFR_RNDN);
        mpfr_sqrt(r3310, r3309, MPFR_RNDN);
        mpfr_sub(r3311, r3310, r3304, MPFR_RNDN);
        mpfr_add(r3312, r3310, r3304, MPFR_RNDN);
        mpfr_mul(r3313, r3311, r3312, MPFR_RNDN);
        mpfr_div(r3314, r3306, r3313, MPFR_RNDN);
        mpfr_div(r3315, r3304, r3314, MPFR_RNDN);
        mpfr_sqrt(r3316, r3315, MPFR_RNDN);
        ;
        mpfr_mul(r3318, r3301, r3301, MPFR_RNDN);
        mpfr_mul(r3319, r3317, r3318, MPFR_RNDN);
        mpfr_add(r3320, r3307, r3319, MPFR_RNDN);
        mpfr_add(r3321, r3301, r3320, MPFR_RNDN);
        mpfr_sqrt(r3322, r3321, MPFR_RNDN);
        if (mpfr_get_si(r3303, MPFR_RNDN)) { mpfr_set(r3323, r3316, MPFR_RNDN); } else { mpfr_set(r3323, r3322, MPFR_RNDN); };
        return mpfr_get_d(r3323, MPFR_RNDN);
}

static mpfr_t r3324, r3325, r3326, r3327, r3328, r3329, r3330, r3331, r3332, r3333, r3334, r3335, r3336, r3337, r3338, r3339, r3340, r3341, r3342, r3343, r3344, r3345, r3346, r3347, r3348, r3349, r3350;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3324);
        mpfr_init_set_str(r3325, "-1.1677212521264974e-05", 10, MPFR_RNDN);
        mpfr_init(r3326);
        mpfr_init_set_str(r3327, "1", 10, MPFR_RNDN);
        mpfr_init_set_str(r3328, "2", 10, MPFR_RNDN);
        mpfr_init(r3329);
        mpfr_init(r3330);
        mpfr_init(r3331);
        mpfr_init(r3332);
        mpfr_init(r3333);
        mpfr_init(r3334);
        mpfr_init(r3335);
        mpfr_init(r3336);
        mpfr_init(r3337);
        mpfr_init(r3338);
        mpfr_init_set_str(r3339, "1/2", 10, MPFR_RNDN);
        mpfr_init(r3340);
        mpfr_init(r3341);
        mpfr_init(r3342);
        mpfr_init(r3343);
        mpfr_init(r3344);
        mpfr_init(r3345);
        mpfr_init(r3346);
        mpfr_init(r3347);
        mpfr_init(r3348);
        mpfr_init(r3349);
        mpfr_init(r3350);
}

double f_dm(double x) {
        mpfr_set_d(r3324, x, MPFR_RNDN);
        ;
        mpfr_set_si(r3326, mpfr_cmp(r3324, r3325) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_mul(r3329, r3328, r3324, MPFR_RNDN);
        mpfr_exp(r3330, r3329, MPFR_RNDN);
        mpfr_sqrt(r3331, r3330, MPFR_RNDN);
        mpfr_add(r3332, r3327, r3331, MPFR_RNDN);
        mpfr_sub(r3333, r3331, r3327, MPFR_RNDN);
        mpfr_mul(r3334, r3332, r3333, MPFR_RNDN);
        mpfr_exp(r3335, r3324, MPFR_RNDN);
        mpfr_sub(r3336, r3335, r3327, MPFR_RNDN);
        mpfr_div(r3337, r3334, r3336, MPFR_RNDN);
        mpfr_sqrt(r3338, r3337, MPFR_RNDN);
        ;
        mpfr_mul(r3340, r3324, r3324, MPFR_RNDN);
        mpfr_mul(r3341, r3340, r3327, MPFR_RNDN);
        mpfr_mul(r3342, r3339, r3341, MPFR_RNDN);
        mpfr_mul(r3343, r3324, r3327, MPFR_RNDN);
        mpfr_mul(r3344, r3327, r3343, MPFR_RNDN);
        mpfr_mul(r3345, r3327, r3327, MPFR_RNDN);
        mpfr_mul(r3346, r3328, r3345, MPFR_RNDN);
        mpfr_add(r3347, r3344, r3346, MPFR_RNDN);
        mpfr_add(r3348, r3342, r3347, MPFR_RNDN);
        mpfr_sqrt(r3349, r3348, MPFR_RNDN);
        if (mpfr_get_si(r3326, MPFR_RNDN)) { mpfr_set(r3350, r3338, MPFR_RNDN); } else { mpfr_set(r3350, r3349, MPFR_RNDN); };
        return mpfr_get_d(r3350, MPFR_RNDN);
}

