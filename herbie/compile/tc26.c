#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.5";

double f_if(float x) {
        float r3351 = x;
        float r3352 = sin(r3351);
        float r3353 = r3351 - r3352;
        float r3354 = tan(r3351);
        float r3355 = r3351 - r3354;
        float r3356 = r3353 / r3355;
        return r3356;
}

double f_id(double x) {
        double r3357 = x;
        double r3358 = sin(r3357);
        double r3359 = r3357 - r3358;
        double r3360 = tan(r3357);
        double r3361 = r3357 - r3360;
        double r3362 = r3359 / r3361;
        return r3362;
}


double f_of(float x) {
        float r3363 = x;
        float r3364 = -0.3564379960298538;
        bool r3365 = r3363 < r3364;
        float r3366 = 1.0;
        float r3367 = sin(r3363);
        float r3368 = r3363 - r3367;
        float r3369 = r3363 / r3368;
        float r3370 = tan(r3363);
        float r3371 = r3368 / r3370;
        float r3372 = r3366 / r3371;
        float r3373 = r3369 - r3372;
        float r3374 = r3366 / r3373;
        float r3375 = 0.368852898478508;
        bool r3376 = r3363 < r3375;
        float r3377 = -0.009642857142857142;
        float r3378 = 4.0;
        float r3379 = pow(r3363, r3378);
        float r3380 = r3377 * r3379;
        float r3381 = 0.225;
        float r3382 = r3363 * r3363;
        float r3383 = r3381 * r3382;
        float r3384 = r3380 + r3383;
        float r3385 = 0.5;
        float r3386 = r3384 - r3385;
        float r3387 = r3376 ? r3386 : r3374;
        float r3388 = r3365 ? r3374 : r3387;
        return r3388;
}

double f_od(double x) {
        double r3389 = x;
        double r3390 = -0.025418290597484586;
        bool r3391 = r3389 < r3390;
        double r3392 = sin(r3389);
        double r3393 = r3389 - r3392;
        double r3394 = tan(r3389);
        double r3395 = r3389 - r3394;
        double r3396 = r3393 / r3395;
        double r3397 = exp(r3396);
        double r3398 = log(r3397);
        double r3399 = 0.029062913185972455;
        bool r3400 = r3389 < r3399;
        double r3401 = -0.009642857142857142;
        double r3402 = r3389 * r3389;
        double r3403 = r3402 * r3402;
        double r3404 = 1.0;
        double r3405 = r3403 * r3404;
        double r3406 = r3401 * r3405;
        double r3407 = 0.225;
        double r3408 = r3389 * r3389;
        double r3409 = r3408 * r3404;
        double r3410 = r3407 * r3409;
        double r3411 = -0.5;
        double r3412 = r3404 * r3404;
        double r3413 = r3411 * r3412;
        double r3414 = r3410 + r3413;
        double r3415 = r3406 + r3414;
        double r3416 = exp(r3415);
        double r3417 = log(r3416);
        double r3418 = r3400 ? r3417 : r3398;
        double r3419 = r3391 ? r3398 : r3418;
        return r3419;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r3420, r3421, r3422, r3423, r3424, r3425;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r3420);
        mpfr_init(r3421);
        mpfr_init(r3422);
        mpfr_init(r3423);
        mpfr_init(r3424);
        mpfr_init(r3425);
}

double f_im(double x) {
        mpfr_set_d(r3420, x, MPFR_RNDN);
        mpfr_sin(r3421, r3420, MPFR_RNDN);
        mpfr_sub(r3422, r3420, r3421, MPFR_RNDN);
        mpfr_tan(r3423, r3420, MPFR_RNDN);
        mpfr_sub(r3424, r3420, r3423, MPFR_RNDN);
        mpfr_div(r3425, r3422, r3424, MPFR_RNDN);
        return mpfr_get_d(r3425, MPFR_RNDN);
}

static mpfr_t r3426, r3427, r3428, r3429, r3430, r3431, r3432, r3433, r3434, r3435, r3436, r3437, r3438, r3439, r3440, r3441, r3442, r3443, r3444, r3445, r3446, r3447, r3448, r3449, r3450, r3451;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r3426);
        mpfr_init_set_str(r3427, "-0.3564379960298538", 10, MPFR_RNDN);
        mpfr_init(r3428);
        mpfr_init_set_str(r3429, "1", 10, MPFR_RNDN);
        mpfr_init(r3430);
        mpfr_init(r3431);
        mpfr_init(r3432);
        mpfr_init(r3433);
        mpfr_init(r3434);
        mpfr_init(r3435);
        mpfr_init(r3436);
        mpfr_init(r3437);
        mpfr_init_set_str(r3438, "0.368852898478508", 10, MPFR_RNDN);
        mpfr_init(r3439);
        mpfr_init_set_str(r3440, "-27/2800", 10, MPFR_RNDN);
        mpfr_init_set_str(r3441, "4", 10, MPFR_RNDN);
        mpfr_init(r3442);
        mpfr_init(r3443);
        mpfr_init_set_str(r3444, "9/40", 10, MPFR_RNDN);
        mpfr_init(r3445);
        mpfr_init(r3446);
        mpfr_init(r3447);
        mpfr_init_set_str(r3448, "1/2", 10, MPFR_RNDN);
        mpfr_init(r3449);
        mpfr_init(r3450);
        mpfr_init(r3451);
}

double f_fm(double x) {
        mpfr_set_d(r3426, x, MPFR_RNDN);
        ;
        mpfr_set_si(r3428, mpfr_cmp(r3426, r3427) < 0, MPFR_RNDN);
        ;
        mpfr_sin(r3430, r3426, MPFR_RNDN);
        mpfr_sub(r3431, r3426, r3430, MPFR_RNDN);
        mpfr_div(r3432, r3426, r3431, MPFR_RNDN);
        mpfr_tan(r3433, r3426, MPFR_RNDN);
        mpfr_div(r3434, r3431, r3433, MPFR_RNDN);
        mpfr_div(r3435, r3429, r3434, MPFR_RNDN);
        mpfr_sub(r3436, r3432, r3435, MPFR_RNDN);
        mpfr_div(r3437, r3429, r3436, MPFR_RNDN);
        ;
        mpfr_set_si(r3439, mpfr_cmp(r3426, r3438) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r3442, r3426, r3441, MPFR_RNDN);
        mpfr_mul(r3443, r3440, r3442, MPFR_RNDN);
        ;
        mpfr_mul(r3445, r3426, r3426, MPFR_RNDN);
        mpfr_mul(r3446, r3444, r3445, MPFR_RNDN);
        mpfr_add(r3447, r3443, r3446, MPFR_RNDN);
        ;
        mpfr_sub(r3449, r3447, r3448, MPFR_RNDN);
        if (mpfr_get_si(r3439, MPFR_RNDN)) { mpfr_set(r3450, r3449, MPFR_RNDN); } else { mpfr_set(r3450, r3437, MPFR_RNDN); };
        if (mpfr_get_si(r3428, MPFR_RNDN)) { mpfr_set(r3451, r3437, MPFR_RNDN); } else { mpfr_set(r3451, r3450, MPFR_RNDN); };
        return mpfr_get_d(r3451, MPFR_RNDN);
}

static mpfr_t r3452, r3453, r3454, r3455, r3456, r3457, r3458, r3459, r3460, r3461, r3462, r3463, r3464, r3465, r3466, r3467, r3468, r3469, r3470, r3471, r3472, r3473, r3474, r3475, r3476, r3477, r3478, r3479, r3480, r3481, r3482;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r3452);
        mpfr_init_set_str(r3453, "-0.025418290597484586", 10, MPFR_RNDN);
        mpfr_init(r3454);
        mpfr_init(r3455);
        mpfr_init(r3456);
        mpfr_init(r3457);
        mpfr_init(r3458);
        mpfr_init(r3459);
        mpfr_init(r3460);
        mpfr_init(r3461);
        mpfr_init_set_str(r3462, "0.029062913185972455", 10, MPFR_RNDN);
        mpfr_init(r3463);
        mpfr_init_set_str(r3464, "-27/2800", 10, MPFR_RNDN);
        mpfr_init(r3465);
        mpfr_init(r3466);
        mpfr_init_set_str(r3467, "1", 10, MPFR_RNDN);
        mpfr_init(r3468);
        mpfr_init(r3469);
        mpfr_init_set_str(r3470, "9/40", 10, MPFR_RNDN);
        mpfr_init(r3471);
        mpfr_init(r3472);
        mpfr_init(r3473);
        mpfr_init_set_str(r3474, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r3475);
        mpfr_init(r3476);
        mpfr_init(r3477);
        mpfr_init(r3478);
        mpfr_init(r3479);
        mpfr_init(r3480);
        mpfr_init(r3481);
        mpfr_init(r3482);
}

double f_dm(double x) {
        mpfr_set_d(r3452, x, MPFR_RNDN);
        ;
        mpfr_set_si(r3454, mpfr_cmp(r3452, r3453) < 0, MPFR_RNDN);
        mpfr_sin(r3455, r3452, MPFR_RNDN);
        mpfr_sub(r3456, r3452, r3455, MPFR_RNDN);
        mpfr_tan(r3457, r3452, MPFR_RNDN);
        mpfr_sub(r3458, r3452, r3457, MPFR_RNDN);
        mpfr_div(r3459, r3456, r3458, MPFR_RNDN);
        mpfr_exp(r3460, r3459, MPFR_RNDN);
        mpfr_log(r3461, r3460, MPFR_RNDN);
        ;
        mpfr_set_si(r3463, mpfr_cmp(r3452, r3462) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r3465, r3452, r3452, MPFR_RNDN);
        mpfr_mul(r3466, r3465, r3465, MPFR_RNDN);
        ;
        mpfr_mul(r3468, r3466, r3467, MPFR_RNDN);
        mpfr_mul(r3469, r3464, r3468, MPFR_RNDN);
        ;
        mpfr_mul(r3471, r3452, r3452, MPFR_RNDN);
        mpfr_mul(r3472, r3471, r3467, MPFR_RNDN);
        mpfr_mul(r3473, r3470, r3472, MPFR_RNDN);
        ;
        mpfr_mul(r3475, r3467, r3467, MPFR_RNDN);
        mpfr_mul(r3476, r3474, r3475, MPFR_RNDN);
        mpfr_add(r3477, r3473, r3476, MPFR_RNDN);
        mpfr_add(r3478, r3469, r3477, MPFR_RNDN);
        mpfr_exp(r3479, r3478, MPFR_RNDN);
        mpfr_log(r3480, r3479, MPFR_RNDN);
        if (mpfr_get_si(r3463, MPFR_RNDN)) { mpfr_set(r3481, r3480, MPFR_RNDN); } else { mpfr_set(r3481, r3461, MPFR_RNDN); };
        if (mpfr_get_si(r3454, MPFR_RNDN)) { mpfr_set(r3482, r3461, MPFR_RNDN); } else { mpfr_set(r3482, r3481, MPFR_RNDN); };
        return mpfr_get_d(r3482, MPFR_RNDN);
}

