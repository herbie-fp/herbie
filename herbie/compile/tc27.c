#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.6";

double f_if(float x, float n) {
        float r3483 = x;
        float r3484 = 1.0;
        float r3485 = r3483 + r3484;
        float r3486 = n;
        float r3487 = r3484 / r3486;
        float r3488 = pow(r3485, r3487);
        float r3489 = pow(r3483, r3487);
        float r3490 = r3488 - r3489;
        return r3490;
}

double f_id(double x, double n) {
        double r3491 = x;
        double r3492 = 1.0;
        double r3493 = r3491 + r3492;
        double r3494 = n;
        double r3495 = r3492 / r3494;
        double r3496 = pow(r3493, r3495);
        double r3497 = pow(r3491, r3495);
        double r3498 = r3496 - r3497;
        return r3498;
}


double f_of(float x, float n) {
        float r3499 = n;
        float r3500 = 1645268.625;
        bool r3501 = r3499 < r3500;
        float r3502 = x;
        float r3503 = 1.0;
        float r3504 = r3502 + r3503;
        float r3505 = r3503 / r3499;
        float r3506 = pow(r3504, r3505);
        float r3507 = sqrt(r3506);
        float r3508 = r3507 * r3507;
        float r3509 = pow(r3502, r3505);
        float r3510 = r3508 - r3509;
        float r3511 = -0.5;
        float r3512 = r3502 * r3502;
        float r3513 = r3512 * r3499;
        float r3514 = r3511 / r3513;
        float r3515 = r3502 * r3499;
        float r3516 = r3503 / r3515;
        float r3517 = -1.0;
        float r3518 = log(r3502);
        float r3519 = r3499 * r3499;
        float r3520 = r3502 * r3519;
        float r3521 = r3518 / r3520;
        float r3522 = r3517 * r3521;
        float r3523 = r3516 + r3522;
        float r3524 = r3514 + r3523;
        float r3525 = r3501 ? r3510 : r3524;
        return r3525;
}

double f_od(double x, double n) {
        double r3526 = n;
        double r3527 = -8510789541.969252;
        bool r3528 = r3526 < r3527;
        double r3529 = -9.0;
        double r3530 = x;
        double r3531 = log(r3530);
        double r3532 = r3529 * r3531;
        double r3533 = 1.0;
        double r3534 = r3533 / r3530;
        double r3535 = r3526 * r3526;
        double r3536 = r3534 / r3535;
        double r3537 = r3532 * r3536;
        double r3538 = -1.5;
        double r3539 = r3530 * r3530;
        double r3540 = r3533 / r3539;
        double r3541 = r3540 / r3526;
        double r3542 = r3538 * r3541;
        double r3543 = 3.0;
        double r3544 = r3534 / r3526;
        double r3545 = r3543 * r3544;
        double r3546 = r3542 + r3545;
        double r3547 = r3537 + r3546;
        double r3548 = r3530 + r3533;
        double r3549 = r3533 / r3526;
        double r3550 = pow(r3548, r3549);
        double r3551 = r3550 * r3550;
        double r3552 = pow(r3530, r3549);
        double r3553 = r3552 * r3552;
        double r3554 = r3551 + r3553;
        double r3555 = r3550 * r3552;
        double r3556 = r3554 + r3555;
        double r3557 = r3547 / r3556;
        double r3558 = 14665177870.83183;
        bool r3559 = r3526 < r3558;
        double r3560 = r3533 + r3530;
        double r3561 = pow(r3560, r3549);
        double r3562 = sqrt(r3552);
        double r3563 = r3562 * r3562;
        double r3564 = r3561 - r3563;
        double r3565 = sqrt(r3550);
        double r3566 = r3565 + r3562;
        double r3567 = -0.25;
        double r3568 = r3567 * r3531;
        double r3569 = r3568 * r3536;
        double r3570 = r3567 * r3541;
        double r3571 = 0.5;
        double r3572 = r3571 * r3544;
        double r3573 = r3570 + r3572;
        double r3574 = r3569 + r3573;
        double r3575 = r3566 * r3574;
        double r3576 = r3559 ? r3564 : r3575;
        double r3577 = r3528 ? r3557 : r3576;
        return r3577;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r3578, r3579, r3580, r3581, r3582, r3583, r3584, r3585;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3578);
        mpfr_init_set_str(r3579, "1", 10, MPFR_RNDN);
        mpfr_init(r3580);
        mpfr_init(r3581);
        mpfr_init(r3582);
        mpfr_init(r3583);
        mpfr_init(r3584);
        mpfr_init(r3585);
}

double f_im(double x, double n) {
        mpfr_set_d(r3578, x, MPFR_RNDN);
        ;
        mpfr_add(r3580, r3578, r3579, MPFR_RNDN);
        mpfr_set_d(r3581, n, MPFR_RNDN);
        mpfr_div(r3582, r3579, r3581, MPFR_RNDN);
        mpfr_pow(r3583, r3580, r3582, MPFR_RNDN);
        mpfr_pow(r3584, r3578, r3582, MPFR_RNDN);
        mpfr_sub(r3585, r3583, r3584, MPFR_RNDN);
        return mpfr_get_d(r3585, MPFR_RNDN);
}

static mpfr_t r3586, r3587, r3588, r3589, r3590, r3591, r3592, r3593, r3594, r3595, r3596, r3597, r3598, r3599, r3600, r3601, r3602, r3603, r3604, r3605, r3606, r3607, r3608, r3609, r3610, r3611, r3612;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3586);
        mpfr_init_set_str(r3587, "1645268.625", 10, MPFR_RNDN);
        mpfr_init(r3588);
        mpfr_init(r3589);
        mpfr_init_set_str(r3590, "1", 10, MPFR_RNDN);
        mpfr_init(r3591);
        mpfr_init(r3592);
        mpfr_init(r3593);
        mpfr_init(r3594);
        mpfr_init(r3595);
        mpfr_init(r3596);
        mpfr_init(r3597);
        mpfr_init_set_str(r3598, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r3599);
        mpfr_init(r3600);
        mpfr_init(r3601);
        mpfr_init(r3602);
        mpfr_init(r3603);
        mpfr_init_set_str(r3604, "-1", 10, MPFR_RNDN);
        mpfr_init(r3605);
        mpfr_init(r3606);
        mpfr_init(r3607);
        mpfr_init(r3608);
        mpfr_init(r3609);
        mpfr_init(r3610);
        mpfr_init(r3611);
        mpfr_init(r3612);
}

double f_fm(double x, double n) {
        mpfr_set_d(r3586, n, MPFR_RNDN);
        ;
        mpfr_set_si(r3588, mpfr_cmp(r3586, r3587) < 0, MPFR_RNDN);
        mpfr_set_d(r3589, x, MPFR_RNDN);
        ;
        mpfr_add(r3591, r3589, r3590, MPFR_RNDN);
        mpfr_div(r3592, r3590, r3586, MPFR_RNDN);
        mpfr_pow(r3593, r3591, r3592, MPFR_RNDN);
        mpfr_sqrt(r3594, r3593, MPFR_RNDN);
        mpfr_mul(r3595, r3594, r3594, MPFR_RNDN);
        mpfr_pow(r3596, r3589, r3592, MPFR_RNDN);
        mpfr_sub(r3597, r3595, r3596, MPFR_RNDN);
        ;
        mpfr_mul(r3599, r3589, r3589, MPFR_RNDN);
        mpfr_mul(r3600, r3599, r3586, MPFR_RNDN);
        mpfr_div(r3601, r3598, r3600, MPFR_RNDN);
        mpfr_mul(r3602, r3589, r3586, MPFR_RNDN);
        mpfr_div(r3603, r3590, r3602, MPFR_RNDN);
        ;
        mpfr_log(r3605, r3589, MPFR_RNDN);
        mpfr_mul(r3606, r3586, r3586, MPFR_RNDN);
        mpfr_mul(r3607, r3589, r3606, MPFR_RNDN);
        mpfr_div(r3608, r3605, r3607, MPFR_RNDN);
        mpfr_mul(r3609, r3604, r3608, MPFR_RNDN);
        mpfr_add(r3610, r3603, r3609, MPFR_RNDN);
        mpfr_add(r3611, r3601, r3610, MPFR_RNDN);
        if (mpfr_get_si(r3588, MPFR_RNDN)) { mpfr_set(r3612, r3597, MPFR_RNDN); } else { mpfr_set(r3612, r3611, MPFR_RNDN); };
        return mpfr_get_d(r3612, MPFR_RNDN);
}

static mpfr_t r3613, r3614, r3615, r3616, r3617, r3618, r3619, r3620, r3621, r3622, r3623, r3624, r3625, r3626, r3627, r3628, r3629, r3630, r3631, r3632, r3633, r3634, r3635, r3636, r3637, r3638, r3639, r3640, r3641, r3642, r3643, r3644, r3645, r3646, r3647, r3648, r3649, r3650, r3651, r3652, r3653, r3654, r3655, r3656, r3657, r3658, r3659, r3660, r3661, r3662, r3663, r3664;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r3613);
        mpfr_init_set_str(r3614, "-8510789541.969252", 10, MPFR_RNDN);
        mpfr_init(r3615);
        mpfr_init_set_str(r3616, "-9", 10, MPFR_RNDN);
        mpfr_init(r3617);
        mpfr_init(r3618);
        mpfr_init(r3619);
        mpfr_init_set_str(r3620, "1", 10, MPFR_RNDN);
        mpfr_init(r3621);
        mpfr_init(r3622);
        mpfr_init(r3623);
        mpfr_init(r3624);
        mpfr_init_set_str(r3625, "-3/2", 10, MPFR_RNDN);
        mpfr_init(r3626);
        mpfr_init(r3627);
        mpfr_init(r3628);
        mpfr_init(r3629);
        mpfr_init_set_str(r3630, "3", 10, MPFR_RNDN);
        mpfr_init(r3631);
        mpfr_init(r3632);
        mpfr_init(r3633);
        mpfr_init(r3634);
        mpfr_init(r3635);
        mpfr_init(r3636);
        mpfr_init(r3637);
        mpfr_init(r3638);
        mpfr_init(r3639);
        mpfr_init(r3640);
        mpfr_init(r3641);
        mpfr_init(r3642);
        mpfr_init(r3643);
        mpfr_init(r3644);
        mpfr_init_set_str(r3645, "14665177870.83183", 10, MPFR_RNDN);
        mpfr_init(r3646);
        mpfr_init(r3647);
        mpfr_init(r3648);
        mpfr_init(r3649);
        mpfr_init(r3650);
        mpfr_init(r3651);
        mpfr_init(r3652);
        mpfr_init(r3653);
        mpfr_init_set_str(r3654, "-1/4", 10, MPFR_RNDN);
        mpfr_init(r3655);
        mpfr_init(r3656);
        mpfr_init(r3657);
        mpfr_init_set_str(r3658, "1/2", 10, MPFR_RNDN);
        mpfr_init(r3659);
        mpfr_init(r3660);
        mpfr_init(r3661);
        mpfr_init(r3662);
        mpfr_init(r3663);
        mpfr_init(r3664);
}

double f_dm(double x, double n) {
        mpfr_set_d(r3613, n, MPFR_RNDN);
        ;
        mpfr_set_si(r3615, mpfr_cmp(r3613, r3614) < 0, MPFR_RNDN);
        ;
        mpfr_set_d(r3617, x, MPFR_RNDN);
        mpfr_log(r3618, r3617, MPFR_RNDN);
        mpfr_mul(r3619, r3616, r3618, MPFR_RNDN);
        ;
        mpfr_div(r3621, r3620, r3617, MPFR_RNDN);
        mpfr_mul(r3622, r3613, r3613, MPFR_RNDN);
        mpfr_div(r3623, r3621, r3622, MPFR_RNDN);
        mpfr_mul(r3624, r3619, r3623, MPFR_RNDN);
        ;
        mpfr_mul(r3626, r3617, r3617, MPFR_RNDN);
        mpfr_div(r3627, r3620, r3626, MPFR_RNDN);
        mpfr_div(r3628, r3627, r3613, MPFR_RNDN);
        mpfr_mul(r3629, r3625, r3628, MPFR_RNDN);
        ;
        mpfr_div(r3631, r3621, r3613, MPFR_RNDN);
        mpfr_mul(r3632, r3630, r3631, MPFR_RNDN);
        mpfr_add(r3633, r3629, r3632, MPFR_RNDN);
        mpfr_add(r3634, r3624, r3633, MPFR_RNDN);
        mpfr_add(r3635, r3617, r3620, MPFR_RNDN);
        mpfr_div(r3636, r3620, r3613, MPFR_RNDN);
        mpfr_pow(r3637, r3635, r3636, MPFR_RNDN);
        mpfr_mul(r3638, r3637, r3637, MPFR_RNDN);
        mpfr_pow(r3639, r3617, r3636, MPFR_RNDN);
        mpfr_mul(r3640, r3639, r3639, MPFR_RNDN);
        mpfr_add(r3641, r3638, r3640, MPFR_RNDN);
        mpfr_mul(r3642, r3637, r3639, MPFR_RNDN);
        mpfr_add(r3643, r3641, r3642, MPFR_RNDN);
        mpfr_div(r3644, r3634, r3643, MPFR_RNDN);
        ;
        mpfr_set_si(r3646, mpfr_cmp(r3613, r3645) < 0, MPFR_RNDN);
        mpfr_add(r3647, r3620, r3617, MPFR_RNDN);
        mpfr_pow(r3648, r3647, r3636, MPFR_RNDN);
        mpfr_sqrt(r3649, r3639, MPFR_RNDN);
        mpfr_mul(r3650, r3649, r3649, MPFR_RNDN);
        mpfr_sub(r3651, r3648, r3650, MPFR_RNDN);
        mpfr_sqrt(r3652, r3637, MPFR_RNDN);
        mpfr_add(r3653, r3652, r3649, MPFR_RNDN);
        ;
        mpfr_mul(r3655, r3654, r3618, MPFR_RNDN);
        mpfr_mul(r3656, r3655, r3623, MPFR_RNDN);
        mpfr_mul(r3657, r3654, r3628, MPFR_RNDN);
        ;
        mpfr_mul(r3659, r3658, r3631, MPFR_RNDN);
        mpfr_add(r3660, r3657, r3659, MPFR_RNDN);
        mpfr_add(r3661, r3656, r3660, MPFR_RNDN);
        mpfr_mul(r3662, r3653, r3661, MPFR_RNDN);
        if (mpfr_get_si(r3646, MPFR_RNDN)) { mpfr_set(r3663, r3651, MPFR_RNDN); } else { mpfr_set(r3663, r3662, MPFR_RNDN); };
        if (mpfr_get_si(r3615, MPFR_RNDN)) { mpfr_set(r3664, r3644, MPFR_RNDN); } else { mpfr_set(r3664, r3663, MPFR_RNDN); };
        return mpfr_get_d(r3664, MPFR_RNDN);
}

