#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.7";

double f_if(float x) {
        float r632 = x;
        float r633 = exp(r632);
        float r634 = 1.0;
        float r635 = r633 - r634;
        return r635;
}

double f_id(double x) {
        double r636 = x;
        double r637 = exp(r636);
        double r638 = 1.0;
        double r639 = r637 - r638;
        return r639;
}


double f_of(float x) {
        float r640 = x;
        float r641 = -0.028476345352828503;
        bool r642 = r640 < r641;
        float r643 = exp(r640);
        float r644 = 1.0;
        float r645 = r643 - r644;
        float r646 = 0.16666666666666666;
        float r647 = 3.0;
        float r648 = pow(r640, r647);
        float r649 = r646 * r648;
        float r650 = 0.5;
        float r651 = r640 * r640;
        float r652 = r650 * r651;
        float r653 = r640 + r652;
        float r654 = r649 + r653;
        float r655 = r642 ? r645 : r654;
        return r655;
}

double f_od(double x) {
        double r656 = x;
        double r657 = -0.00015804599441025758;
        bool r658 = r656 < r657;
        double r659 = exp(r656);
        double r660 = r659 * r659;
        double r661 = r660 * r660;
        double r662 = 1.0;
        double r663 = r662 + r660;
        double r664 = r661 / r663;
        double r665 = r659 + r662;
        double r666 = r664 / r665;
        double r667 = r662 / r663;
        double r668 = r662 + r659;
        double r669 = r667 / r668;
        double r670 = r666 - r669;
        double r671 = 0.16666666666666666;
        double r672 = r656 * r656;
        double r673 = r656 * r672;
        double r674 = r673 * r662;
        double r675 = r671 * r674;
        double r676 = 0.5;
        double r677 = r656 * r656;
        double r678 = r677 * r662;
        double r679 = r676 * r678;
        double r680 = r656 * r662;
        double r681 = r662 * r680;
        double r682 = r679 + r681;
        double r683 = r675 + r682;
        double r684 = r658 ? r670 : r683;
        return r684;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r685, r686, r687, r688;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r685);
        mpfr_init(r686);
        mpfr_init_set_str(r687, "1", 10, MPFR_RNDN);
        mpfr_init(r688);
}

double f_im(double x) {
        mpfr_set_d(r685, x, MPFR_RNDN);
        mpfr_exp(r686, r685, MPFR_RNDN);
        ;
        mpfr_sub(r688, r686, r687, MPFR_RNDN);
        return mpfr_get_d(r688, MPFR_RNDN);
}

static mpfr_t r689, r690, r691, r692, r693, r694, r695, r696, r697, r698, r699, r700, r701, r702, r703, r704;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r689);
        mpfr_init_set_str(r690, "-0.028476345352828503", 10, MPFR_RNDN);
        mpfr_init(r691);
        mpfr_init(r692);
        mpfr_init_set_str(r693, "1", 10, MPFR_RNDN);
        mpfr_init(r694);
        mpfr_init_set_str(r695, "1/6", 10, MPFR_RNDN);
        mpfr_init_set_str(r696, "3", 10, MPFR_RNDN);
        mpfr_init(r697);
        mpfr_init(r698);
        mpfr_init_set_str(r699, "1/2", 10, MPFR_RNDN);
        mpfr_init(r700);
        mpfr_init(r701);
        mpfr_init(r702);
        mpfr_init(r703);
        mpfr_init(r704);
}

double f_fm(double x) {
        mpfr_set_d(r689, x, MPFR_RNDN);
        ;
        mpfr_set_si(r691, mpfr_cmp(r689, r690) < 0, MPFR_RNDN);
        mpfr_exp(r692, r689, MPFR_RNDN);
        ;
        mpfr_sub(r694, r692, r693, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r697, r689, r696, MPFR_RNDN);
        mpfr_mul(r698, r695, r697, MPFR_RNDN);
        ;
        mpfr_mul(r700, r689, r689, MPFR_RNDN);
        mpfr_mul(r701, r699, r700, MPFR_RNDN);
        mpfr_add(r702, r689, r701, MPFR_RNDN);
        mpfr_add(r703, r698, r702, MPFR_RNDN);
        if (mpfr_get_si(r691, MPFR_RNDN)) { mpfr_set(r704, r694, MPFR_RNDN); } else { mpfr_set(r704, r703, MPFR_RNDN); };
        return mpfr_get_d(r704, MPFR_RNDN);
}

static mpfr_t r705, r706, r707, r708, r709, r710, r711, r712, r713, r714, r715, r716, r717, r718, r719, r720, r721, r722, r723, r724, r725, r726, r727, r728, r729, r730, r731, r732, r733;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r705);
        mpfr_init_set_str(r706, "-0.00015804599441025758", 10, MPFR_RNDN);
        mpfr_init(r707);
        mpfr_init(r708);
        mpfr_init(r709);
        mpfr_init(r710);
        mpfr_init_set_str(r711, "1", 10, MPFR_RNDN);
        mpfr_init(r712);
        mpfr_init(r713);
        mpfr_init(r714);
        mpfr_init(r715);
        mpfr_init(r716);
        mpfr_init(r717);
        mpfr_init(r718);
        mpfr_init(r719);
        mpfr_init_set_str(r720, "1/6", 10, MPFR_RNDN);
        mpfr_init(r721);
        mpfr_init(r722);
        mpfr_init(r723);
        mpfr_init(r724);
        mpfr_init_set_str(r725, "1/2", 10, MPFR_RNDN);
        mpfr_init(r726);
        mpfr_init(r727);
        mpfr_init(r728);
        mpfr_init(r729);
        mpfr_init(r730);
        mpfr_init(r731);
        mpfr_init(r732);
        mpfr_init(r733);
}

double f_dm(double x) {
        mpfr_set_d(r705, x, MPFR_RNDN);
        ;
        mpfr_set_si(r707, mpfr_cmp(r705, r706) < 0, MPFR_RNDN);
        mpfr_exp(r708, r705, MPFR_RNDN);
        mpfr_mul(r709, r708, r708, MPFR_RNDN);
        mpfr_mul(r710, r709, r709, MPFR_RNDN);
        ;
        mpfr_add(r712, r711, r709, MPFR_RNDN);
        mpfr_div(r713, r710, r712, MPFR_RNDN);
        mpfr_add(r714, r708, r711, MPFR_RNDN);
        mpfr_div(r715, r713, r714, MPFR_RNDN);
        mpfr_div(r716, r711, r712, MPFR_RNDN);
        mpfr_add(r717, r711, r708, MPFR_RNDN);
        mpfr_div(r718, r716, r717, MPFR_RNDN);
        mpfr_sub(r719, r715, r718, MPFR_RNDN);
        ;
        mpfr_mul(r721, r705, r705, MPFR_RNDN);
        mpfr_mul(r722, r705, r721, MPFR_RNDN);
        mpfr_mul(r723, r722, r711, MPFR_RNDN);
        mpfr_mul(r724, r720, r723, MPFR_RNDN);
        ;
        mpfr_mul(r726, r705, r705, MPFR_RNDN);
        mpfr_mul(r727, r726, r711, MPFR_RNDN);
        mpfr_mul(r728, r725, r727, MPFR_RNDN);
        mpfr_mul(r729, r705, r711, MPFR_RNDN);
        mpfr_mul(r730, r711, r729, MPFR_RNDN);
        mpfr_add(r731, r728, r730, MPFR_RNDN);
        mpfr_add(r732, r724, r731, MPFR_RNDN);
        if (mpfr_get_si(r707, MPFR_RNDN)) { mpfr_set(r733, r719, MPFR_RNDN); } else { mpfr_set(r733, r732, MPFR_RNDN); };
        return mpfr_get_d(r733, MPFR_RNDN);
}

