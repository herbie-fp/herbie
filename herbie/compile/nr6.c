#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.7";

double f_if(float x) {
        float r628 = x;
        float r629 = exp(r628);
        float r630 = 1.0;
        float r631 = r629 - r630;
        return r631;
}

double f_id(double x) {
        double r632 = x;
        double r633 = exp(r632);
        double r634 = 1.0;
        double r635 = r633 - r634;
        return r635;
}


double f_of(float x) {
        float r636 = 0.16666666666666666;
        float r637 = x;
        float r638 = r637 * r637;
        float r639 = r637 * r638;
        float r640 = 1.0;
        float r641 = r639 * r640;
        float r642 = r636 * r641;
        float r643 = 0.5;
        float r644 = r637 * r637;
        float r645 = r644 * r640;
        float r646 = r643 * r645;
        float r647 = r637 * r640;
        float r648 = r640 * r647;
        float r649 = r646 + r648;
        float r650 = r642 + r649;
        return r650;
}

double f_od(double x) {
        double r651 = 0.16666666666666666;
        double r652 = x;
        double r653 = r652 * r652;
        double r654 = r652 * r653;
        double r655 = 1.0;
        double r656 = r654 * r655;
        double r657 = r651 * r656;
        double r658 = 0.5;
        double r659 = r652 * r652;
        double r660 = r659 * r655;
        double r661 = r658 * r660;
        double r662 = r652 * r655;
        double r663 = r655 * r662;
        double r664 = r661 + r663;
        double r665 = r657 + r664;
        return r665;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r666, r667, r668, r669;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r666);
        mpfr_init(r667);
        mpfr_init_set_str(r668, "1", 10, MPFR_RNDN);
        mpfr_init(r669);
}

double f_im(double x) {
        mpfr_set_d(r666, x, MPFR_RNDN);
        mpfr_exp(r667, r666, MPFR_RNDN);
        ;
        mpfr_sub(r669, r667, r668, MPFR_RNDN);
        return mpfr_get_d(r669, MPFR_RNDN);
}

static mpfr_t r670, r671, r672, r673, r674, r675, r676, r677, r678, r679, r680, r681, r682, r683, r684;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r670, "1/6", 10, MPFR_RNDN);
        mpfr_init(r671);
        mpfr_init(r672);
        mpfr_init(r673);
        mpfr_init_set_str(r674, "1", 10, MPFR_RNDN);
        mpfr_init(r675);
        mpfr_init(r676);
        mpfr_init_set_str(r677, "1/2", 10, MPFR_RNDN);
        mpfr_init(r678);
        mpfr_init(r679);
        mpfr_init(r680);
        mpfr_init(r681);
        mpfr_init(r682);
        mpfr_init(r683);
        mpfr_init(r684);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r671, x, MPFR_RNDN);
        mpfr_mul(r672, r671, r671, MPFR_RNDN);
        mpfr_mul(r673, r671, r672, MPFR_RNDN);
        ;
        mpfr_mul(r675, r673, r674, MPFR_RNDN);
        mpfr_mul(r676, r670, r675, MPFR_RNDN);
        ;
        mpfr_mul(r678, r671, r671, MPFR_RNDN);
        mpfr_mul(r679, r678, r674, MPFR_RNDN);
        mpfr_mul(r680, r677, r679, MPFR_RNDN);
        mpfr_mul(r681, r671, r674, MPFR_RNDN);
        mpfr_mul(r682, r674, r681, MPFR_RNDN);
        mpfr_add(r683, r680, r682, MPFR_RNDN);
        mpfr_add(r684, r676, r683, MPFR_RNDN);
        return mpfr_get_d(r684, MPFR_RNDN);
}

static mpfr_t r685, r686, r687, r688, r689, r690, r691, r692, r693, r694, r695, r696, r697, r698, r699;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r685, "1/6", 10, MPFR_RNDN);
        mpfr_init(r686);
        mpfr_init(r687);
        mpfr_init(r688);
        mpfr_init_set_str(r689, "1", 10, MPFR_RNDN);
        mpfr_init(r690);
        mpfr_init(r691);
        mpfr_init_set_str(r692, "1/2", 10, MPFR_RNDN);
        mpfr_init(r693);
        mpfr_init(r694);
        mpfr_init(r695);
        mpfr_init(r696);
        mpfr_init(r697);
        mpfr_init(r698);
        mpfr_init(r699);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r686, x, MPFR_RNDN);
        mpfr_mul(r687, r686, r686, MPFR_RNDN);
        mpfr_mul(r688, r686, r687, MPFR_RNDN);
        ;
        mpfr_mul(r690, r688, r689, MPFR_RNDN);
        mpfr_mul(r691, r685, r690, MPFR_RNDN);
        ;
        mpfr_mul(r693, r686, r686, MPFR_RNDN);
        mpfr_mul(r694, r693, r689, MPFR_RNDN);
        mpfr_mul(r695, r692, r694, MPFR_RNDN);
        mpfr_mul(r696, r686, r689, MPFR_RNDN);
        mpfr_mul(r697, r689, r696, MPFR_RNDN);
        mpfr_add(r698, r695, r697, MPFR_RNDN);
        mpfr_add(r699, r691, r698, MPFR_RNDN);
        return mpfr_get_d(r699, MPFR_RNDN);
}

