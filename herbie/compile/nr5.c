#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.6";

double f_if(float x) {
        float r556 = 1.0;
        float r557 = x;
        float r558 = sqrt(r557);
        float r559 = r556 / r558;
        float r560 = r557 + r556;
        float r561 = sqrt(r560);
        float r562 = r556 / r561;
        float r563 = r559 - r562;
        return r563;
}

double f_id(double x) {
        double r564 = 1.0;
        double r565 = x;
        double r566 = sqrt(r565);
        double r567 = r564 / r566;
        double r568 = r565 + r564;
        double r569 = sqrt(r568);
        double r570 = r564 / r569;
        double r571 = r567 - r570;
        return r571;
}


double f_of(float x) {
        float r572 = 1.0;
        float r573 = x;
        float r574 = r572 + r573;
        float r575 = sqrt(r574);
        float r576 = sqrt(r573);
        float r577 = r575 + r576;
        float r578 = r572 / r577;
        float r579 = r573 + r572;
        float r580 = sqrt(r579);
        float r581 = r580 * r576;
        float r582 = r572 / r581;
        float r583 = r578 * r582;
        return r583;
}

double f_od(double x) {
        double r584 = 1.0;
        double r585 = x;
        double r586 = r584 + r585;
        double r587 = sqrt(r586);
        double r588 = sqrt(r585);
        double r589 = r587 + r588;
        double r590 = r584 / r589;
        double r591 = r585 + r584;
        double r592 = sqrt(r591);
        double r593 = r592 * r588;
        double r594 = r584 / r593;
        double r595 = r590 * r594;
        return r595;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r596, r597, r598, r599, r600, r601, r602, r603;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r596, "1", 10, MPFR_RNDN);
        mpfr_init(r597);
        mpfr_init(r598);
        mpfr_init(r599);
        mpfr_init(r600);
        mpfr_init(r601);
        mpfr_init(r602);
        mpfr_init(r603);
}

double f_im(double x) {
        ;
        mpfr_set_d(r597, x, MPFR_RNDN);
        mpfr_sqrt(r598, r597, MPFR_RNDN);
        mpfr_div(r599, r596, r598, MPFR_RNDN);
        mpfr_add(r600, r597, r596, MPFR_RNDN);
        mpfr_sqrt(r601, r600, MPFR_RNDN);
        mpfr_div(r602, r596, r601, MPFR_RNDN);
        mpfr_sub(r603, r599, r602, MPFR_RNDN);
        return mpfr_get_d(r603, MPFR_RNDN);
}

static mpfr_t r604, r605, r606, r607, r608, r609, r610, r611, r612, r613, r614, r615;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r604, "1", 10, MPFR_RNDN);
        mpfr_init(r605);
        mpfr_init(r606);
        mpfr_init(r607);
        mpfr_init(r608);
        mpfr_init(r609);
        mpfr_init(r610);
        mpfr_init(r611);
        mpfr_init(r612);
        mpfr_init(r613);
        mpfr_init(r614);
        mpfr_init(r615);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r605, x, MPFR_RNDN);
        mpfr_add(r606, r604, r605, MPFR_RNDN);
        mpfr_sqrt(r607, r606, MPFR_RNDN);
        mpfr_sqrt(r608, r605, MPFR_RNDN);
        mpfr_add(r609, r607, r608, MPFR_RNDN);
        mpfr_div(r610, r604, r609, MPFR_RNDN);
        mpfr_add(r611, r605, r604, MPFR_RNDN);
        mpfr_sqrt(r612, r611, MPFR_RNDN);
        mpfr_mul(r613, r612, r608, MPFR_RNDN);
        mpfr_div(r614, r604, r613, MPFR_RNDN);
        mpfr_mul(r615, r610, r614, MPFR_RNDN);
        return mpfr_get_d(r615, MPFR_RNDN);
}

static mpfr_t r616, r617, r618, r619, r620, r621, r622, r623, r624, r625, r626, r627;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r616, "1", 10, MPFR_RNDN);
        mpfr_init(r617);
        mpfr_init(r618);
        mpfr_init(r619);
        mpfr_init(r620);
        mpfr_init(r621);
        mpfr_init(r622);
        mpfr_init(r623);
        mpfr_init(r624);
        mpfr_init(r625);
        mpfr_init(r626);
        mpfr_init(r627);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r617, x, MPFR_RNDN);
        mpfr_add(r618, r616, r617, MPFR_RNDN);
        mpfr_sqrt(r619, r618, MPFR_RNDN);
        mpfr_sqrt(r620, r617, MPFR_RNDN);
        mpfr_add(r621, r619, r620, MPFR_RNDN);
        mpfr_div(r622, r616, r621, MPFR_RNDN);
        mpfr_add(r623, r617, r616, MPFR_RNDN);
        mpfr_sqrt(r624, r623, MPFR_RNDN);
        mpfr_mul(r625, r624, r620, MPFR_RNDN);
        mpfr_div(r626, r616, r625, MPFR_RNDN);
        mpfr_mul(r627, r622, r626, MPFR_RNDN);
        return mpfr_get_d(r627, MPFR_RNDN);
}

