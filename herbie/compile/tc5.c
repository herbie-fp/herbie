#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.6";

double f_if(float x) {
        float r526 = 1.0;
        float r527 = x;
        float r528 = sqrt(r527);
        float r529 = r526 / r528;
        float r530 = r527 + r526;
        float r531 = sqrt(r530);
        float r532 = r526 / r531;
        float r533 = r529 - r532;
        return r533;
}

double f_id(double x) {
        double r534 = 1.0;
        double r535 = x;
        double r536 = sqrt(r535);
        double r537 = r534 / r536;
        double r538 = r535 + r534;
        double r539 = sqrt(r538);
        double r540 = r534 / r539;
        double r541 = r537 - r540;
        return r541;
}


double f_of(float x) {
        float r542 = x;
        float r543 = 1.3339469757051953e+19;
        bool r544 = r542 < r543;
        float r545 = 1.0;
        float r546 = r542 * r542;
        float r547 = r546 + r542;
        float r548 = r545 / r547;
        float r549 = sqrt(r542);
        float r550 = r545 / r549;
        float r551 = r542 + r545;
        float r552 = sqrt(r551);
        float r553 = r545 / r552;
        float r554 = r550 + r553;
        float r555 = r545 / r554;
        float r556 = r548 * r555;
        float r557 = 0.375;
        float r558 = 3.0;
        float r559 = pow(r542, r558);
        float r560 = r557 / r559;
        float r561 = r545 / r542;
        float r562 = -0.5;
        float r563 = r562 / r546;
        float r564 = r561 + r563;
        float r565 = r560 + r564;
        float r566 = r553 + r550;
        float r567 = sqrt(r566);
        float r568 = r565 / r567;
        float r569 = r568 * r568;
        float r570 = r544 ? r556 : r569;
        return r570;
}

double f_od(double x) {
        double r571 = 1.0;
        double r572 = x;
        double r573 = r571 + r572;
        double r574 = sqrt(r573);
        double r575 = sqrt(r572);
        double r576 = r574 + r575;
        double r577 = r571 / r576;
        double r578 = r572 + r571;
        double r579 = sqrt(r578);
        double r580 = r579 * r575;
        double r581 = r571 / r580;
        double r582 = r577 * r581;
        return r582;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r583, r584, r585, r586, r587, r588, r589, r590;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r583, "1", 10, MPFR_RNDN);
        mpfr_init(r584);
        mpfr_init(r585);
        mpfr_init(r586);
        mpfr_init(r587);
        mpfr_init(r588);
        mpfr_init(r589);
        mpfr_init(r590);
}

double f_im(double x) {
        ;
        mpfr_set_d(r584, x, MPFR_RNDN);
        mpfr_sqrt(r585, r584, MPFR_RNDN);
        mpfr_div(r586, r583, r585, MPFR_RNDN);
        mpfr_add(r587, r584, r583, MPFR_RNDN);
        mpfr_sqrt(r588, r587, MPFR_RNDN);
        mpfr_div(r589, r583, r588, MPFR_RNDN);
        mpfr_sub(r590, r586, r589, MPFR_RNDN);
        return mpfr_get_d(r590, MPFR_RNDN);
}

static mpfr_t r591, r592, r593, r594, r595, r596, r597, r598, r599, r600, r601, r602, r603, r604, r605, r606, r607, r608, r609, r610, r611, r612, r613, r614, r615, r616, r617, r618, r619;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r591);
        mpfr_init_set_str(r592, "1.3339469757051953e+19", 10, MPFR_RNDN);
        mpfr_init(r593);
        mpfr_init_set_str(r594, "1", 10, MPFR_RNDN);
        mpfr_init(r595);
        mpfr_init(r596);
        mpfr_init(r597);
        mpfr_init(r598);
        mpfr_init(r599);
        mpfr_init(r600);
        mpfr_init(r601);
        mpfr_init(r602);
        mpfr_init(r603);
        mpfr_init(r604);
        mpfr_init(r605);
        mpfr_init_set_str(r606, "3/8", 10, MPFR_RNDN);
        mpfr_init_set_str(r607, "3", 10, MPFR_RNDN);
        mpfr_init(r608);
        mpfr_init(r609);
        mpfr_init(r610);
        mpfr_init_set_str(r611, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r612);
        mpfr_init(r613);
        mpfr_init(r614);
        mpfr_init(r615);
        mpfr_init(r616);
        mpfr_init(r617);
        mpfr_init(r618);
        mpfr_init(r619);
}

double f_fm(double x) {
        mpfr_set_d(r591, x, MPFR_RNDN);
        ;
        mpfr_set_si(r593, mpfr_cmp(r591, r592) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r595, r591, r591, MPFR_RNDN);
        mpfr_add(r596, r595, r591, MPFR_RNDN);
        mpfr_div(r597, r594, r596, MPFR_RNDN);
        mpfr_sqrt(r598, r591, MPFR_RNDN);
        mpfr_div(r599, r594, r598, MPFR_RNDN);
        mpfr_add(r600, r591, r594, MPFR_RNDN);
        mpfr_sqrt(r601, r600, MPFR_RNDN);
        mpfr_div(r602, r594, r601, MPFR_RNDN);
        mpfr_add(r603, r599, r602, MPFR_RNDN);
        mpfr_div(r604, r594, r603, MPFR_RNDN);
        mpfr_mul(r605, r597, r604, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r608, r591, r607, MPFR_RNDN);
        mpfr_div(r609, r606, r608, MPFR_RNDN);
        mpfr_div(r610, r594, r591, MPFR_RNDN);
        ;
        mpfr_div(r612, r611, r595, MPFR_RNDN);
        mpfr_add(r613, r610, r612, MPFR_RNDN);
        mpfr_add(r614, r609, r613, MPFR_RNDN);
        mpfr_add(r615, r602, r599, MPFR_RNDN);
        mpfr_sqrt(r616, r615, MPFR_RNDN);
        mpfr_div(r617, r614, r616, MPFR_RNDN);
        mpfr_mul(r618, r617, r617, MPFR_RNDN);
        if (mpfr_get_si(r593, MPFR_RNDN)) { mpfr_set(r619, r605, MPFR_RNDN); } else { mpfr_set(r619, r618, MPFR_RNDN); };
        return mpfr_get_d(r619, MPFR_RNDN);
}

static mpfr_t r620, r621, r622, r623, r624, r625, r626, r627, r628, r629, r630, r631;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r620, "1", 10, MPFR_RNDN);
        mpfr_init(r621);
        mpfr_init(r622);
        mpfr_init(r623);
        mpfr_init(r624);
        mpfr_init(r625);
        mpfr_init(r626);
        mpfr_init(r627);
        mpfr_init(r628);
        mpfr_init(r629);
        mpfr_init(r630);
        mpfr_init(r631);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r621, x, MPFR_RNDN);
        mpfr_add(r622, r620, r621, MPFR_RNDN);
        mpfr_sqrt(r623, r622, MPFR_RNDN);
        mpfr_sqrt(r624, r621, MPFR_RNDN);
        mpfr_add(r625, r623, r624, MPFR_RNDN);
        mpfr_div(r626, r620, r625, MPFR_RNDN);
        mpfr_add(r627, r621, r620, MPFR_RNDN);
        mpfr_sqrt(r628, r627, MPFR_RNDN);
        mpfr_mul(r629, r628, r624, MPFR_RNDN);
        mpfr_div(r630, r620, r629, MPFR_RNDN);
        mpfr_mul(r631, r626, r630, MPFR_RNDN);
        return mpfr_get_d(r631, MPFR_RNDN);
}

