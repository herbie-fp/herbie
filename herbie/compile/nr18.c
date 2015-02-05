#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE section 3.5";

double f_if(float a, float x) {
        float r1725 = a;
        float r1726 = x;
        float r1727 = r1725 * r1726;
        float r1728 = exp(r1727);
        float r1729 = 1.0;
        float r1730 = r1728 - r1729;
        return r1730;
}

double f_id(double a, double x) {
        double r1731 = a;
        double r1732 = x;
        double r1733 = r1731 * r1732;
        double r1734 = exp(r1733);
        double r1735 = 1.0;
        double r1736 = r1734 - r1735;
        return r1736;
}


double f_of(float a, float x) {
        float r1737 = 1.0;
        float r1738 = a;
        float r1739 = x;
        float r1740 = r1738 * r1739;
        float r1741 = exp(r1740);
        float r1742 = sqrt(r1741);
        float r1743 = r1737 + r1742;
        float r1744 = r1742 - r1737;
        float r1745 = r1743 * r1744;
        return r1745;
}

double f_od(double a, double x) {
        double r1746 = 1.0;
        double r1747 = a;
        double r1748 = x;
        double r1749 = r1747 * r1748;
        double r1750 = exp(r1749);
        double r1751 = sqrt(r1750);
        double r1752 = r1746 + r1751;
        double r1753 = r1751 - r1746;
        double r1754 = r1752 * r1753;
        return r1754;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1755, r1756, r1757, r1758, r1759, r1760;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1755);
        mpfr_init(r1756);
        mpfr_init(r1757);
        mpfr_init(r1758);
        mpfr_init_set_str(r1759, "1", 10, MPFR_RNDN);
        mpfr_init(r1760);
}

double f_im(double a, double x) {
        mpfr_set_d(r1755, a, MPFR_RNDN);
        mpfr_set_d(r1756, x, MPFR_RNDN);
        mpfr_mul(r1757, r1755, r1756, MPFR_RNDN);
        mpfr_exp(r1758, r1757, MPFR_RNDN);
        ;
        mpfr_sub(r1760, r1758, r1759, MPFR_RNDN);
        return mpfr_get_d(r1760, MPFR_RNDN);
}

static mpfr_t r1761, r1762, r1763, r1764, r1765, r1766, r1767, r1768, r1769;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1761, "1", 10, MPFR_RNDN);
        mpfr_init(r1762);
        mpfr_init(r1763);
        mpfr_init(r1764);
        mpfr_init(r1765);
        mpfr_init(r1766);
        mpfr_init(r1767);
        mpfr_init(r1768);
        mpfr_init(r1769);
}

double f_fm(double a, double x) {
        ;
        mpfr_set_d(r1762, a, MPFR_RNDN);
        mpfr_set_d(r1763, x, MPFR_RNDN);
        mpfr_mul(r1764, r1762, r1763, MPFR_RNDN);
        mpfr_exp(r1765, r1764, MPFR_RNDN);
        mpfr_sqrt(r1766, r1765, MPFR_RNDN);
        mpfr_add(r1767, r1761, r1766, MPFR_RNDN);
        mpfr_sub(r1768, r1766, r1761, MPFR_RNDN);
        mpfr_mul(r1769, r1767, r1768, MPFR_RNDN);
        return mpfr_get_d(r1769, MPFR_RNDN);
}

static mpfr_t r1770, r1771, r1772, r1773, r1774, r1775, r1776, r1777, r1778;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1770, "1", 10, MPFR_RNDN);
        mpfr_init(r1771);
        mpfr_init(r1772);
        mpfr_init(r1773);
        mpfr_init(r1774);
        mpfr_init(r1775);
        mpfr_init(r1776);
        mpfr_init(r1777);
        mpfr_init(r1778);
}

double f_dm(double a, double x) {
        ;
        mpfr_set_d(r1771, a, MPFR_RNDN);
        mpfr_set_d(r1772, x, MPFR_RNDN);
        mpfr_mul(r1773, r1771, r1772, MPFR_RNDN);
        mpfr_exp(r1774, r1773, MPFR_RNDN);
        mpfr_sqrt(r1775, r1774, MPFR_RNDN);
        mpfr_add(r1776, r1770, r1775, MPFR_RNDN);
        mpfr_sub(r1777, r1775, r1770, MPFR_RNDN);
        mpfr_mul(r1778, r1776, r1777, MPFR_RNDN);
        return mpfr_get_d(r1778, MPFR_RNDN);
}

