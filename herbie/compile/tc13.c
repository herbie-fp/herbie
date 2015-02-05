#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.7";

double f_if(float x) {
        float r1708 = x;
        float r1709 = exp(r1708);
        float r1710 = 2.0;
        float r1711 = r1709 - r1710;
        float r1712 = -r1708;
        float r1713 = exp(r1712);
        float r1714 = r1711 + r1713;
        return r1714;
}

double f_id(double x) {
        double r1715 = x;
        double r1716 = exp(r1715);
        double r1717 = 2.0;
        double r1718 = r1716 - r1717;
        double r1719 = -r1715;
        double r1720 = exp(r1719);
        double r1721 = r1718 + r1720;
        return r1721;
}


double f_of(float x) {
        float r1722 = x;
        float r1723 = r1722 * r1722;
        float r1724 = 0.08333333333333333;
        float r1725 = 4.0;
        float r1726 = pow(r1722, r1725);
        float r1727 = r1724 * r1726;
        float r1728 = 0.002777777777777778;
        float r1729 = 6.0;
        float r1730 = pow(r1722, r1729);
        float r1731 = r1728 * r1730;
        float r1732 = r1727 + r1731;
        float r1733 = r1723 + r1732;
        return r1733;
}

double f_od(double x) {
        double r1734 = 0.002777777777777778;
        double r1735 = x;
        double r1736 = r1735 * r1735;
        double r1737 = r1735 * r1736;
        double r1738 = r1737 * r1737;
        double r1739 = 1.0;
        double r1740 = r1738 * r1739;
        double r1741 = r1734 * r1740;
        double r1742 = 0.08333333333333333;
        double r1743 = r1736 * r1736;
        double r1744 = r1743 * r1739;
        double r1745 = r1742 * r1744;
        double r1746 = r1735 * r1735;
        double r1747 = r1746 * r1739;
        double r1748 = r1739 * r1747;
        double r1749 = r1745 + r1748;
        double r1750 = r1741 + r1749;
        return r1750;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1751, r1752, r1753, r1754, r1755, r1756, r1757;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1751);
        mpfr_init(r1752);
        mpfr_init_set_str(r1753, "2", 10, MPFR_RNDN);
        mpfr_init(r1754);
        mpfr_init(r1755);
        mpfr_init(r1756);
        mpfr_init(r1757);
}

double f_im(double x) {
        mpfr_set_d(r1751, x, MPFR_RNDN);
        mpfr_exp(r1752, r1751, MPFR_RNDN);
        ;
        mpfr_sub(r1754, r1752, r1753, MPFR_RNDN);
        mpfr_neg(r1755, r1751, MPFR_RNDN);
        mpfr_exp(r1756, r1755, MPFR_RNDN);
        mpfr_add(r1757, r1754, r1756, MPFR_RNDN);
        return mpfr_get_d(r1757, MPFR_RNDN);
}

static mpfr_t r1758, r1759, r1760, r1761, r1762, r1763, r1764, r1765, r1766, r1767, r1768, r1769;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1758);
        mpfr_init(r1759);
        mpfr_init_set_str(r1760, "1/12", 10, MPFR_RNDN);
        mpfr_init_set_str(r1761, "4", 10, MPFR_RNDN);
        mpfr_init(r1762);
        mpfr_init(r1763);
        mpfr_init_set_str(r1764, "1/360", 10, MPFR_RNDN);
        mpfr_init_set_str(r1765, "6", 10, MPFR_RNDN);
        mpfr_init(r1766);
        mpfr_init(r1767);
        mpfr_init(r1768);
        mpfr_init(r1769);
}

double f_fm(double x) {
        mpfr_set_d(r1758, x, MPFR_RNDN);
        mpfr_mul(r1759, r1758, r1758, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r1762, r1758, r1761, MPFR_RNDN);
        mpfr_mul(r1763, r1760, r1762, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r1766, r1758, r1765, MPFR_RNDN);
        mpfr_mul(r1767, r1764, r1766, MPFR_RNDN);
        mpfr_add(r1768, r1763, r1767, MPFR_RNDN);
        mpfr_add(r1769, r1759, r1768, MPFR_RNDN);
        return mpfr_get_d(r1769, MPFR_RNDN);
}

static mpfr_t r1770, r1771, r1772, r1773, r1774, r1775, r1776, r1777, r1778, r1779, r1780, r1781, r1782, r1783, r1784, r1785, r1786;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r1770, "1/360", 10, MPFR_RNDN);
        mpfr_init(r1771);
        mpfr_init(r1772);
        mpfr_init(r1773);
        mpfr_init(r1774);
        mpfr_init_set_str(r1775, "1", 10, MPFR_RNDN);
        mpfr_init(r1776);
        mpfr_init(r1777);
        mpfr_init_set_str(r1778, "1/12", 10, MPFR_RNDN);
        mpfr_init(r1779);
        mpfr_init(r1780);
        mpfr_init(r1781);
        mpfr_init(r1782);
        mpfr_init(r1783);
        mpfr_init(r1784);
        mpfr_init(r1785);
        mpfr_init(r1786);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r1771, x, MPFR_RNDN);
        mpfr_mul(r1772, r1771, r1771, MPFR_RNDN);
        mpfr_mul(r1773, r1771, r1772, MPFR_RNDN);
        mpfr_mul(r1774, r1773, r1773, MPFR_RNDN);
        ;
        mpfr_mul(r1776, r1774, r1775, MPFR_RNDN);
        mpfr_mul(r1777, r1770, r1776, MPFR_RNDN);
        ;
        mpfr_mul(r1779, r1772, r1772, MPFR_RNDN);
        mpfr_mul(r1780, r1779, r1775, MPFR_RNDN);
        mpfr_mul(r1781, r1778, r1780, MPFR_RNDN);
        mpfr_mul(r1782, r1771, r1771, MPFR_RNDN);
        mpfr_mul(r1783, r1782, r1775, MPFR_RNDN);
        mpfr_mul(r1784, r1775, r1783, MPFR_RNDN);
        mpfr_add(r1785, r1781, r1784, MPFR_RNDN);
        mpfr_add(r1786, r1777, r1785, MPFR_RNDN);
        return mpfr_get_d(r1786, MPFR_RNDN);
}

