#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.1";

double f_if(float x) {
        float r1787 = 1.0;
        float r1788 = x;
        float r1789 = cos(r1788);
        float r1790 = r1787 - r1789;
        float r1791 = r1788 * r1788;
        float r1792 = r1790 / r1791;
        return r1792;
}

double f_id(double x) {
        double r1793 = 1.0;
        double r1794 = x;
        double r1795 = cos(r1794);
        double r1796 = r1793 - r1795;
        double r1797 = r1794 * r1794;
        double r1798 = r1796 / r1797;
        return r1798;
}


double f_of(float x) {
        float r1799 = x;
        float r1800 = sin(r1799);
        float r1801 = 1.0;
        float r1802 = r1800 / r1801;
        float r1803 = r1802 / r1799;
        float r1804 = cos(r1799);
        float r1805 = r1804 + r1801;
        float r1806 = r1800 / r1805;
        float r1807 = r1806 / r1799;
        float r1808 = r1803 * r1807;
        return r1808;
}

double f_od(double x) {
        double r1809 = x;
        double r1810 = sin(r1809);
        double r1811 = 1.0;
        double r1812 = r1810 / r1811;
        double r1813 = r1812 / r1809;
        double r1814 = cos(r1809);
        double r1815 = r1814 + r1811;
        double r1816 = r1810 / r1815;
        double r1817 = r1816 / r1809;
        double r1818 = r1813 * r1817;
        return r1818;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1819, r1820, r1821, r1822, r1823, r1824;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1819, "1", 10, MPFR_RNDN);
        mpfr_init(r1820);
        mpfr_init(r1821);
        mpfr_init(r1822);
        mpfr_init(r1823);
        mpfr_init(r1824);
}

double f_im(double x) {
        ;
        mpfr_set_d(r1820, x, MPFR_RNDN);
        mpfr_cos(r1821, r1820, MPFR_RNDN);
        mpfr_sub(r1822, r1819, r1821, MPFR_RNDN);
        mpfr_mul(r1823, r1820, r1820, MPFR_RNDN);
        mpfr_div(r1824, r1822, r1823, MPFR_RNDN);
        return mpfr_get_d(r1824, MPFR_RNDN);
}

static mpfr_t r1825, r1826, r1827, r1828, r1829, r1830, r1831, r1832, r1833, r1834;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1825);
        mpfr_init(r1826);
        mpfr_init_set_str(r1827, "1", 10, MPFR_RNDN);
        mpfr_init(r1828);
        mpfr_init(r1829);
        mpfr_init(r1830);
        mpfr_init(r1831);
        mpfr_init(r1832);
        mpfr_init(r1833);
        mpfr_init(r1834);
}

double f_fm(double x) {
        mpfr_set_d(r1825, x, MPFR_RNDN);
        mpfr_sin(r1826, r1825, MPFR_RNDN);
        ;
        mpfr_div(r1828, r1826, r1827, MPFR_RNDN);
        mpfr_div(r1829, r1828, r1825, MPFR_RNDN);
        mpfr_cos(r1830, r1825, MPFR_RNDN);
        mpfr_add(r1831, r1830, r1827, MPFR_RNDN);
        mpfr_div(r1832, r1826, r1831, MPFR_RNDN);
        mpfr_div(r1833, r1832, r1825, MPFR_RNDN);
        mpfr_mul(r1834, r1829, r1833, MPFR_RNDN);
        return mpfr_get_d(r1834, MPFR_RNDN);
}

static mpfr_t r1835, r1836, r1837, r1838, r1839, r1840, r1841, r1842, r1843, r1844;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1835);
        mpfr_init(r1836);
        mpfr_init_set_str(r1837, "1", 10, MPFR_RNDN);
        mpfr_init(r1838);
        mpfr_init(r1839);
        mpfr_init(r1840);
        mpfr_init(r1841);
        mpfr_init(r1842);
        mpfr_init(r1843);
        mpfr_init(r1844);
}

double f_dm(double x) {
        mpfr_set_d(r1835, x, MPFR_RNDN);
        mpfr_sin(r1836, r1835, MPFR_RNDN);
        ;
        mpfr_div(r1838, r1836, r1837, MPFR_RNDN);
        mpfr_div(r1839, r1838, r1835, MPFR_RNDN);
        mpfr_cos(r1840, r1835, MPFR_RNDN);
        mpfr_add(r1841, r1840, r1837, MPFR_RNDN);
        mpfr_div(r1842, r1836, r1841, MPFR_RNDN);
        mpfr_div(r1843, r1842, r1835, MPFR_RNDN);
        mpfr_mul(r1844, r1839, r1843, MPFR_RNDN);
        return mpfr_get_d(r1844, MPFR_RNDN);
}

