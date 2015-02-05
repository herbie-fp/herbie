#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.2.1";

double f_if(float a, float b_2F2, float c) {
        float r1779 = b_2F2;
        float r1780 = -r1779;
        float r1781 = r1779 * r1779;
        float r1782 = a;
        float r1783 = c;
        float r1784 = r1782 * r1783;
        float r1785 = r1781 - r1784;
        float r1786 = sqrt(r1785);
        float r1787 = r1780 + r1786;
        float r1788 = r1787 / r1782;
        return r1788;
}

double f_id(double a, double b_2F2, double c) {
        double r1789 = b_2F2;
        double r1790 = -r1789;
        double r1791 = r1789 * r1789;
        double r1792 = a;
        double r1793 = c;
        double r1794 = r1792 * r1793;
        double r1795 = r1791 - r1794;
        double r1796 = sqrt(r1795);
        double r1797 = r1790 + r1796;
        double r1798 = r1797 / r1792;
        return r1798;
}


double f_of(float a, float b_2F2, float c) {
        float r1799 = 1.0;
        float r1800 = c;
        float r1801 = a;
        float r1802 = r1799 * r1799;
        float r1803 = r1801 * r1802;
        float r1804 = r1800 * r1803;
        float r1805 = r1799 * r1804;
        float r1806 = b_2F2;
        float r1807 = -r1806;
        float r1808 = r1806 * r1806;
        float r1809 = r1801 * r1800;
        float r1810 = r1808 - r1809;
        float r1811 = sqrt(r1810);
        float r1812 = r1807 - r1811;
        float r1813 = r1805 / r1812;
        float r1814 = r1799 / r1801;
        float r1815 = r1813 * r1814;
        return r1815;
}

double f_od(double a, double b_2F2, double c) {
        double r1816 = 1.0;
        double r1817 = c;
        double r1818 = a;
        double r1819 = r1816 * r1816;
        double r1820 = r1818 * r1819;
        double r1821 = r1817 * r1820;
        double r1822 = r1816 * r1821;
        double r1823 = b_2F2;
        double r1824 = -r1823;
        double r1825 = r1823 * r1823;
        double r1826 = r1818 * r1817;
        double r1827 = r1825 - r1826;
        double r1828 = sqrt(r1827);
        double r1829 = r1824 - r1828;
        double r1830 = r1822 / r1829;
        double r1831 = r1816 / r1818;
        double r1832 = r1830 * r1831;
        return r1832;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1833, r1834, r1835, r1836, r1837, r1838, r1839, r1840, r1841, r1842;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1833);
        mpfr_init(r1834);
        mpfr_init(r1835);
        mpfr_init(r1836);
        mpfr_init(r1837);
        mpfr_init(r1838);
        mpfr_init(r1839);
        mpfr_init(r1840);
        mpfr_init(r1841);
        mpfr_init(r1842);
}

double f_im(double a, double b_2F2, double c) {
        mpfr_set_d(r1833, b_2F2, MPFR_RNDN);
        mpfr_neg(r1834, r1833, MPFR_RNDN);
        mpfr_mul(r1835, r1833, r1833, MPFR_RNDN);
        mpfr_set_d(r1836, a, MPFR_RNDN);
        mpfr_set_d(r1837, c, MPFR_RNDN);
        mpfr_mul(r1838, r1836, r1837, MPFR_RNDN);
        mpfr_sub(r1839, r1835, r1838, MPFR_RNDN);
        mpfr_sqrt(r1840, r1839, MPFR_RNDN);
        mpfr_add(r1841, r1834, r1840, MPFR_RNDN);
        mpfr_div(r1842, r1841, r1836, MPFR_RNDN);
        return mpfr_get_d(r1842, MPFR_RNDN);
}

static mpfr_t r1843, r1844, r1845, r1846, r1847, r1848, r1849, r1850, r1851, r1852, r1853, r1854, r1855, r1856, r1857, r1858, r1859;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1843, "1", 10, MPFR_RNDN);
        mpfr_init(r1844);
        mpfr_init(r1845);
        mpfr_init(r1846);
        mpfr_init(r1847);
        mpfr_init(r1848);
        mpfr_init(r1849);
        mpfr_init(r1850);
        mpfr_init(r1851);
        mpfr_init(r1852);
        mpfr_init(r1853);
        mpfr_init(r1854);
        mpfr_init(r1855);
        mpfr_init(r1856);
        mpfr_init(r1857);
        mpfr_init(r1858);
        mpfr_init(r1859);
}

double f_fm(double a, double b_2F2, double c) {
        ;
        mpfr_set_d(r1844, c, MPFR_RNDN);
        mpfr_set_d(r1845, a, MPFR_RNDN);
        mpfr_mul(r1846, r1843, r1843, MPFR_RNDN);
        mpfr_mul(r1847, r1845, r1846, MPFR_RNDN);
        mpfr_mul(r1848, r1844, r1847, MPFR_RNDN);
        mpfr_mul(r1849, r1843, r1848, MPFR_RNDN);
        mpfr_set_d(r1850, b_2F2, MPFR_RNDN);
        mpfr_neg(r1851, r1850, MPFR_RNDN);
        mpfr_mul(r1852, r1850, r1850, MPFR_RNDN);
        mpfr_mul(r1853, r1845, r1844, MPFR_RNDN);
        mpfr_sub(r1854, r1852, r1853, MPFR_RNDN);
        mpfr_sqrt(r1855, r1854, MPFR_RNDN);
        mpfr_sub(r1856, r1851, r1855, MPFR_RNDN);
        mpfr_div(r1857, r1849, r1856, MPFR_RNDN);
        mpfr_div(r1858, r1843, r1845, MPFR_RNDN);
        mpfr_mul(r1859, r1857, r1858, MPFR_RNDN);
        return mpfr_get_d(r1859, MPFR_RNDN);
}

static mpfr_t r1860, r1861, r1862, r1863, r1864, r1865, r1866, r1867, r1868, r1869, r1870, r1871, r1872, r1873, r1874, r1875, r1876;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1860, "1", 10, MPFR_RNDN);
        mpfr_init(r1861);
        mpfr_init(r1862);
        mpfr_init(r1863);
        mpfr_init(r1864);
        mpfr_init(r1865);
        mpfr_init(r1866);
        mpfr_init(r1867);
        mpfr_init(r1868);
        mpfr_init(r1869);
        mpfr_init(r1870);
        mpfr_init(r1871);
        mpfr_init(r1872);
        mpfr_init(r1873);
        mpfr_init(r1874);
        mpfr_init(r1875);
        mpfr_init(r1876);
}

double f_dm(double a, double b_2F2, double c) {
        ;
        mpfr_set_d(r1861, c, MPFR_RNDN);
        mpfr_set_d(r1862, a, MPFR_RNDN);
        mpfr_mul(r1863, r1860, r1860, MPFR_RNDN);
        mpfr_mul(r1864, r1862, r1863, MPFR_RNDN);
        mpfr_mul(r1865, r1861, r1864, MPFR_RNDN);
        mpfr_mul(r1866, r1860, r1865, MPFR_RNDN);
        mpfr_set_d(r1867, b_2F2, MPFR_RNDN);
        mpfr_neg(r1868, r1867, MPFR_RNDN);
        mpfr_mul(r1869, r1867, r1867, MPFR_RNDN);
        mpfr_mul(r1870, r1862, r1861, MPFR_RNDN);
        mpfr_sub(r1871, r1869, r1870, MPFR_RNDN);
        mpfr_sqrt(r1872, r1871, MPFR_RNDN);
        mpfr_sub(r1873, r1868, r1872, MPFR_RNDN);
        mpfr_div(r1874, r1866, r1873, MPFR_RNDN);
        mpfr_div(r1875, r1860, r1862, MPFR_RNDN);
        mpfr_mul(r1876, r1874, r1875, MPFR_RNDN);
        return mpfr_get_d(r1876, MPFR_RNDN);
}

