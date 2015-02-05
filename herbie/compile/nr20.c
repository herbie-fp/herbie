#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.2.1";

double f_if(float a, float b_2F2, float c) {
        float r1877 = b_2F2;
        float r1878 = -r1877;
        float r1879 = r1877 * r1877;
        float r1880 = a;
        float r1881 = c;
        float r1882 = r1880 * r1881;
        float r1883 = r1879 - r1882;
        float r1884 = sqrt(r1883);
        float r1885 = r1878 - r1884;
        float r1886 = r1885 / r1880;
        return r1886;
}

double f_id(double a, double b_2F2, double c) {
        double r1887 = b_2F2;
        double r1888 = -r1887;
        double r1889 = r1887 * r1887;
        double r1890 = a;
        double r1891 = c;
        double r1892 = r1890 * r1891;
        double r1893 = r1889 - r1892;
        double r1894 = sqrt(r1893);
        double r1895 = r1888 - r1894;
        double r1896 = r1895 / r1890;
        return r1896;
}


double f_of(float a, float b_2F2, float c) {
        float r1897 = b_2F2;
        float r1898 = -r1897;
        float r1899 = r1897 * r1897;
        float r1900 = c;
        float r1901 = a;
        float r1902 = r1900 * r1901;
        float r1903 = r1899 - r1902;
        float r1904 = sqrt(r1903);
        float r1905 = r1898 - r1904;
        float r1906 = 1.0;
        float r1907 = r1906 / r1901;
        float r1908 = r1905 * r1907;
        return r1908;
}

double f_od(double a, double b_2F2, double c) {
        double r1909 = b_2F2;
        double r1910 = -r1909;
        double r1911 = r1909 * r1909;
        double r1912 = c;
        double r1913 = a;
        double r1914 = r1912 * r1913;
        double r1915 = r1911 - r1914;
        double r1916 = sqrt(r1915);
        double r1917 = r1910 - r1916;
        double r1918 = 1.0;
        double r1919 = r1918 / r1913;
        double r1920 = r1917 * r1919;
        return r1920;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1921, r1922, r1923, r1924, r1925, r1926, r1927, r1928, r1929, r1930;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3472);
        mpfr_init(r1921);
        mpfr_init(r1922);
        mpfr_init(r1923);
        mpfr_init(r1924);
        mpfr_init(r1925);
        mpfr_init(r1926);
        mpfr_init(r1927);
        mpfr_init(r1928);
        mpfr_init(r1929);
        mpfr_init(r1930);
}

double f_im(double a, double b_2F2, double c) {
        mpfr_set_d(r1921, b_2F2, MPFR_RNDN);
        mpfr_neg(r1922, r1921, MPFR_RNDN);
        mpfr_mul(r1923, r1921, r1921, MPFR_RNDN);
        mpfr_set_d(r1924, a, MPFR_RNDN);
        mpfr_set_d(r1925, c, MPFR_RNDN);
        mpfr_mul(r1926, r1924, r1925, MPFR_RNDN);
        mpfr_sub(r1927, r1923, r1926, MPFR_RNDN);
        mpfr_sqrt(r1928, r1927, MPFR_RNDN);
        mpfr_sub(r1929, r1922, r1928, MPFR_RNDN);
        mpfr_div(r1930, r1929, r1924, MPFR_RNDN);
        return mpfr_get_d(r1930, MPFR_RNDN);
}

static mpfr_t r1931, r1932, r1933, r1934, r1935, r1936, r1937, r1938, r1939, r1940, r1941, r1942;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r1931);
        mpfr_init(r1932);
        mpfr_init(r1933);
        mpfr_init(r1934);
        mpfr_init(r1935);
        mpfr_init(r1936);
        mpfr_init(r1937);
        mpfr_init(r1938);
        mpfr_init(r1939);
        mpfr_init_set_str(r1940, "1", 10, MPFR_RNDN);
        mpfr_init(r1941);
        mpfr_init(r1942);
}

double f_fm(double a, double b_2F2, double c) {
        mpfr_set_d(r1931, b_2F2, MPFR_RNDN);
        mpfr_neg(r1932, r1931, MPFR_RNDN);
        mpfr_mul(r1933, r1931, r1931, MPFR_RNDN);
        mpfr_set_d(r1934, c, MPFR_RNDN);
        mpfr_set_d(r1935, a, MPFR_RNDN);
        mpfr_mul(r1936, r1934, r1935, MPFR_RNDN);
        mpfr_sub(r1937, r1933, r1936, MPFR_RNDN);
        mpfr_sqrt(r1938, r1937, MPFR_RNDN);
        mpfr_sub(r1939, r1932, r1938, MPFR_RNDN);
        ;
        mpfr_div(r1941, r1940, r1935, MPFR_RNDN);
        mpfr_mul(r1942, r1939, r1941, MPFR_RNDN);
        return mpfr_get_d(r1942, MPFR_RNDN);
}

static mpfr_t r1943, r1944, r1945, r1946, r1947, r1948, r1949, r1950, r1951, r1952, r1953, r1954;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3472);
        mpfr_init(r1943);
        mpfr_init(r1944);
        mpfr_init(r1945);
        mpfr_init(r1946);
        mpfr_init(r1947);
        mpfr_init(r1948);
        mpfr_init(r1949);
        mpfr_init(r1950);
        mpfr_init(r1951);
        mpfr_init_set_str(r1952, "1", 10, MPFR_RNDN);
        mpfr_init(r1953);
        mpfr_init(r1954);
}

double f_dm(double a, double b_2F2, double c) {
        mpfr_set_d(r1943, b_2F2, MPFR_RNDN);
        mpfr_neg(r1944, r1943, MPFR_RNDN);
        mpfr_mul(r1945, r1943, r1943, MPFR_RNDN);
        mpfr_set_d(r1946, c, MPFR_RNDN);
        mpfr_set_d(r1947, a, MPFR_RNDN);
        mpfr_mul(r1948, r1946, r1947, MPFR_RNDN);
        mpfr_sub(r1949, r1945, r1948, MPFR_RNDN);
        mpfr_sqrt(r1950, r1949, MPFR_RNDN);
        mpfr_sub(r1951, r1944, r1950, MPFR_RNDN);
        ;
        mpfr_div(r1953, r1952, r1947, MPFR_RNDN);
        mpfr_mul(r1954, r1951, r1953, MPFR_RNDN);
        return mpfr_get_d(r1954, MPFR_RNDN);
}

