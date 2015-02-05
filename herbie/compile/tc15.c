#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.2";

double f_if(float a, float b, float eps) {
        float r1845 = eps;
        float r1846 = a;
        float r1847 = b;
        float r1848 = r1846 + r1847;
        float r1849 = r1848 * r1845;
        float r1850 = exp(r1849);
        float r1851 = 1.0;
        float r1852 = r1850 - r1851;
        float r1853 = r1845 * r1852;
        float r1854 = r1846 * r1845;
        float r1855 = exp(r1854);
        float r1856 = r1855 - r1851;
        float r1857 = r1847 * r1845;
        float r1858 = exp(r1857);
        float r1859 = r1858 - r1851;
        float r1860 = r1856 * r1859;
        float r1861 = r1853 / r1860;
        return r1861;
}

double f_id(double a, double b, double eps) {
        double r1862 = eps;
        double r1863 = a;
        double r1864 = b;
        double r1865 = r1863 + r1864;
        double r1866 = r1865 * r1862;
        double r1867 = exp(r1866);
        double r1868 = 1.0;
        double r1869 = r1867 - r1868;
        double r1870 = r1862 * r1869;
        double r1871 = r1863 * r1862;
        double r1872 = exp(r1871);
        double r1873 = r1872 - r1868;
        double r1874 = r1864 * r1862;
        double r1875 = exp(r1874);
        double r1876 = r1875 - r1868;
        double r1877 = r1873 * r1876;
        double r1878 = r1870 / r1877;
        return r1878;
}


double f_of(float a, float b, float eps) {
        float r1879 = eps;
        float r1880 = -534.0193481445312;
        bool r1881 = r1879 < r1880;
        float r1882 = a;
        float r1883 = r1882 * r1879;
        float r1884 = exp(r1883);
        float r1885 = 1.0;
        float r1886 = r1884 - r1885;
        float r1887 = r1879 / r1886;
        float r1888 = b;
        float r1889 = r1882 + r1888;
        float r1890 = r1879 * r1889;
        float r1891 = exp(r1890);
        float r1892 = sqrt(r1891);
        float r1893 = r1892 + r1885;
        float r1894 = sqrt(r1892);
        float r1895 = r1894 + r1885;
        float r1896 = r1894 - r1885;
        float r1897 = r1895 * r1896;
        float r1898 = r1893 * r1897;
        float r1899 = r1888 * r1879;
        float r1900 = exp(r1899);
        float r1901 = sqrt(r1900);
        float r1902 = r1885 + r1901;
        float r1903 = r1901 - r1885;
        float r1904 = r1902 * r1903;
        float r1905 = r1898 / r1904;
        float r1906 = r1887 * r1905;
        float r1907 = 11.861969947814941;
        bool r1908 = r1879 < r1907;
        float r1909 = -0.5;
        float r1910 = r1909 * r1879;
        float r1911 = 0.08333333333333333;
        float r1912 = r1879 * r1879;
        float r1913 = r1912 * r1882;
        float r1914 = r1911 * r1913;
        float r1915 = r1885 / r1882;
        float r1916 = r1914 + r1915;
        float r1917 = r1910 + r1916;
        float r1918 = r1891 - r1885;
        float r1919 = r1879 * r1888;
        float r1920 = exp(r1919);
        float r1921 = r1920 - r1885;
        float r1922 = r1918 / r1921;
        float r1923 = r1917 * r1922;
        float r1924 = r1908 ? r1923 : r1906;
        float r1925 = r1881 ? r1906 : r1924;
        return r1925;
}

double f_od(double a, double b, double eps) {
        double r1926 = eps;
        double r1927 = -7.514744253122606e+139;
        bool r1928 = r1926 < r1927;
        double r1929 = 1.0;
        double r1930 = a;
        double r1931 = r1926 * r1930;
        double r1932 = exp(r1931);
        double r1933 = sqrt(r1932);
        double r1934 = r1933 - r1929;
        double r1935 = r1929 + r1933;
        double r1936 = r1934 * r1935;
        double r1937 = r1929 / r1936;
        double r1938 = r1926 * r1937;
        double r1939 = b;
        double r1940 = r1930 + r1939;
        double r1941 = r1926 * r1940;
        double r1942 = exp(r1941);
        double r1943 = r1942 - r1929;
        double r1944 = r1926 * r1939;
        double r1945 = exp(r1944);
        double r1946 = r1945 - r1929;
        double r1947 = r1943 / r1946;
        double r1948 = r1938 * r1947;
        double r1949 = 4.677050181470991e+43;
        bool r1950 = r1926 < r1949;
        double r1951 = -0.5;
        double r1952 = r1926 * r1929;
        double r1953 = r1929 * r1952;
        double r1954 = r1951 * r1953;
        double r1955 = r1929 / r1930;
        double r1956 = r1929 * r1929;
        double r1957 = r1955 * r1956;
        double r1958 = r1929 * r1957;
        double r1959 = r1954 + r1958;
        double r1960 = r1959 * r1947;
        double r1961 = r1930 * r1926;
        double r1962 = exp(r1961);
        double r1963 = r1962 - r1929;
        double r1964 = r1926 / r1963;
        double r1965 = r1939 * r1926;
        double r1966 = exp(r1965);
        double r1967 = sqrt(r1966);
        double r1968 = r1929 + r1967;
        double r1969 = r1967 - r1929;
        double r1970 = r1968 * r1969;
        double r1971 = r1943 / r1970;
        double r1972 = r1964 * r1971;
        double r1973 = r1950 ? r1960 : r1972;
        double r1974 = r1928 ? r1948 : r1973;
        return r1974;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1975, r1976, r1977, r1978, r1979, r1980, r1981, r1982, r1983, r1984, r1985, r1986, r1987, r1988, r1989, r1990, r1991;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1975);
        mpfr_init(r1976);
        mpfr_init(r1977);
        mpfr_init(r1978);
        mpfr_init(r1979);
        mpfr_init(r1980);
        mpfr_init_set_str(r1981, "1", 10, MPFR_RNDN);
        mpfr_init(r1982);
        mpfr_init(r1983);
        mpfr_init(r1984);
        mpfr_init(r1985);
        mpfr_init(r1986);
        mpfr_init(r1987);
        mpfr_init(r1988);
        mpfr_init(r1989);
        mpfr_init(r1990);
        mpfr_init(r1991);
}

double f_im(double a, double b, double eps) {
        mpfr_set_d(r1975, eps, MPFR_RNDN);
        mpfr_set_d(r1976, a, MPFR_RNDN);
        mpfr_set_d(r1977, b, MPFR_RNDN);
        mpfr_add(r1978, r1976, r1977, MPFR_RNDN);
        mpfr_mul(r1979, r1978, r1975, MPFR_RNDN);
        mpfr_exp(r1980, r1979, MPFR_RNDN);
        ;
        mpfr_sub(r1982, r1980, r1981, MPFR_RNDN);
        mpfr_mul(r1983, r1975, r1982, MPFR_RNDN);
        mpfr_mul(r1984, r1976, r1975, MPFR_RNDN);
        mpfr_exp(r1985, r1984, MPFR_RNDN);
        mpfr_sub(r1986, r1985, r1981, MPFR_RNDN);
        mpfr_mul(r1987, r1977, r1975, MPFR_RNDN);
        mpfr_exp(r1988, r1987, MPFR_RNDN);
        mpfr_sub(r1989, r1988, r1981, MPFR_RNDN);
        mpfr_mul(r1990, r1986, r1989, MPFR_RNDN);
        mpfr_div(r1991, r1983, r1990, MPFR_RNDN);
        return mpfr_get_d(r1991, MPFR_RNDN);
}

static mpfr_t r1992, r1993, r1994, r1995, r1996, r1997, r1998, r1999, r2000, r2001, r2002, r2003, r2004, r2005, r2006, r2007, r2008, r2009, r2010, r2011, r2012, r2013, r2014, r2015, r2016, r2017, r2018, r2019, r2020, r2021, r2022, r2023, r2024, r2025, r2026, r2027, r2028, r2029, r2030, r2031, r2032, r2033, r2034, r2035, r2036, r2037, r2038;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1992);
        mpfr_init_set_str(r1993, "-534.0193481445312", 10, MPFR_RNDN);
        mpfr_init(r1994);
        mpfr_init(r1995);
        mpfr_init(r1996);
        mpfr_init(r1997);
        mpfr_init_set_str(r1998, "1", 10, MPFR_RNDN);
        mpfr_init(r1999);
        mpfr_init(r2000);
        mpfr_init(r2001);
        mpfr_init(r2002);
        mpfr_init(r2003);
        mpfr_init(r2004);
        mpfr_init(r2005);
        mpfr_init(r2006);
        mpfr_init(r2007);
        mpfr_init(r2008);
        mpfr_init(r2009);
        mpfr_init(r2010);
        mpfr_init(r2011);
        mpfr_init(r2012);
        mpfr_init(r2013);
        mpfr_init(r2014);
        mpfr_init(r2015);
        mpfr_init(r2016);
        mpfr_init(r2017);
        mpfr_init(r2018);
        mpfr_init(r2019);
        mpfr_init_set_str(r2020, "11.861969947814941", 10, MPFR_RNDN);
        mpfr_init(r2021);
        mpfr_init_set_str(r2022, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2023);
        mpfr_init_set_str(r2024, "1/12", 10, MPFR_RNDN);
        mpfr_init(r2025);
        mpfr_init(r2026);
        mpfr_init(r2027);
        mpfr_init(r2028);
        mpfr_init(r2029);
        mpfr_init(r2030);
        mpfr_init(r2031);
        mpfr_init(r2032);
        mpfr_init(r2033);
        mpfr_init(r2034);
        mpfr_init(r2035);
        mpfr_init(r2036);
        mpfr_init(r2037);
        mpfr_init(r2038);
}

double f_fm(double a, double b, double eps) {
        mpfr_set_d(r1992, eps, MPFR_RNDN);
        ;
        mpfr_set_si(r1994, mpfr_cmp(r1992, r1993) < 0, MPFR_RNDN);
        mpfr_set_d(r1995, a, MPFR_RNDN);
        mpfr_mul(r1996, r1995, r1992, MPFR_RNDN);
        mpfr_exp(r1997, r1996, MPFR_RNDN);
        ;
        mpfr_sub(r1999, r1997, r1998, MPFR_RNDN);
        mpfr_div(r2000, r1992, r1999, MPFR_RNDN);
        mpfr_set_d(r2001, b, MPFR_RNDN);
        mpfr_add(r2002, r1995, r2001, MPFR_RNDN);
        mpfr_mul(r2003, r1992, r2002, MPFR_RNDN);
        mpfr_exp(r2004, r2003, MPFR_RNDN);
        mpfr_sqrt(r2005, r2004, MPFR_RNDN);
        mpfr_add(r2006, r2005, r1998, MPFR_RNDN);
        mpfr_sqrt(r2007, r2005, MPFR_RNDN);
        mpfr_add(r2008, r2007, r1998, MPFR_RNDN);
        mpfr_sub(r2009, r2007, r1998, MPFR_RNDN);
        mpfr_mul(r2010, r2008, r2009, MPFR_RNDN);
        mpfr_mul(r2011, r2006, r2010, MPFR_RNDN);
        mpfr_mul(r2012, r2001, r1992, MPFR_RNDN);
        mpfr_exp(r2013, r2012, MPFR_RNDN);
        mpfr_sqrt(r2014, r2013, MPFR_RNDN);
        mpfr_add(r2015, r1998, r2014, MPFR_RNDN);
        mpfr_sub(r2016, r2014, r1998, MPFR_RNDN);
        mpfr_mul(r2017, r2015, r2016, MPFR_RNDN);
        mpfr_div(r2018, r2011, r2017, MPFR_RNDN);
        mpfr_mul(r2019, r2000, r2018, MPFR_RNDN);
        ;
        mpfr_set_si(r2021, mpfr_cmp(r1992, r2020) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r2023, r2022, r1992, MPFR_RNDN);
        ;
        mpfr_mul(r2025, r1992, r1992, MPFR_RNDN);
        mpfr_mul(r2026, r2025, r1995, MPFR_RNDN);
        mpfr_mul(r2027, r2024, r2026, MPFR_RNDN);
        mpfr_div(r2028, r1998, r1995, MPFR_RNDN);
        mpfr_add(r2029, r2027, r2028, MPFR_RNDN);
        mpfr_add(r2030, r2023, r2029, MPFR_RNDN);
        mpfr_sub(r2031, r2004, r1998, MPFR_RNDN);
        mpfr_mul(r2032, r1992, r2001, MPFR_RNDN);
        mpfr_exp(r2033, r2032, MPFR_RNDN);
        mpfr_sub(r2034, r2033, r1998, MPFR_RNDN);
        mpfr_div(r2035, r2031, r2034, MPFR_RNDN);
        mpfr_mul(r2036, r2030, r2035, MPFR_RNDN);
        if (mpfr_get_si(r2021, MPFR_RNDN)) { mpfr_set(r2037, r2036, MPFR_RNDN); } else { mpfr_set(r2037, r2019, MPFR_RNDN); };
        if (mpfr_get_si(r1994, MPFR_RNDN)) { mpfr_set(r2038, r2019, MPFR_RNDN); } else { mpfr_set(r2038, r2037, MPFR_RNDN); };
        return mpfr_get_d(r2038, MPFR_RNDN);
}

static mpfr_t r2039, r2040, r2041, r2042, r2043, r2044, r2045, r2046, r2047, r2048, r2049, r2050, r2051, r2052, r2053, r2054, r2055, r2056, r2057, r2058, r2059, r2060, r2061, r2062, r2063, r2064, r2065, r2066, r2067, r2068, r2069, r2070, r2071, r2072, r2073, r2074, r2075, r2076, r2077, r2078, r2079, r2080, r2081, r2082, r2083, r2084, r2085, r2086, r2087;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r2039);
        mpfr_init_set_str(r2040, "-7.514744253122606e+139", 10, MPFR_RNDN);
        mpfr_init(r2041);
        mpfr_init_set_str(r2042, "1", 10, MPFR_RNDN);
        mpfr_init(r2043);
        mpfr_init(r2044);
        mpfr_init(r2045);
        mpfr_init(r2046);
        mpfr_init(r2047);
        mpfr_init(r2048);
        mpfr_init(r2049);
        mpfr_init(r2050);
        mpfr_init(r2051);
        mpfr_init(r2052);
        mpfr_init(r2053);
        mpfr_init(r2054);
        mpfr_init(r2055);
        mpfr_init(r2056);
        mpfr_init(r2057);
        mpfr_init(r2058);
        mpfr_init(r2059);
        mpfr_init(r2060);
        mpfr_init(r2061);
        mpfr_init_set_str(r2062, "4.677050181470991e+43", 10, MPFR_RNDN);
        mpfr_init(r2063);
        mpfr_init_set_str(r2064, "-1/2", 10, MPFR_RNDN);
        mpfr_init(r2065);
        mpfr_init(r2066);
        mpfr_init(r2067);
        mpfr_init(r2068);
        mpfr_init(r2069);
        mpfr_init(r2070);
        mpfr_init(r2071);
        mpfr_init(r2072);
        mpfr_init(r2073);
        mpfr_init(r2074);
        mpfr_init(r2075);
        mpfr_init(r2076);
        mpfr_init(r2077);
        mpfr_init(r2078);
        mpfr_init(r2079);
        mpfr_init(r2080);
        mpfr_init(r2081);
        mpfr_init(r2082);
        mpfr_init(r2083);
        mpfr_init(r2084);
        mpfr_init(r2085);
        mpfr_init(r2086);
        mpfr_init(r2087);
}

double f_dm(double a, double b, double eps) {
        mpfr_set_d(r2039, eps, MPFR_RNDN);
        ;
        mpfr_set_si(r2041, mpfr_cmp(r2039, r2040) < 0, MPFR_RNDN);
        ;
        mpfr_set_d(r2043, a, MPFR_RNDN);
        mpfr_mul(r2044, r2039, r2043, MPFR_RNDN);
        mpfr_exp(r2045, r2044, MPFR_RNDN);
        mpfr_sqrt(r2046, r2045, MPFR_RNDN);
        mpfr_sub(r2047, r2046, r2042, MPFR_RNDN);
        mpfr_add(r2048, r2042, r2046, MPFR_RNDN);
        mpfr_mul(r2049, r2047, r2048, MPFR_RNDN);
        mpfr_div(r2050, r2042, r2049, MPFR_RNDN);
        mpfr_mul(r2051, r2039, r2050, MPFR_RNDN);
        mpfr_set_d(r2052, b, MPFR_RNDN);
        mpfr_add(r2053, r2043, r2052, MPFR_RNDN);
        mpfr_mul(r2054, r2039, r2053, MPFR_RNDN);
        mpfr_exp(r2055, r2054, MPFR_RNDN);
        mpfr_sub(r2056, r2055, r2042, MPFR_RNDN);
        mpfr_mul(r2057, r2039, r2052, MPFR_RNDN);
        mpfr_exp(r2058, r2057, MPFR_RNDN);
        mpfr_sub(r2059, r2058, r2042, MPFR_RNDN);
        mpfr_div(r2060, r2056, r2059, MPFR_RNDN);
        mpfr_mul(r2061, r2051, r2060, MPFR_RNDN);
        ;
        mpfr_set_si(r2063, mpfr_cmp(r2039, r2062) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r2065, r2039, r2042, MPFR_RNDN);
        mpfr_mul(r2066, r2042, r2065, MPFR_RNDN);
        mpfr_mul(r2067, r2064, r2066, MPFR_RNDN);
        mpfr_div(r2068, r2042, r2043, MPFR_RNDN);
        mpfr_mul(r2069, r2042, r2042, MPFR_RNDN);
        mpfr_mul(r2070, r2068, r2069, MPFR_RNDN);
        mpfr_mul(r2071, r2042, r2070, MPFR_RNDN);
        mpfr_add(r2072, r2067, r2071, MPFR_RNDN);
        mpfr_mul(r2073, r2072, r2060, MPFR_RNDN);
        mpfr_mul(r2074, r2043, r2039, MPFR_RNDN);
        mpfr_exp(r2075, r2074, MPFR_RNDN);
        mpfr_sub(r2076, r2075, r2042, MPFR_RNDN);
        mpfr_div(r2077, r2039, r2076, MPFR_RNDN);
        mpfr_mul(r2078, r2052, r2039, MPFR_RNDN);
        mpfr_exp(r2079, r2078, MPFR_RNDN);
        mpfr_sqrt(r2080, r2079, MPFR_RNDN);
        mpfr_add(r2081, r2042, r2080, MPFR_RNDN);
        mpfr_sub(r2082, r2080, r2042, MPFR_RNDN);
        mpfr_mul(r2083, r2081, r2082, MPFR_RNDN);
        mpfr_div(r2084, r2056, r2083, MPFR_RNDN);
        mpfr_mul(r2085, r2077, r2084, MPFR_RNDN);
        if (mpfr_get_si(r2063, MPFR_RNDN)) { mpfr_set(r2086, r2073, MPFR_RNDN); } else { mpfr_set(r2086, r2085, MPFR_RNDN); };
        if (mpfr_get_si(r2041, MPFR_RNDN)) { mpfr_set(r2087, r2061, MPFR_RNDN); } else { mpfr_set(r2087, r2086, MPFR_RNDN); };
        return mpfr_get_d(r2087, MPFR_RNDN);
}

