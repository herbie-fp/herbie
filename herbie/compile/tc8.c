#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE example 3.9";

double f_if(float x) {
        float r927 = 1.0;
        float r928 = x;
        float r929 = r927 / r928;
        float r930 = 1.0 / tan(r928);
        float r931 = r929 - r930;
        return r931;
}

double f_id(double x) {
        double r932 = 1.0;
        double r933 = x;
        double r934 = r932 / r933;
        double r935 = 1.0 / tan(r933);
        double r936 = r934 - r935;
        return r936;
}


double f_of(float x) {
        float r937 = x;
        float r938 = -0.31748639047145844;
        bool r939 = r937 < r938;
        float r940 = 1.0;
        float r941 = r940 / r937;
        float r942 = 1.0 / tan(r937);
        float r943 = r941 - r942;
        float r944 = 0.32908858358860016;
        bool r945 = r937 < r944;
        float r946 = 0.3333333333333333;
        float r947 = r946 * r937;
        float r948 = 0.022222222222222223;
        float r949 = 3.0;
        float r950 = pow(r937, r949);
        float r951 = r948 * r950;
        float r952 = 0.0021164021164021165;
        float r953 = 5.0;
        float r954 = pow(r937, r953);
        float r955 = r952 * r954;
        float r956 = r951 + r955;
        float r957 = r947 + r956;
        float r958 = r945 ? r957 : r943;
        float r959 = r939 ? r943 : r958;
        return r959;
}

double f_od(double x) {
        double r960 = x;
        double r961 = -0.02493497906413089;
        bool r962 = r960 < r961;
        double r963 = tan(r960);
        double r964 = r963 - r960;
        double r965 = r963 * r960;
        double r966 = r964 / r965;
        double r967 = 0.025492731297064287;
        bool r968 = r960 < r967;
        double r969 = 0.0021164021164021165;
        double r970 = r960 * r960;
        double r971 = r960 * r970;
        double r972 = r970 * r971;
        double r973 = 1.0;
        double r974 = r972 * r973;
        double r975 = r969 * r974;
        double r976 = 0.022222222222222223;
        double r977 = r971 * r973;
        double r978 = r976 * r977;
        double r979 = 0.3333333333333333;
        double r980 = r960 * r973;
        double r981 = r979 * r980;
        double r982 = r978 + r981;
        double r983 = r975 + r982;
        double r984 = r968 ? r983 : r966;
        double r985 = r962 ? r966 : r984;
        return r985;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r986, r987, r988, r989, r990;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r986, "1", 10, MPFR_RNDN);
        mpfr_init(r987);
        mpfr_init(r988);
        mpfr_init(r989);
        mpfr_init(r990);
}

double f_im(double x) {
        ;
        mpfr_set_d(r987, x, MPFR_RNDN);
        mpfr_div(r988, r986, r987, MPFR_RNDN);
        mpfr_cot(r989, r987, MPFR_RNDN);
        mpfr_sub(r990, r988, r989, MPFR_RNDN);
        return mpfr_get_d(r990, MPFR_RNDN);
}

static mpfr_t r991, r992, r993, r994, r995, r996, r997, r998, r999, r1000, r1001, r1002, r1003, r1004, r1005, r1006, r1007, r1008, r1009, r1010, r1011, r1012, r1013;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r991);
        mpfr_init_set_str(r992, "-0.31748639047145844", 10, MPFR_RNDN);
        mpfr_init(r993);
        mpfr_init_set_str(r994, "1", 10, MPFR_RNDN);
        mpfr_init(r995);
        mpfr_init(r996);
        mpfr_init(r997);
        mpfr_init_set_str(r998, "0.32908858358860016", 10, MPFR_RNDN);
        mpfr_init(r999);
        mpfr_init_set_str(r1000, "1/3", 10, MPFR_RNDN);
        mpfr_init(r1001);
        mpfr_init_set_str(r1002, "1/45", 10, MPFR_RNDN);
        mpfr_init_set_str(r1003, "3", 10, MPFR_RNDN);
        mpfr_init(r1004);
        mpfr_init(r1005);
        mpfr_init_set_str(r1006, "2/945", 10, MPFR_RNDN);
        mpfr_init_set_str(r1007, "5", 10, MPFR_RNDN);
        mpfr_init(r1008);
        mpfr_init(r1009);
        mpfr_init(r1010);
        mpfr_init(r1011);
        mpfr_init(r1012);
        mpfr_init(r1013);
}

double f_fm(double x) {
        mpfr_set_d(r991, x, MPFR_RNDN);
        ;
        mpfr_set_si(r993, mpfr_cmp(r991, r992) < 0, MPFR_RNDN);
        ;
        mpfr_div(r995, r994, r991, MPFR_RNDN);
        mpfr_cot(r996, r991, MPFR_RNDN);
        mpfr_sub(r997, r995, r996, MPFR_RNDN);
        ;
        mpfr_set_si(r999, mpfr_cmp(r991, r998) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r1001, r1000, r991, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r1004, r991, r1003, MPFR_RNDN);
        mpfr_mul(r1005, r1002, r1004, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r1008, r991, r1007, MPFR_RNDN);
        mpfr_mul(r1009, r1006, r1008, MPFR_RNDN);
        mpfr_add(r1010, r1005, r1009, MPFR_RNDN);
        mpfr_add(r1011, r1001, r1010, MPFR_RNDN);
        if (mpfr_get_si(r999, MPFR_RNDN)) { mpfr_set(r1012, r1011, MPFR_RNDN); } else { mpfr_set(r1012, r997, MPFR_RNDN); };
        if (mpfr_get_si(r993, MPFR_RNDN)) { mpfr_set(r1013, r997, MPFR_RNDN); } else { mpfr_set(r1013, r1012, MPFR_RNDN); };
        return mpfr_get_d(r1013, MPFR_RNDN);
}

static mpfr_t r1014, r1015, r1016, r1017, r1018, r1019, r1020, r1021, r1022, r1023, r1024, r1025, r1026, r1027, r1028, r1029, r1030, r1031, r1032, r1033, r1034, r1035, r1036, r1037, r1038, r1039;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1014);
        mpfr_init_set_str(r1015, "-0.02493497906413089", 10, MPFR_RNDN);
        mpfr_init(r1016);
        mpfr_init(r1017);
        mpfr_init(r1018);
        mpfr_init(r1019);
        mpfr_init(r1020);
        mpfr_init_set_str(r1021, "0.025492731297064287", 10, MPFR_RNDN);
        mpfr_init(r1022);
        mpfr_init_set_str(r1023, "2/945", 10, MPFR_RNDN);
        mpfr_init(r1024);
        mpfr_init(r1025);
        mpfr_init(r1026);
        mpfr_init_set_str(r1027, "1", 10, MPFR_RNDN);
        mpfr_init(r1028);
        mpfr_init(r1029);
        mpfr_init_set_str(r1030, "1/45", 10, MPFR_RNDN);
        mpfr_init(r1031);
        mpfr_init(r1032);
        mpfr_init_set_str(r1033, "1/3", 10, MPFR_RNDN);
        mpfr_init(r1034);
        mpfr_init(r1035);
        mpfr_init(r1036);
        mpfr_init(r1037);
        mpfr_init(r1038);
        mpfr_init(r1039);
}

double f_dm(double x) {
        mpfr_set_d(r1014, x, MPFR_RNDN);
        ;
        mpfr_set_si(r1016, mpfr_cmp(r1014, r1015) < 0, MPFR_RNDN);
        mpfr_tan(r1017, r1014, MPFR_RNDN);
        mpfr_sub(r1018, r1017, r1014, MPFR_RNDN);
        mpfr_mul(r1019, r1017, r1014, MPFR_RNDN);
        mpfr_div(r1020, r1018, r1019, MPFR_RNDN);
        ;
        mpfr_set_si(r1022, mpfr_cmp(r1014, r1021) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r1024, r1014, r1014, MPFR_RNDN);
        mpfr_mul(r1025, r1014, r1024, MPFR_RNDN);
        mpfr_mul(r1026, r1024, r1025, MPFR_RNDN);
        ;
        mpfr_mul(r1028, r1026, r1027, MPFR_RNDN);
        mpfr_mul(r1029, r1023, r1028, MPFR_RNDN);
        ;
        mpfr_mul(r1031, r1025, r1027, MPFR_RNDN);
        mpfr_mul(r1032, r1030, r1031, MPFR_RNDN);
        ;
        mpfr_mul(r1034, r1014, r1027, MPFR_RNDN);
        mpfr_mul(r1035, r1033, r1034, MPFR_RNDN);
        mpfr_add(r1036, r1032, r1035, MPFR_RNDN);
        mpfr_add(r1037, r1029, r1036, MPFR_RNDN);
        if (mpfr_get_si(r1022, MPFR_RNDN)) { mpfr_set(r1038, r1037, MPFR_RNDN); } else { mpfr_set(r1038, r1020, MPFR_RNDN); };
        if (mpfr_get_si(r1016, MPFR_RNDN)) { mpfr_set(r1039, r1020, MPFR_RNDN); } else { mpfr_set(r1039, r1038, MPFR_RNDN); };
        return mpfr_get_d(r1039, MPFR_RNDN);
}

