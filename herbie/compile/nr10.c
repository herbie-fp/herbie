#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE p42";

double f_if(float a, float b, float c) {
        float r976 = b;
        float r977 = -r976;
        float r978 = r976 * r976;
        float r979 = 4.0;
        float r980 = a;
        float r981 = c;
        float r982 = r980 * r981;
        float r983 = r979 * r982;
        float r984 = r978 - r983;
        float r985 = sqrt(r984);
        float r986 = r977 + r985;
        float r987 = 2.0;
        float r988 = r987 * r980;
        float r989 = r986 / r988;
        return r989;
}

double f_id(double a, double b, double c) {
        double r990 = b;
        double r991 = -r990;
        double r992 = r990 * r990;
        double r993 = 4.0;
        double r994 = a;
        double r995 = c;
        double r996 = r994 * r995;
        double r997 = r993 * r996;
        double r998 = r992 - r997;
        double r999 = sqrt(r998);
        double r1000 = r991 + r999;
        double r1001 = 2.0;
        double r1002 = r1001 * r994;
        double r1003 = r1000 / r1002;
        return r1003;
}


double f_of(float a, float b, float c) {
        float r1004 = 4.0;
        float r1005 = c;
        float r1006 = a;
        float r1007 = 1.0;
        float r1008 = r1007 * r1007;
        float r1009 = r1006 * r1008;
        float r1010 = r1005 * r1009;
        float r1011 = r1004 * r1010;
        float r1012 = b;
        float r1013 = -r1012;
        float r1014 = r1012 * r1012;
        float r1015 = r1004 * r1005;
        float r1016 = r1015 * r1006;
        float r1017 = r1014 - r1016;
        float r1018 = sqrt(r1017);
        float r1019 = r1013 - r1018;
        float r1020 = r1011 / r1019;
        float r1021 = 2.0;
        float r1022 = r1021 * r1006;
        float r1023 = r1007 / r1022;
        float r1024 = r1020 * r1023;
        return r1024;
}

double f_od(double a, double b, double c) {
        double r1025 = 4.0;
        double r1026 = c;
        double r1027 = a;
        double r1028 = 1.0;
        double r1029 = r1028 * r1028;
        double r1030 = r1027 * r1029;
        double r1031 = r1026 * r1030;
        double r1032 = r1025 * r1031;
        double r1033 = b;
        double r1034 = -r1033;
        double r1035 = r1033 * r1033;
        double r1036 = r1025 * r1026;
        double r1037 = r1036 * r1027;
        double r1038 = r1035 - r1037;
        double r1039 = sqrt(r1038);
        double r1040 = r1034 - r1039;
        double r1041 = r1032 / r1040;
        double r1042 = 2.0;
        double r1043 = r1042 * r1027;
        double r1044 = r1028 / r1043;
        double r1045 = r1041 * r1044;
        return r1045;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1046, r1047, r1048, r1049, r1050, r1051, r1052, r1053, r1054, r1055, r1056, r1057, r1058, r1059;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1046);
        mpfr_init(r1047);
        mpfr_init(r1048);
        mpfr_init_set_str(r1049, "4", 10, MPFR_RNDN);
        mpfr_init(r1050);
        mpfr_init(r1051);
        mpfr_init(r1052);
        mpfr_init(r1053);
        mpfr_init(r1054);
        mpfr_init(r1055);
        mpfr_init(r1056);
        mpfr_init_set_str(r1057, "2", 10, MPFR_RNDN);
        mpfr_init(r1058);
        mpfr_init(r1059);
}

double f_im(double a, double b, double c) {
        mpfr_set_d(r1046, b, MPFR_RNDN);
        mpfr_neg(r1047, r1046, MPFR_RNDN);
        mpfr_mul(r1048, r1046, r1046, MPFR_RNDN);
        ;
        mpfr_set_d(r1050, a, MPFR_RNDN);
        mpfr_set_d(r1051, c, MPFR_RNDN);
        mpfr_mul(r1052, r1050, r1051, MPFR_RNDN);
        mpfr_mul(r1053, r1049, r1052, MPFR_RNDN);
        mpfr_sub(r1054, r1048, r1053, MPFR_RNDN);
        mpfr_sqrt(r1055, r1054, MPFR_RNDN);
        mpfr_add(r1056, r1047, r1055, MPFR_RNDN);
        ;
        mpfr_mul(r1058, r1057, r1050, MPFR_RNDN);
        mpfr_div(r1059, r1056, r1058, MPFR_RNDN);
        return mpfr_get_d(r1059, MPFR_RNDN);
}

static mpfr_t r1060, r1061, r1062, r1063, r1064, r1065, r1066, r1067, r1068, r1069, r1070, r1071, r1072, r1073, r1074, r1075, r1076, r1077, r1078, r1079, r1080;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1060, "4", 10, MPFR_RNDN);
        mpfr_init(r1061);
        mpfr_init(r1062);
        mpfr_init_set_str(r1063, "1", 10, MPFR_RNDN);
        mpfr_init(r1064);
        mpfr_init(r1065);
        mpfr_init(r1066);
        mpfr_init(r1067);
        mpfr_init(r1068);
        mpfr_init(r1069);
        mpfr_init(r1070);
        mpfr_init(r1071);
        mpfr_init(r1072);
        mpfr_init(r1073);
        mpfr_init(r1074);
        mpfr_init(r1075);
        mpfr_init(r1076);
        mpfr_init_set_str(r1077, "2", 10, MPFR_RNDN);
        mpfr_init(r1078);
        mpfr_init(r1079);
        mpfr_init(r1080);
}

double f_fm(double a, double b, double c) {
        ;
        mpfr_set_d(r1061, c, MPFR_RNDN);
        mpfr_set_d(r1062, a, MPFR_RNDN);
        ;
        mpfr_mul(r1064, r1063, r1063, MPFR_RNDN);
        mpfr_mul(r1065, r1062, r1064, MPFR_RNDN);
        mpfr_mul(r1066, r1061, r1065, MPFR_RNDN);
        mpfr_mul(r1067, r1060, r1066, MPFR_RNDN);
        mpfr_set_d(r1068, b, MPFR_RNDN);
        mpfr_neg(r1069, r1068, MPFR_RNDN);
        mpfr_mul(r1070, r1068, r1068, MPFR_RNDN);
        mpfr_mul(r1071, r1060, r1061, MPFR_RNDN);
        mpfr_mul(r1072, r1071, r1062, MPFR_RNDN);
        mpfr_sub(r1073, r1070, r1072, MPFR_RNDN);
        mpfr_sqrt(r1074, r1073, MPFR_RNDN);
        mpfr_sub(r1075, r1069, r1074, MPFR_RNDN);
        mpfr_div(r1076, r1067, r1075, MPFR_RNDN);
        ;
        mpfr_mul(r1078, r1077, r1062, MPFR_RNDN);
        mpfr_div(r1079, r1063, r1078, MPFR_RNDN);
        mpfr_mul(r1080, r1076, r1079, MPFR_RNDN);
        return mpfr_get_d(r1080, MPFR_RNDN);
}

static mpfr_t r1081, r1082, r1083, r1084, r1085, r1086, r1087, r1088, r1089, r1090, r1091, r1092, r1093, r1094, r1095, r1096, r1097, r1098, r1099, r1100, r1101;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1081, "4", 10, MPFR_RNDN);
        mpfr_init(r1082);
        mpfr_init(r1083);
        mpfr_init_set_str(r1084, "1", 10, MPFR_RNDN);
        mpfr_init(r1085);
        mpfr_init(r1086);
        mpfr_init(r1087);
        mpfr_init(r1088);
        mpfr_init(r1089);
        mpfr_init(r1090);
        mpfr_init(r1091);
        mpfr_init(r1092);
        mpfr_init(r1093);
        mpfr_init(r1094);
        mpfr_init(r1095);
        mpfr_init(r1096);
        mpfr_init(r1097);
        mpfr_init_set_str(r1098, "2", 10, MPFR_RNDN);
        mpfr_init(r1099);
        mpfr_init(r1100);
        mpfr_init(r1101);
}

double f_dm(double a, double b, double c) {
        ;
        mpfr_set_d(r1082, c, MPFR_RNDN);
        mpfr_set_d(r1083, a, MPFR_RNDN);
        ;
        mpfr_mul(r1085, r1084, r1084, MPFR_RNDN);
        mpfr_mul(r1086, r1083, r1085, MPFR_RNDN);
        mpfr_mul(r1087, r1082, r1086, MPFR_RNDN);
        mpfr_mul(r1088, r1081, r1087, MPFR_RNDN);
        mpfr_set_d(r1089, b, MPFR_RNDN);
        mpfr_neg(r1090, r1089, MPFR_RNDN);
        mpfr_mul(r1091, r1089, r1089, MPFR_RNDN);
        mpfr_mul(r1092, r1081, r1082, MPFR_RNDN);
        mpfr_mul(r1093, r1092, r1083, MPFR_RNDN);
        mpfr_sub(r1094, r1091, r1093, MPFR_RNDN);
        mpfr_sqrt(r1095, r1094, MPFR_RNDN);
        mpfr_sub(r1096, r1090, r1095, MPFR_RNDN);
        mpfr_div(r1097, r1088, r1096, MPFR_RNDN);
        ;
        mpfr_mul(r1099, r1098, r1083, MPFR_RNDN);
        mpfr_div(r1100, r1084, r1099, MPFR_RNDN);
        mpfr_mul(r1101, r1097, r1100, MPFR_RNDN);
        return mpfr_get_d(r1101, MPFR_RNDN);
}

