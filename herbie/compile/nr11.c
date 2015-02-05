#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.2";

double f_if(float x, float eps) {
        float r1102 = x;
        float r1103 = eps;
        float r1104 = r1102 + r1103;
        float r1105 = tan(r1104);
        float r1106 = tan(r1102);
        float r1107 = r1105 - r1106;
        return r1107;
}

double f_id(double x, double eps) {
        double r1108 = x;
        double r1109 = eps;
        double r1110 = r1108 + r1109;
        double r1111 = tan(r1110);
        double r1112 = tan(r1108);
        double r1113 = r1111 - r1112;
        return r1113;
}


double f_of(float x, float eps) {
        float r1114 = -0.16666666666666666;
        float r1115 = eps;
        float r1116 = r1115 * r1115;
        float r1117 = r1115 * r1116;
        float r1118 = 1.0;
        float r1119 = r1118 * r1118;
        float r1120 = r1117 * r1119;
        float r1121 = r1114 * r1120;
        float r1122 = r1115 * r1119;
        float r1123 = r1118 * r1122;
        float r1124 = r1121 + r1123;
        float r1125 = x;
        float r1126 = cos(r1125);
        float r1127 = r1125 + r1115;
        float r1128 = cos(r1127);
        float r1129 = r1126 * r1128;
        float r1130 = r1124 / r1129;
        return r1130;
}

double f_od(double x, double eps) {
        double r1131 = -0.16666666666666666;
        double r1132 = eps;
        double r1133 = r1132 * r1132;
        double r1134 = r1132 * r1133;
        double r1135 = 1.0;
        double r1136 = r1135 * r1135;
        double r1137 = r1134 * r1136;
        double r1138 = r1131 * r1137;
        double r1139 = r1132 * r1136;
        double r1140 = r1135 * r1139;
        double r1141 = r1138 + r1140;
        double r1142 = x;
        double r1143 = cos(r1142);
        double r1144 = r1142 + r1132;
        double r1145 = cos(r1144);
        double r1146 = r1143 * r1145;
        double r1147 = r1141 / r1146;
        return r1147;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1148, r1149, r1150, r1151, r1152, r1153;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1148);
        mpfr_init(r1149);
        mpfr_init(r1150);
        mpfr_init(r1151);
        mpfr_init(r1152);
        mpfr_init(r1153);
}

double f_im(double x, double eps) {
        mpfr_set_d(r1148, x, MPFR_RNDN);
        mpfr_set_d(r1149, eps, MPFR_RNDN);
        mpfr_add(r1150, r1148, r1149, MPFR_RNDN);
        mpfr_tan(r1151, r1150, MPFR_RNDN);
        mpfr_tan(r1152, r1148, MPFR_RNDN);
        mpfr_sub(r1153, r1151, r1152, MPFR_RNDN);
        return mpfr_get_d(r1153, MPFR_RNDN);
}

static mpfr_t r1154, r1155, r1156, r1157, r1158, r1159, r1160, r1161, r1162, r1163, r1164, r1165, r1166, r1167, r1168, r1169, r1170;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r1154, "-1/6", 10, MPFR_RNDN);
        mpfr_init(r1155);
        mpfr_init(r1156);
        mpfr_init(r1157);
        mpfr_init_set_str(r1158, "1", 10, MPFR_RNDN);
        mpfr_init(r1159);
        mpfr_init(r1160);
        mpfr_init(r1161);
        mpfr_init(r1162);
        mpfr_init(r1163);
        mpfr_init(r1164);
        mpfr_init(r1165);
        mpfr_init(r1166);
        mpfr_init(r1167);
        mpfr_init(r1168);
        mpfr_init(r1169);
        mpfr_init(r1170);
}

double f_fm(double x, double eps) {
        ;
        mpfr_set_d(r1155, eps, MPFR_RNDN);
        mpfr_mul(r1156, r1155, r1155, MPFR_RNDN);
        mpfr_mul(r1157, r1155, r1156, MPFR_RNDN);
        ;
        mpfr_mul(r1159, r1158, r1158, MPFR_RNDN);
        mpfr_mul(r1160, r1157, r1159, MPFR_RNDN);
        mpfr_mul(r1161, r1154, r1160, MPFR_RNDN);
        mpfr_mul(r1162, r1155, r1159, MPFR_RNDN);
        mpfr_mul(r1163, r1158, r1162, MPFR_RNDN);
        mpfr_add(r1164, r1161, r1163, MPFR_RNDN);
        mpfr_set_d(r1165, x, MPFR_RNDN);
        mpfr_cos(r1166, r1165, MPFR_RNDN);
        mpfr_add(r1167, r1165, r1155, MPFR_RNDN);
        mpfr_cos(r1168, r1167, MPFR_RNDN);
        mpfr_mul(r1169, r1166, r1168, MPFR_RNDN);
        mpfr_div(r1170, r1164, r1169, MPFR_RNDN);
        return mpfr_get_d(r1170, MPFR_RNDN);
}

static mpfr_t r1171, r1172, r1173, r1174, r1175, r1176, r1177, r1178, r1179, r1180, r1181, r1182, r1183, r1184, r1185, r1186, r1187;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r1171, "-1/6", 10, MPFR_RNDN);
        mpfr_init(r1172);
        mpfr_init(r1173);
        mpfr_init(r1174);
        mpfr_init_set_str(r1175, "1", 10, MPFR_RNDN);
        mpfr_init(r1176);
        mpfr_init(r1177);
        mpfr_init(r1178);
        mpfr_init(r1179);
        mpfr_init(r1180);
        mpfr_init(r1181);
        mpfr_init(r1182);
        mpfr_init(r1183);
        mpfr_init(r1184);
        mpfr_init(r1185);
        mpfr_init(r1186);
        mpfr_init(r1187);
}

double f_dm(double x, double eps) {
        ;
        mpfr_set_d(r1172, eps, MPFR_RNDN);
        mpfr_mul(r1173, r1172, r1172, MPFR_RNDN);
        mpfr_mul(r1174, r1172, r1173, MPFR_RNDN);
        ;
        mpfr_mul(r1176, r1175, r1175, MPFR_RNDN);
        mpfr_mul(r1177, r1174, r1176, MPFR_RNDN);
        mpfr_mul(r1178, r1171, r1177, MPFR_RNDN);
        mpfr_mul(r1179, r1172, r1176, MPFR_RNDN);
        mpfr_mul(r1180, r1175, r1179, MPFR_RNDN);
        mpfr_add(r1181, r1178, r1180, MPFR_RNDN);
        mpfr_set_d(r1182, x, MPFR_RNDN);
        mpfr_cos(r1183, r1182, MPFR_RNDN);
        mpfr_add(r1184, r1182, r1172, MPFR_RNDN);
        mpfr_cos(r1185, r1184, MPFR_RNDN);
        mpfr_mul(r1186, r1183, r1185, MPFR_RNDN);
        mpfr_div(r1187, r1181, r1186, MPFR_RNDN);
        return mpfr_get_d(r1187, MPFR_RNDN);
}

