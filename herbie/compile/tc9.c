#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE p42";

double f_if(float a, float b, float c) {
        float r1040 = b;
        float r1041 = -r1040;
        float r1042 = r1040 * r1040;
        float r1043 = 4.0;
        float r1044 = a;
        float r1045 = c;
        float r1046 = r1044 * r1045;
        float r1047 = r1043 * r1046;
        float r1048 = r1042 - r1047;
        float r1049 = sqrt(r1048);
        float r1050 = r1041 - r1049;
        float r1051 = 2.0;
        float r1052 = r1051 * r1044;
        float r1053 = r1050 / r1052;
        return r1053;
}

double f_id(double a, double b, double c) {
        double r1054 = b;
        double r1055 = -r1054;
        double r1056 = r1054 * r1054;
        double r1057 = 4.0;
        double r1058 = a;
        double r1059 = c;
        double r1060 = r1058 * r1059;
        double r1061 = r1057 * r1060;
        double r1062 = r1056 - r1061;
        double r1063 = sqrt(r1062);
        double r1064 = r1055 - r1063;
        double r1065 = 2.0;
        double r1066 = r1065 * r1058;
        double r1067 = r1064 / r1066;
        return r1067;
}


double f_of(float a, float b, float c) {
        float r1068 = b;
        float r1069 = -2.989842257199244e-32;
        bool r1070 = r1068 < r1069;
        float r1071 = 4.0;
        float r1072 = c;
        float r1073 = a;
        float r1074 = r1072 * r1073;
        float r1075 = r1071 * r1074;
        float r1076 = -r1068;
        float r1077 = r1068 * r1068;
        float r1078 = r1071 * r1072;
        float r1079 = r1078 * r1073;
        float r1080 = r1077 - r1079;
        float r1081 = sqrt(r1080);
        float r1082 = r1076 + r1081;
        float r1083 = r1075 / r1082;
        float r1084 = 1.0;
        float r1085 = 2.0;
        float r1086 = r1085 * r1073;
        float r1087 = r1084 / r1086;
        float r1088 = r1083 * r1087;
        float r1089 = 1010939067367424.0;
        bool r1090 = r1068 < r1089;
        float r1091 = r1073 * r1071;
        float r1092 = r1091 * r1072;
        float r1093 = r1077 - r1092;
        float r1094 = sqrt(r1093);
        float r1095 = r1076 - r1094;
        float r1096 = r1095 * r1087;
        float r1097 = -1.0;
        float r1098 = r1068 / r1073;
        float r1099 = r1097 * r1098;
        float r1100 = r1072 / r1068;
        float r1101 = r1099 + r1100;
        float r1102 = r1090 ? r1096 : r1101;
        float r1103 = r1070 ? r1088 : r1102;
        return r1103;
}

double f_od(double a, double b, double c) {
        double r1104 = b;
        double r1105 = -5.12178672906159e-80;
        bool r1106 = r1104 < r1105;
        double r1107 = 4.0;
        double r1108 = 1.0;
        double r1109 = r1108 / r1108;
        double r1110 = a;
        double r1111 = r1108 / r1110;
        double r1112 = r1109 / r1111;
        double r1113 = c;
        double r1114 = r1108 / r1113;
        double r1115 = r1112 / r1114;
        double r1116 = r1107 * r1115;
        double r1117 = -r1104;
        double r1118 = r1104 * r1104;
        double r1119 = r1107 * r1113;
        double r1120 = r1119 * r1110;
        double r1121 = r1118 - r1120;
        double r1122 = sqrt(r1121);
        double r1123 = r1117 + r1122;
        double r1124 = r1116 / r1123;
        double r1125 = 2.0;
        double r1126 = r1125 * r1110;
        double r1127 = r1108 / r1126;
        double r1128 = r1124 * r1127;
        double r1129 = 4.4464930377598826e+102;
        bool r1130 = r1104 < r1129;
        double r1131 = r1113 * r1107;
        double r1132 = r1131 * r1110;
        double r1133 = r1118 - r1132;
        double r1134 = sqrt(r1133);
        double r1135 = sqrt(r1134);
        double r1136 = r1135 * r1135;
        double r1137 = r1117 - r1136;
        double r1138 = r1137 / r1126;
        double r1139 = r1108 / r1104;
        double r1140 = r1139 / r1108;
        double r1141 = r1140 / r1114;
        double r1142 = r1108 * r1141;
        double r1143 = -1.0;
        double r1144 = r1108 / r1139;
        double r1145 = r1144 / r1110;
        double r1146 = r1145 / r1108;
        double r1147 = r1143 * r1146;
        double r1148 = r1142 + r1147;
        double r1149 = r1130 ? r1138 : r1148;
        double r1150 = r1106 ? r1128 : r1149;
        return r1150;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1151, r1152, r1153, r1154, r1155, r1156, r1157, r1158, r1159, r1160, r1161, r1162, r1163, r1164;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1151);
        mpfr_init(r1152);
        mpfr_init(r1153);
        mpfr_init_set_str(r1154, "4", 10, MPFR_RNDN);
        mpfr_init(r1155);
        mpfr_init(r1156);
        mpfr_init(r1157);
        mpfr_init(r1158);
        mpfr_init(r1159);
        mpfr_init(r1160);
        mpfr_init(r1161);
        mpfr_init_set_str(r1162, "2", 10, MPFR_RNDN);
        mpfr_init(r1163);
        mpfr_init(r1164);
}

double f_im(double a, double b, double c) {
        mpfr_set_d(r1151, b, MPFR_RNDN);
        mpfr_neg(r1152, r1151, MPFR_RNDN);
        mpfr_mul(r1153, r1151, r1151, MPFR_RNDN);
        ;
        mpfr_set_d(r1155, a, MPFR_RNDN);
        mpfr_set_d(r1156, c, MPFR_RNDN);
        mpfr_mul(r1157, r1155, r1156, MPFR_RNDN);
        mpfr_mul(r1158, r1154, r1157, MPFR_RNDN);
        mpfr_sub(r1159, r1153, r1158, MPFR_RNDN);
        mpfr_sqrt(r1160, r1159, MPFR_RNDN);
        mpfr_sub(r1161, r1152, r1160, MPFR_RNDN);
        ;
        mpfr_mul(r1163, r1162, r1155, MPFR_RNDN);
        mpfr_div(r1164, r1161, r1163, MPFR_RNDN);
        return mpfr_get_d(r1164, MPFR_RNDN);
}

static mpfr_t r1165, r1166, r1167, r1168, r1169, r1170, r1171, r1172, r1173, r1174, r1175, r1176, r1177, r1178, r1179, r1180, r1181, r1182, r1183, r1184, r1185, r1186, r1187, r1188, r1189, r1190, r1191, r1192, r1193, r1194, r1195, r1196, r1197, r1198, r1199, r1200;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1165);
        mpfr_init_set_str(r1166, "-2.989842257199244e-32", 10, MPFR_RNDN);
        mpfr_init(r1167);
        mpfr_init_set_str(r1168, "4", 10, MPFR_RNDN);
        mpfr_init(r1169);
        mpfr_init(r1170);
        mpfr_init(r1171);
        mpfr_init(r1172);
        mpfr_init(r1173);
        mpfr_init(r1174);
        mpfr_init(r1175);
        mpfr_init(r1176);
        mpfr_init(r1177);
        mpfr_init(r1178);
        mpfr_init(r1179);
        mpfr_init(r1180);
        mpfr_init_set_str(r1181, "1", 10, MPFR_RNDN);
        mpfr_init_set_str(r1182, "2", 10, MPFR_RNDN);
        mpfr_init(r1183);
        mpfr_init(r1184);
        mpfr_init(r1185);
        mpfr_init_set_str(r1186, "1010939067367424.0", 10, MPFR_RNDN);
        mpfr_init(r1187);
        mpfr_init(r1188);
        mpfr_init(r1189);
        mpfr_init(r1190);
        mpfr_init(r1191);
        mpfr_init(r1192);
        mpfr_init(r1193);
        mpfr_init_set_str(r1194, "-1", 10, MPFR_RNDN);
        mpfr_init(r1195);
        mpfr_init(r1196);
        mpfr_init(r1197);
        mpfr_init(r1198);
        mpfr_init(r1199);
        mpfr_init(r1200);
}

double f_fm(double a, double b, double c) {
        mpfr_set_d(r1165, b, MPFR_RNDN);
        ;
        mpfr_set_si(r1167, mpfr_cmp(r1165, r1166) < 0, MPFR_RNDN);
        ;
        mpfr_set_d(r1169, c, MPFR_RNDN);
        mpfr_set_d(r1170, a, MPFR_RNDN);
        mpfr_mul(r1171, r1169, r1170, MPFR_RNDN);
        mpfr_mul(r1172, r1168, r1171, MPFR_RNDN);
        mpfr_neg(r1173, r1165, MPFR_RNDN);
        mpfr_mul(r1174, r1165, r1165, MPFR_RNDN);
        mpfr_mul(r1175, r1168, r1169, MPFR_RNDN);
        mpfr_mul(r1176, r1175, r1170, MPFR_RNDN);
        mpfr_sub(r1177, r1174, r1176, MPFR_RNDN);
        mpfr_sqrt(r1178, r1177, MPFR_RNDN);
        mpfr_add(r1179, r1173, r1178, MPFR_RNDN);
        mpfr_div(r1180, r1172, r1179, MPFR_RNDN);
        ;
        ;
        mpfr_mul(r1183, r1182, r1170, MPFR_RNDN);
        mpfr_div(r1184, r1181, r1183, MPFR_RNDN);
        mpfr_mul(r1185, r1180, r1184, MPFR_RNDN);
        ;
        mpfr_set_si(r1187, mpfr_cmp(r1165, r1186) < 0, MPFR_RNDN);
        mpfr_mul(r1188, r1170, r1168, MPFR_RNDN);
        mpfr_mul(r1189, r1188, r1169, MPFR_RNDN);
        mpfr_sub(r1190, r1174, r1189, MPFR_RNDN);
        mpfr_sqrt(r1191, r1190, MPFR_RNDN);
        mpfr_sub(r1192, r1173, r1191, MPFR_RNDN);
        mpfr_mul(r1193, r1192, r1184, MPFR_RNDN);
        ;
        mpfr_div(r1195, r1165, r1170, MPFR_RNDN);
        mpfr_mul(r1196, r1194, r1195, MPFR_RNDN);
        mpfr_div(r1197, r1169, r1165, MPFR_RNDN);
        mpfr_add(r1198, r1196, r1197, MPFR_RNDN);
        if (mpfr_get_si(r1187, MPFR_RNDN)) { mpfr_set(r1199, r1193, MPFR_RNDN); } else { mpfr_set(r1199, r1198, MPFR_RNDN); };
        if (mpfr_get_si(r1167, MPFR_RNDN)) { mpfr_set(r1200, r1185, MPFR_RNDN); } else { mpfr_set(r1200, r1199, MPFR_RNDN); };
        return mpfr_get_d(r1200, MPFR_RNDN);
}

static mpfr_t r1201, r1202, r1203, r1204, r1205, r1206, r1207, r1208, r1209, r1210, r1211, r1212, r1213, r1214, r1215, r1216, r1217, r1218, r1219, r1220, r1221, r1222, r1223, r1224, r1225, r1226, r1227, r1228, r1229, r1230, r1231, r1232, r1233, r1234, r1235, r1236, r1237, r1238, r1239, r1240, r1241, r1242, r1243, r1244, r1245, r1246, r1247;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1201);
        mpfr_init_set_str(r1202, "-5.12178672906159e-80", 10, MPFR_RNDN);
        mpfr_init(r1203);
        mpfr_init_set_str(r1204, "4", 10, MPFR_RNDN);
        mpfr_init_set_str(r1205, "1", 10, MPFR_RNDN);
        mpfr_init(r1206);
        mpfr_init(r1207);
        mpfr_init(r1208);
        mpfr_init(r1209);
        mpfr_init(r1210);
        mpfr_init(r1211);
        mpfr_init(r1212);
        mpfr_init(r1213);
        mpfr_init(r1214);
        mpfr_init(r1215);
        mpfr_init(r1216);
        mpfr_init(r1217);
        mpfr_init(r1218);
        mpfr_init(r1219);
        mpfr_init(r1220);
        mpfr_init(r1221);
        mpfr_init_set_str(r1222, "2", 10, MPFR_RNDN);
        mpfr_init(r1223);
        mpfr_init(r1224);
        mpfr_init(r1225);
        mpfr_init_set_str(r1226, "4.4464930377598826e+102", 10, MPFR_RNDN);
        mpfr_init(r1227);
        mpfr_init(r1228);
        mpfr_init(r1229);
        mpfr_init(r1230);
        mpfr_init(r1231);
        mpfr_init(r1232);
        mpfr_init(r1233);
        mpfr_init(r1234);
        mpfr_init(r1235);
        mpfr_init(r1236);
        mpfr_init(r1237);
        mpfr_init(r1238);
        mpfr_init(r1239);
        mpfr_init_set_str(r1240, "-1", 10, MPFR_RNDN);
        mpfr_init(r1241);
        mpfr_init(r1242);
        mpfr_init(r1243);
        mpfr_init(r1244);
        mpfr_init(r1245);
        mpfr_init(r1246);
        mpfr_init(r1247);
}

double f_dm(double a, double b, double c) {
        mpfr_set_d(r1201, b, MPFR_RNDN);
        ;
        mpfr_set_si(r1203, mpfr_cmp(r1201, r1202) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_div(r1206, r1205, r1205, MPFR_RNDN);
        mpfr_set_d(r1207, a, MPFR_RNDN);
        mpfr_div(r1208, r1205, r1207, MPFR_RNDN);
        mpfr_div(r1209, r1206, r1208, MPFR_RNDN);
        mpfr_set_d(r1210, c, MPFR_RNDN);
        mpfr_div(r1211, r1205, r1210, MPFR_RNDN);
        mpfr_div(r1212, r1209, r1211, MPFR_RNDN);
        mpfr_mul(r1213, r1204, r1212, MPFR_RNDN);
        mpfr_neg(r1214, r1201, MPFR_RNDN);
        mpfr_mul(r1215, r1201, r1201, MPFR_RNDN);
        mpfr_mul(r1216, r1204, r1210, MPFR_RNDN);
        mpfr_mul(r1217, r1216, r1207, MPFR_RNDN);
        mpfr_sub(r1218, r1215, r1217, MPFR_RNDN);
        mpfr_sqrt(r1219, r1218, MPFR_RNDN);
        mpfr_add(r1220, r1214, r1219, MPFR_RNDN);
        mpfr_div(r1221, r1213, r1220, MPFR_RNDN);
        ;
        mpfr_mul(r1223, r1222, r1207, MPFR_RNDN);
        mpfr_div(r1224, r1205, r1223, MPFR_RNDN);
        mpfr_mul(r1225, r1221, r1224, MPFR_RNDN);
        ;
        mpfr_set_si(r1227, mpfr_cmp(r1201, r1226) < 0, MPFR_RNDN);
        mpfr_mul(r1228, r1210, r1204, MPFR_RNDN);
        mpfr_mul(r1229, r1228, r1207, MPFR_RNDN);
        mpfr_sub(r1230, r1215, r1229, MPFR_RNDN);
        mpfr_sqrt(r1231, r1230, MPFR_RNDN);
        mpfr_sqrt(r1232, r1231, MPFR_RNDN);
        mpfr_mul(r1233, r1232, r1232, MPFR_RNDN);
        mpfr_sub(r1234, r1214, r1233, MPFR_RNDN);
        mpfr_div(r1235, r1234, r1223, MPFR_RNDN);
        mpfr_div(r1236, r1205, r1201, MPFR_RNDN);
        mpfr_div(r1237, r1236, r1205, MPFR_RNDN);
        mpfr_div(r1238, r1237, r1211, MPFR_RNDN);
        mpfr_mul(r1239, r1205, r1238, MPFR_RNDN);
        ;
        mpfr_div(r1241, r1205, r1236, MPFR_RNDN);
        mpfr_div(r1242, r1241, r1207, MPFR_RNDN);
        mpfr_div(r1243, r1242, r1205, MPFR_RNDN);
        mpfr_mul(r1244, r1240, r1243, MPFR_RNDN);
        mpfr_add(r1245, r1239, r1244, MPFR_RNDN);
        if (mpfr_get_si(r1227, MPFR_RNDN)) { mpfr_set(r1246, r1235, MPFR_RNDN); } else { mpfr_set(r1246, r1245, MPFR_RNDN); };
        if (mpfr_get_si(r1203, MPFR_RNDN)) { mpfr_set(r1247, r1225, MPFR_RNDN); } else { mpfr_set(r1247, r1246, MPFR_RNDN); };
        return mpfr_get_d(r1247, MPFR_RNDN);
}

