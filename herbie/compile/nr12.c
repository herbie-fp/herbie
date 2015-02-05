#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.3";

double f_if(float x) {
        float r1188 = 1.0;
        float r1189 = x;
        float r1190 = r1189 + r1188;
        float r1191 = r1188 / r1190;
        float r1192 = 2.0;
        float r1193 = r1192 / r1189;
        float r1194 = r1191 - r1193;
        float r1195 = r1189 - r1188;
        float r1196 = r1188 / r1195;
        float r1197 = r1194 + r1196;
        return r1197;
}

double f_id(double x) {
        double r1198 = 1.0;
        double r1199 = x;
        double r1200 = r1199 + r1198;
        double r1201 = r1198 / r1200;
        double r1202 = 2.0;
        double r1203 = r1202 / r1199;
        double r1204 = r1201 - r1203;
        double r1205 = r1199 - r1198;
        double r1206 = r1198 / r1205;
        double r1207 = r1204 + r1206;
        return r1207;
}


double f_of(float x) {
        float r1208 = 1.0;
        float r1209 = x;
        float r1210 = r1209 - r1208;
        float r1211 = r1209 * r1210;
        float r1212 = r1208 + r1209;
        float r1213 = r1211 * r1212;
        float r1214 = r1210 + r1209;
        float r1215 = 2.0;
        float r1216 = r1215 * r1210;
        float r1217 = r1214 - r1216;
        float r1218 = r1217 * r1209;
        float r1219 = r1209 - r1216;
        float r1220 = r1218 + r1219;
        float r1221 = r1213 / r1220;
        float r1222 = r1208 / r1221;
        return r1222;
}

double f_od(double x) {
        double r1223 = 1.0;
        double r1224 = x;
        double r1225 = r1224 - r1223;
        double r1226 = r1224 * r1225;
        double r1227 = r1223 + r1224;
        double r1228 = r1226 * r1227;
        double r1229 = r1225 + r1224;
        double r1230 = 2.0;
        double r1231 = r1230 * r1225;
        double r1232 = r1229 - r1231;
        double r1233 = r1232 * r1224;
        double r1234 = r1224 - r1231;
        double r1235 = r1233 + r1234;
        double r1236 = r1228 / r1235;
        double r1237 = r1223 / r1236;
        return r1237;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1238, r1239, r1240, r1241, r1242, r1243, r1244, r1245, r1246, r1247;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1238, "1", 10, MPFR_RNDN);
        mpfr_init(r1239);
        mpfr_init(r1240);
        mpfr_init(r1241);
        mpfr_init_set_str(r1242, "2", 10, MPFR_RNDN);
        mpfr_init(r1243);
        mpfr_init(r1244);
        mpfr_init(r1245);
        mpfr_init(r1246);
        mpfr_init(r1247);
}

double f_im(double x) {
        ;
        mpfr_set_d(r1239, x, MPFR_RNDN);
        mpfr_add(r1240, r1239, r1238, MPFR_RNDN);
        mpfr_div(r1241, r1238, r1240, MPFR_RNDN);
        ;
        mpfr_div(r1243, r1242, r1239, MPFR_RNDN);
        mpfr_sub(r1244, r1241, r1243, MPFR_RNDN);
        mpfr_sub(r1245, r1239, r1238, MPFR_RNDN);
        mpfr_div(r1246, r1238, r1245, MPFR_RNDN);
        mpfr_add(r1247, r1244, r1246, MPFR_RNDN);
        return mpfr_get_d(r1247, MPFR_RNDN);
}

static mpfr_t r1248, r1249, r1250, r1251, r1252, r1253, r1254, r1255, r1256, r1257, r1258, r1259, r1260, r1261, r1262;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1248, "1", 10, MPFR_RNDN);
        mpfr_init(r1249);
        mpfr_init(r1250);
        mpfr_init(r1251);
        mpfr_init(r1252);
        mpfr_init(r1253);
        mpfr_init(r1254);
        mpfr_init_set_str(r1255, "2", 10, MPFR_RNDN);
        mpfr_init(r1256);
        mpfr_init(r1257);
        mpfr_init(r1258);
        mpfr_init(r1259);
        mpfr_init(r1260);
        mpfr_init(r1261);
        mpfr_init(r1262);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r1249, x, MPFR_RNDN);
        mpfr_sub(r1250, r1249, r1248, MPFR_RNDN);
        mpfr_mul(r1251, r1249, r1250, MPFR_RNDN);
        mpfr_add(r1252, r1248, r1249, MPFR_RNDN);
        mpfr_mul(r1253, r1251, r1252, MPFR_RNDN);
        mpfr_add(r1254, r1250, r1249, MPFR_RNDN);
        ;
        mpfr_mul(r1256, r1255, r1250, MPFR_RNDN);
        mpfr_sub(r1257, r1254, r1256, MPFR_RNDN);
        mpfr_mul(r1258, r1257, r1249, MPFR_RNDN);
        mpfr_sub(r1259, r1249, r1256, MPFR_RNDN);
        mpfr_add(r1260, r1258, r1259, MPFR_RNDN);
        mpfr_div(r1261, r1253, r1260, MPFR_RNDN);
        mpfr_div(r1262, r1248, r1261, MPFR_RNDN);
        return mpfr_get_d(r1262, MPFR_RNDN);
}

static mpfr_t r1263, r1264, r1265, r1266, r1267, r1268, r1269, r1270, r1271, r1272, r1273, r1274, r1275, r1276, r1277;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1263, "1", 10, MPFR_RNDN);
        mpfr_init(r1264);
        mpfr_init(r1265);
        mpfr_init(r1266);
        mpfr_init(r1267);
        mpfr_init(r1268);
        mpfr_init(r1269);
        mpfr_init_set_str(r1270, "2", 10, MPFR_RNDN);
        mpfr_init(r1271);
        mpfr_init(r1272);
        mpfr_init(r1273);
        mpfr_init(r1274);
        mpfr_init(r1275);
        mpfr_init(r1276);
        mpfr_init(r1277);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r1264, x, MPFR_RNDN);
        mpfr_sub(r1265, r1264, r1263, MPFR_RNDN);
        mpfr_mul(r1266, r1264, r1265, MPFR_RNDN);
        mpfr_add(r1267, r1263, r1264, MPFR_RNDN);
        mpfr_mul(r1268, r1266, r1267, MPFR_RNDN);
        mpfr_add(r1269, r1265, r1264, MPFR_RNDN);
        ;
        mpfr_mul(r1271, r1270, r1265, MPFR_RNDN);
        mpfr_sub(r1272, r1269, r1271, MPFR_RNDN);
        mpfr_mul(r1273, r1272, r1264, MPFR_RNDN);
        mpfr_sub(r1274, r1264, r1271, MPFR_RNDN);
        mpfr_add(r1275, r1273, r1274, MPFR_RNDN);
        mpfr_div(r1276, r1268, r1275, MPFR_RNDN);
        mpfr_div(r1277, r1263, r1276, MPFR_RNDN);
        return mpfr_get_d(r1277, MPFR_RNDN);
}

