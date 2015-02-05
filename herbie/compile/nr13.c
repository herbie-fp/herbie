#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.7";

double f_if(float x) {
        float r1278 = x;
        float r1279 = exp(r1278);
        float r1280 = 2.0;
        float r1281 = r1279 - r1280;
        float r1282 = -r1278;
        float r1283 = exp(r1282);
        float r1284 = r1281 + r1283;
        return r1284;
}

double f_id(double x) {
        double r1285 = x;
        double r1286 = exp(r1285);
        double r1287 = 2.0;
        double r1288 = r1286 - r1287;
        double r1289 = -r1285;
        double r1290 = exp(r1289);
        double r1291 = r1288 + r1290;
        return r1291;
}


double f_of(float x) {
        float r1292 = 0.002777777777777778;
        float r1293 = x;
        float r1294 = r1293 * r1293;
        float r1295 = r1293 * r1294;
        float r1296 = r1295 * r1295;
        float r1297 = 1.0;
        float r1298 = r1296 * r1297;
        float r1299 = r1292 * r1298;
        float r1300 = 0.08333333333333333;
        float r1301 = r1294 * r1294;
        float r1302 = r1301 * r1297;
        float r1303 = r1300 * r1302;
        float r1304 = r1293 * r1293;
        float r1305 = r1304 * r1297;
        float r1306 = r1297 * r1305;
        float r1307 = r1303 + r1306;
        float r1308 = r1299 + r1307;
        return r1308;
}

double f_od(double x) {
        double r1309 = 0.002777777777777778;
        double r1310 = x;
        double r1311 = r1310 * r1310;
        double r1312 = r1310 * r1311;
        double r1313 = r1312 * r1312;
        double r1314 = 1.0;
        double r1315 = r1313 * r1314;
        double r1316 = r1309 * r1315;
        double r1317 = 0.08333333333333333;
        double r1318 = r1311 * r1311;
        double r1319 = r1318 * r1314;
        double r1320 = r1317 * r1319;
        double r1321 = r1310 * r1310;
        double r1322 = r1321 * r1314;
        double r1323 = r1314 * r1322;
        double r1324 = r1320 + r1323;
        double r1325 = r1316 + r1324;
        return r1325;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1326, r1327, r1328, r1329, r1330, r1331, r1332;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1326);
        mpfr_init(r1327);
        mpfr_init_set_str(r1328, "2", 10, MPFR_RNDN);
        mpfr_init(r1329);
        mpfr_init(r1330);
        mpfr_init(r1331);
        mpfr_init(r1332);
}

double f_im(double x) {
        mpfr_set_d(r1326, x, MPFR_RNDN);
        mpfr_exp(r1327, r1326, MPFR_RNDN);
        ;
        mpfr_sub(r1329, r1327, r1328, MPFR_RNDN);
        mpfr_neg(r1330, r1326, MPFR_RNDN);
        mpfr_exp(r1331, r1330, MPFR_RNDN);
        mpfr_add(r1332, r1329, r1331, MPFR_RNDN);
        return mpfr_get_d(r1332, MPFR_RNDN);
}

static mpfr_t r1333, r1334, r1335, r1336, r1337, r1338, r1339, r1340, r1341, r1342, r1343, r1344, r1345, r1346, r1347, r1348, r1349;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1333, "1/360", 10, MPFR_RNDN);
        mpfr_init(r1334);
        mpfr_init(r1335);
        mpfr_init(r1336);
        mpfr_init(r1337);
        mpfr_init_set_str(r1338, "1", 10, MPFR_RNDN);
        mpfr_init(r1339);
        mpfr_init(r1340);
        mpfr_init_set_str(r1341, "1/12", 10, MPFR_RNDN);
        mpfr_init(r1342);
        mpfr_init(r1343);
        mpfr_init(r1344);
        mpfr_init(r1345);
        mpfr_init(r1346);
        mpfr_init(r1347);
        mpfr_init(r1348);
        mpfr_init(r1349);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r1334, x, MPFR_RNDN);
        mpfr_mul(r1335, r1334, r1334, MPFR_RNDN);
        mpfr_mul(r1336, r1334, r1335, MPFR_RNDN);
        mpfr_mul(r1337, r1336, r1336, MPFR_RNDN);
        ;
        mpfr_mul(r1339, r1337, r1338, MPFR_RNDN);
        mpfr_mul(r1340, r1333, r1339, MPFR_RNDN);
        ;
        mpfr_mul(r1342, r1335, r1335, MPFR_RNDN);
        mpfr_mul(r1343, r1342, r1338, MPFR_RNDN);
        mpfr_mul(r1344, r1341, r1343, MPFR_RNDN);
        mpfr_mul(r1345, r1334, r1334, MPFR_RNDN);
        mpfr_mul(r1346, r1345, r1338, MPFR_RNDN);
        mpfr_mul(r1347, r1338, r1346, MPFR_RNDN);
        mpfr_add(r1348, r1344, r1347, MPFR_RNDN);
        mpfr_add(r1349, r1340, r1348, MPFR_RNDN);
        return mpfr_get_d(r1349, MPFR_RNDN);
}

static mpfr_t r1350, r1351, r1352, r1353, r1354, r1355, r1356, r1357, r1358, r1359, r1360, r1361, r1362, r1363, r1364, r1365, r1366;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1350, "1/360", 10, MPFR_RNDN);
        mpfr_init(r1351);
        mpfr_init(r1352);
        mpfr_init(r1353);
        mpfr_init(r1354);
        mpfr_init_set_str(r1355, "1", 10, MPFR_RNDN);
        mpfr_init(r1356);
        mpfr_init(r1357);
        mpfr_init_set_str(r1358, "1/12", 10, MPFR_RNDN);
        mpfr_init(r1359);
        mpfr_init(r1360);
        mpfr_init(r1361);
        mpfr_init(r1362);
        mpfr_init(r1363);
        mpfr_init(r1364);
        mpfr_init(r1365);
        mpfr_init(r1366);
}

double f_dm(double x) {
        ;
        mpfr_set_d(r1351, x, MPFR_RNDN);
        mpfr_mul(r1352, r1351, r1351, MPFR_RNDN);
        mpfr_mul(r1353, r1351, r1352, MPFR_RNDN);
        mpfr_mul(r1354, r1353, r1353, MPFR_RNDN);
        ;
        mpfr_mul(r1356, r1354, r1355, MPFR_RNDN);
        mpfr_mul(r1357, r1350, r1356, MPFR_RNDN);
        ;
        mpfr_mul(r1359, r1352, r1352, MPFR_RNDN);
        mpfr_mul(r1360, r1359, r1355, MPFR_RNDN);
        mpfr_mul(r1361, r1358, r1360, MPFR_RNDN);
        mpfr_mul(r1362, r1351, r1351, MPFR_RNDN);
        mpfr_mul(r1363, r1362, r1355, MPFR_RNDN);
        mpfr_mul(r1364, r1355, r1363, MPFR_RNDN);
        mpfr_add(r1365, r1361, r1364, MPFR_RNDN);
        mpfr_add(r1366, r1357, r1365, MPFR_RNDN);
        return mpfr_get_d(r1366, MPFR_RNDN);
}

