#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE p42";

double f_if(float a, float b, float c) {
        float r1248 = b;
        float r1249 = -r1248;
        float r1250 = r1248 * r1248;
        float r1251 = 4.0;
        float r1252 = a;
        float r1253 = c;
        float r1254 = r1252 * r1253;
        float r1255 = r1251 * r1254;
        float r1256 = r1250 - r1255;
        float r1257 = sqrt(r1256);
        float r1258 = r1249 + r1257;
        float r1259 = 2.0;
        float r1260 = r1259 * r1252;
        float r1261 = r1258 / r1260;
        return r1261;
}

double f_id(double a, double b, double c) {
        double r1262 = b;
        double r1263 = -r1262;
        double r1264 = r1262 * r1262;
        double r1265 = 4.0;
        double r1266 = a;
        double r1267 = c;
        double r1268 = r1266 * r1267;
        double r1269 = r1265 * r1268;
        double r1270 = r1264 - r1269;
        double r1271 = sqrt(r1270);
        double r1272 = r1263 + r1271;
        double r1273 = 2.0;
        double r1274 = r1273 * r1266;
        double r1275 = r1272 / r1274;
        return r1275;
}


double f_of(float a, float b, float c) {
        float r1276 = b;
        float r1277 = 1.58166383636138e-12;
        bool r1278 = r1276 < r1277;
        float r1279 = -r1276;
        float r1280 = r1276 * r1276;
        float r1281 = a;
        float r1282 = 4.0;
        float r1283 = r1281 * r1282;
        float r1284 = c;
        float r1285 = r1283 * r1284;
        float r1286 = r1280 - r1285;
        float r1287 = sqrt(r1286);
        float r1288 = r1279 + r1287;
        float r1289 = 1.0;
        float r1290 = 2.0;
        float r1291 = r1290 * r1281;
        float r1292 = r1289 / r1291;
        float r1293 = r1288 * r1292;
        float r1294 = -1.0;
        float r1295 = r1284 / r1276;
        float r1296 = r1294 * r1295;
        float r1297 = r1278 ? r1293 : r1296;
        return r1297;
}

double f_od(double a, double b, double c) {
        double r1298 = b;
        double r1299 = 2.0569927817214236e-282;
        bool r1300 = r1298 < r1299;
        double r1301 = -r1298;
        double r1302 = r1298 * r1298;
        double r1303 = a;
        double r1304 = 4.0;
        double r1305 = r1303 * r1304;
        double r1306 = c;
        double r1307 = r1305 * r1306;
        double r1308 = r1302 - r1307;
        double r1309 = sqrt(r1308);
        double r1310 = r1301 + r1309;
        double r1311 = 2.0;
        double r1312 = r1311 * r1303;
        double r1313 = r1310 / r1312;
        double r1314 = 4.4464930377598826e+102;
        bool r1315 = r1298 < r1314;
        double r1316 = 1.0;
        double r1317 = r1316 * r1316;
        double r1318 = r1303 * r1317;
        double r1319 = r1306 * r1318;
        double r1320 = r1304 * r1319;
        double r1321 = r1304 * r1306;
        double r1322 = r1321 * r1303;
        double r1323 = r1302 - r1322;
        double r1324 = sqrt(r1323);
        double r1325 = r1301 - r1324;
        double r1326 = r1320 / r1325;
        double r1327 = r1316 / r1312;
        double r1328 = r1326 * r1327;
        double r1329 = -1.0;
        double r1330 = r1316 / r1298;
        double r1331 = r1330 / r1316;
        double r1332 = r1316 / r1306;
        double r1333 = r1331 / r1332;
        double r1334 = r1329 * r1333;
        double r1335 = r1315 ? r1328 : r1334;
        double r1336 = r1300 ? r1313 : r1335;
        return r1336;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1337, r1338, r1339, r1340, r1341, r1342, r1343, r1344, r1345, r1346, r1347, r1348, r1349, r1350;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1337);
        mpfr_init(r1338);
        mpfr_init(r1339);
        mpfr_init_set_str(r1340, "4", 10, MPFR_RNDN);
        mpfr_init(r1341);
        mpfr_init(r1342);
        mpfr_init(r1343);
        mpfr_init(r1344);
        mpfr_init(r1345);
        mpfr_init(r1346);
        mpfr_init(r1347);
        mpfr_init_set_str(r1348, "2", 10, MPFR_RNDN);
        mpfr_init(r1349);
        mpfr_init(r1350);
}

double f_im(double a, double b, double c) {
        mpfr_set_d(r1337, b, MPFR_RNDN);
        mpfr_neg(r1338, r1337, MPFR_RNDN);
        mpfr_mul(r1339, r1337, r1337, MPFR_RNDN);
        ;
        mpfr_set_d(r1341, a, MPFR_RNDN);
        mpfr_set_d(r1342, c, MPFR_RNDN);
        mpfr_mul(r1343, r1341, r1342, MPFR_RNDN);
        mpfr_mul(r1344, r1340, r1343, MPFR_RNDN);
        mpfr_sub(r1345, r1339, r1344, MPFR_RNDN);
        mpfr_sqrt(r1346, r1345, MPFR_RNDN);
        mpfr_add(r1347, r1338, r1346, MPFR_RNDN);
        ;
        mpfr_mul(r1349, r1348, r1341, MPFR_RNDN);
        mpfr_div(r1350, r1347, r1349, MPFR_RNDN);
        return mpfr_get_d(r1350, MPFR_RNDN);
}

static mpfr_t r1351, r1352, r1353, r1354, r1355, r1356, r1357, r1358, r1359, r1360, r1361, r1362, r1363, r1364, r1365, r1366, r1367, r1368, r1369, r1370, r1371, r1372;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1351);
        mpfr_init_set_str(r1352, "1.58166383636138e-12", 10, MPFR_RNDN);
        mpfr_init(r1353);
        mpfr_init(r1354);
        mpfr_init(r1355);
        mpfr_init(r1356);
        mpfr_init_set_str(r1357, "4", 10, MPFR_RNDN);
        mpfr_init(r1358);
        mpfr_init(r1359);
        mpfr_init(r1360);
        mpfr_init(r1361);
        mpfr_init(r1362);
        mpfr_init(r1363);
        mpfr_init_set_str(r1364, "1", 10, MPFR_RNDN);
        mpfr_init_set_str(r1365, "2", 10, MPFR_RNDN);
        mpfr_init(r1366);
        mpfr_init(r1367);
        mpfr_init(r1368);
        mpfr_init_set_str(r1369, "-1", 10, MPFR_RNDN);
        mpfr_init(r1370);
        mpfr_init(r1371);
        mpfr_init(r1372);
}

double f_fm(double a, double b, double c) {
        mpfr_set_d(r1351, b, MPFR_RNDN);
        ;
        mpfr_set_si(r1353, mpfr_cmp(r1351, r1352) < 0, MPFR_RNDN);
        mpfr_neg(r1354, r1351, MPFR_RNDN);
        mpfr_mul(r1355, r1351, r1351, MPFR_RNDN);
        mpfr_set_d(r1356, a, MPFR_RNDN);
        ;
        mpfr_mul(r1358, r1356, r1357, MPFR_RNDN);
        mpfr_set_d(r1359, c, MPFR_RNDN);
        mpfr_mul(r1360, r1358, r1359, MPFR_RNDN);
        mpfr_sub(r1361, r1355, r1360, MPFR_RNDN);
        mpfr_sqrt(r1362, r1361, MPFR_RNDN);
        mpfr_add(r1363, r1354, r1362, MPFR_RNDN);
        ;
        ;
        mpfr_mul(r1366, r1365, r1356, MPFR_RNDN);
        mpfr_div(r1367, r1364, r1366, MPFR_RNDN);
        mpfr_mul(r1368, r1363, r1367, MPFR_RNDN);
        ;
        mpfr_div(r1370, r1359, r1351, MPFR_RNDN);
        mpfr_mul(r1371, r1369, r1370, MPFR_RNDN);
        if (mpfr_get_si(r1353, MPFR_RNDN)) { mpfr_set(r1372, r1368, MPFR_RNDN); } else { mpfr_set(r1372, r1371, MPFR_RNDN); };
        return mpfr_get_d(r1372, MPFR_RNDN);
}

static mpfr_t r1373, r1374, r1375, r1376, r1377, r1378, r1379, r1380, r1381, r1382, r1383, r1384, r1385, r1386, r1387, r1388, r1389, r1390, r1391, r1392, r1393, r1394, r1395, r1396, r1397, r1398, r1399, r1400, r1401, r1402, r1403, r1404, r1405, r1406, r1407, r1408, r1409, r1410, r1411;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1373);
        mpfr_init_set_str(r1374, "2.0569927817214236e-282", 10, MPFR_RNDN);
        mpfr_init(r1375);
        mpfr_init(r1376);
        mpfr_init(r1377);
        mpfr_init(r1378);
        mpfr_init_set_str(r1379, "4", 10, MPFR_RNDN);
        mpfr_init(r1380);
        mpfr_init(r1381);
        mpfr_init(r1382);
        mpfr_init(r1383);
        mpfr_init(r1384);
        mpfr_init(r1385);
        mpfr_init_set_str(r1386, "2", 10, MPFR_RNDN);
        mpfr_init(r1387);
        mpfr_init(r1388);
        mpfr_init_set_str(r1389, "4.4464930377598826e+102", 10, MPFR_RNDN);
        mpfr_init(r1390);
        mpfr_init_set_str(r1391, "1", 10, MPFR_RNDN);
        mpfr_init(r1392);
        mpfr_init(r1393);
        mpfr_init(r1394);
        mpfr_init(r1395);
        mpfr_init(r1396);
        mpfr_init(r1397);
        mpfr_init(r1398);
        mpfr_init(r1399);
        mpfr_init(r1400);
        mpfr_init(r1401);
        mpfr_init(r1402);
        mpfr_init(r1403);
        mpfr_init_set_str(r1404, "-1", 10, MPFR_RNDN);
        mpfr_init(r1405);
        mpfr_init(r1406);
        mpfr_init(r1407);
        mpfr_init(r1408);
        mpfr_init(r1409);
        mpfr_init(r1410);
        mpfr_init(r1411);
}

double f_dm(double a, double b, double c) {
        mpfr_set_d(r1373, b, MPFR_RNDN);
        ;
        mpfr_set_si(r1375, mpfr_cmp(r1373, r1374) < 0, MPFR_RNDN);
        mpfr_neg(r1376, r1373, MPFR_RNDN);
        mpfr_mul(r1377, r1373, r1373, MPFR_RNDN);
        mpfr_set_d(r1378, a, MPFR_RNDN);
        ;
        mpfr_mul(r1380, r1378, r1379, MPFR_RNDN);
        mpfr_set_d(r1381, c, MPFR_RNDN);
        mpfr_mul(r1382, r1380, r1381, MPFR_RNDN);
        mpfr_sub(r1383, r1377, r1382, MPFR_RNDN);
        mpfr_sqrt(r1384, r1383, MPFR_RNDN);
        mpfr_add(r1385, r1376, r1384, MPFR_RNDN);
        ;
        mpfr_mul(r1387, r1386, r1378, MPFR_RNDN);
        mpfr_div(r1388, r1385, r1387, MPFR_RNDN);
        ;
        mpfr_set_si(r1390, mpfr_cmp(r1373, r1389) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r1392, r1391, r1391, MPFR_RNDN);
        mpfr_mul(r1393, r1378, r1392, MPFR_RNDN);
        mpfr_mul(r1394, r1381, r1393, MPFR_RNDN);
        mpfr_mul(r1395, r1379, r1394, MPFR_RNDN);
        mpfr_mul(r1396, r1379, r1381, MPFR_RNDN);
        mpfr_mul(r1397, r1396, r1378, MPFR_RNDN);
        mpfr_sub(r1398, r1377, r1397, MPFR_RNDN);
        mpfr_sqrt(r1399, r1398, MPFR_RNDN);
        mpfr_sub(r1400, r1376, r1399, MPFR_RNDN);
        mpfr_div(r1401, r1395, r1400, MPFR_RNDN);
        mpfr_div(r1402, r1391, r1387, MPFR_RNDN);
        mpfr_mul(r1403, r1401, r1402, MPFR_RNDN);
        ;
        mpfr_div(r1405, r1391, r1373, MPFR_RNDN);
        mpfr_div(r1406, r1405, r1391, MPFR_RNDN);
        mpfr_div(r1407, r1391, r1381, MPFR_RNDN);
        mpfr_div(r1408, r1406, r1407, MPFR_RNDN);
        mpfr_mul(r1409, r1404, r1408, MPFR_RNDN);
        if (mpfr_get_si(r1390, MPFR_RNDN)) { mpfr_set(r1410, r1403, MPFR_RNDN); } else { mpfr_set(r1410, r1409, MPFR_RNDN); };
        if (mpfr_get_si(r1375, MPFR_RNDN)) { mpfr_set(r1411, r1388, MPFR_RNDN); } else { mpfr_set(r1411, r1410, MPFR_RNDN); };
        return mpfr_get_d(r1411, MPFR_RNDN);
}

