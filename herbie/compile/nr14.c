#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.1";

double f_if(float x) {
        float r1367 = 1.0;
        float r1368 = x;
        float r1369 = cos(r1368);
        float r1370 = r1367 - r1369;
        float r1371 = r1368 * r1368;
        float r1372 = r1370 / r1371;
        return r1372;
}

double f_id(double x) {
        double r1373 = 1.0;
        double r1374 = x;
        double r1375 = cos(r1374);
        double r1376 = r1373 - r1375;
        double r1377 = r1374 * r1374;
        double r1378 = r1376 / r1377;
        return r1378;
}


double f_of(float x) {
        float r1379 = x;
        float r1380 = sin(r1379);
        float r1381 = 1.0;
        float r1382 = r1380 / r1381;
        float r1383 = r1382 / r1379;
        float r1384 = cos(r1379);
        float r1385 = r1384 + r1381;
        float r1386 = r1380 / r1385;
        float r1387 = r1386 / r1379;
        float r1388 = r1383 * r1387;
        return r1388;
}

double f_od(double x) {
        double r1389 = x;
        double r1390 = sin(r1389);
        double r1391 = 1.0;
        double r1392 = r1390 / r1391;
        double r1393 = r1392 / r1389;
        double r1394 = cos(r1389);
        double r1395 = r1394 + r1391;
        double r1396 = r1390 / r1395;
        double r1397 = r1396 / r1389;
        double r1398 = r1393 * r1397;
        return r1398;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1399, r1400, r1401, r1402, r1403, r1404;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1399, "1", 10, MPFR_RNDN);
        mpfr_init(r1400);
        mpfr_init(r1401);
        mpfr_init(r1402);
        mpfr_init(r1403);
        mpfr_init(r1404);
}

double f_im(double x) {
        ;
        mpfr_set_d(r1400, x, MPFR_RNDN);
        mpfr_cos(r1401, r1400, MPFR_RNDN);
        mpfr_sub(r1402, r1399, r1401, MPFR_RNDN);
        mpfr_mul(r1403, r1400, r1400, MPFR_RNDN);
        mpfr_div(r1404, r1402, r1403, MPFR_RNDN);
        return mpfr_get_d(r1404, MPFR_RNDN);
}

static mpfr_t r1405, r1406, r1407, r1408, r1409, r1410, r1411, r1412, r1413, r1414;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1405);
        mpfr_init(r1406);
        mpfr_init_set_str(r1407, "1", 10, MPFR_RNDN);
        mpfr_init(r1408);
        mpfr_init(r1409);
        mpfr_init(r1410);
        mpfr_init(r1411);
        mpfr_init(r1412);
        mpfr_init(r1413);
        mpfr_init(r1414);
}

double f_fm(double x) {
        mpfr_set_d(r1405, x, MPFR_RNDN);
        mpfr_sin(r1406, r1405, MPFR_RNDN);
        ;
        mpfr_div(r1408, r1406, r1407, MPFR_RNDN);
        mpfr_div(r1409, r1408, r1405, MPFR_RNDN);
        mpfr_cos(r1410, r1405, MPFR_RNDN);
        mpfr_add(r1411, r1410, r1407, MPFR_RNDN);
        mpfr_div(r1412, r1406, r1411, MPFR_RNDN);
        mpfr_div(r1413, r1412, r1405, MPFR_RNDN);
        mpfr_mul(r1414, r1409, r1413, MPFR_RNDN);
        return mpfr_get_d(r1414, MPFR_RNDN);
}

static mpfr_t r1415, r1416, r1417, r1418, r1419, r1420, r1421, r1422, r1423, r1424;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1415);
        mpfr_init(r1416);
        mpfr_init_set_str(r1417, "1", 10, MPFR_RNDN);
        mpfr_init(r1418);
        mpfr_init(r1419);
        mpfr_init(r1420);
        mpfr_init(r1421);
        mpfr_init(r1422);
        mpfr_init(r1423);
        mpfr_init(r1424);
}

double f_dm(double x) {
        mpfr_set_d(r1415, x, MPFR_RNDN);
        mpfr_sin(r1416, r1415, MPFR_RNDN);
        ;
        mpfr_div(r1418, r1416, r1417, MPFR_RNDN);
        mpfr_div(r1419, r1418, r1415, MPFR_RNDN);
        mpfr_cos(r1420, r1415, MPFR_RNDN);
        mpfr_add(r1421, r1420, r1417, MPFR_RNDN);
        mpfr_div(r1422, r1416, r1421, MPFR_RNDN);
        mpfr_div(r1423, r1422, r1415, MPFR_RNDN);
        mpfr_mul(r1424, r1419, r1423, MPFR_RNDN);
        return mpfr_get_d(r1424, MPFR_RNDN);
}

