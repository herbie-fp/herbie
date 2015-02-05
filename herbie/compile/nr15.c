#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.2";

double f_if(float a, float b, float eps) {
        float r1425 = eps;
        float r1426 = a;
        float r1427 = b;
        float r1428 = r1426 + r1427;
        float r1429 = r1428 * r1425;
        float r1430 = exp(r1429);
        float r1431 = 1.0;
        float r1432 = r1430 - r1431;
        float r1433 = r1425 * r1432;
        float r1434 = r1426 * r1425;
        float r1435 = exp(r1434);
        float r1436 = r1435 - r1431;
        float r1437 = r1427 * r1425;
        float r1438 = exp(r1437);
        float r1439 = r1438 - r1431;
        float r1440 = r1436 * r1439;
        float r1441 = r1433 / r1440;
        return r1441;
}

double f_id(double a, double b, double eps) {
        double r1442 = eps;
        double r1443 = a;
        double r1444 = b;
        double r1445 = r1443 + r1444;
        double r1446 = r1445 * r1442;
        double r1447 = exp(r1446);
        double r1448 = 1.0;
        double r1449 = r1447 - r1448;
        double r1450 = r1442 * r1449;
        double r1451 = r1443 * r1442;
        double r1452 = exp(r1451);
        double r1453 = r1452 - r1448;
        double r1454 = r1444 * r1442;
        double r1455 = exp(r1454);
        double r1456 = r1455 - r1448;
        double r1457 = r1453 * r1456;
        double r1458 = r1450 / r1457;
        return r1458;
}


double f_of(float a, float b, float eps) {
        float r1459 = eps;
        float r1460 = 1.0;
        float r1461 = a;
        float r1462 = r1459 * r1461;
        float r1463 = exp(r1462);
        float r1464 = sqrt(r1463);
        float r1465 = r1464 - r1460;
        float r1466 = r1460 + r1464;
        float r1467 = r1465 * r1466;
        float r1468 = r1460 / r1467;
        float r1469 = r1459 * r1468;
        float r1470 = b;
        float r1471 = r1461 + r1470;
        float r1472 = r1459 * r1471;
        float r1473 = exp(r1472);
        float r1474 = r1473 - r1460;
        float r1475 = r1459 * r1470;
        float r1476 = exp(r1475);
        float r1477 = r1476 - r1460;
        float r1478 = r1474 / r1477;
        float r1479 = r1469 * r1478;
        return r1479;
}

double f_od(double a, double b, double eps) {
        double r1480 = eps;
        double r1481 = 1.0;
        double r1482 = a;
        double r1483 = r1480 * r1482;
        double r1484 = exp(r1483);
        double r1485 = sqrt(r1484);
        double r1486 = r1485 - r1481;
        double r1487 = r1481 + r1485;
        double r1488 = r1486 * r1487;
        double r1489 = r1481 / r1488;
        double r1490 = r1480 * r1489;
        double r1491 = b;
        double r1492 = r1482 + r1491;
        double r1493 = r1480 * r1492;
        double r1494 = exp(r1493);
        double r1495 = r1494 - r1481;
        double r1496 = r1480 * r1491;
        double r1497 = exp(r1496);
        double r1498 = r1497 - r1481;
        double r1499 = r1495 / r1498;
        double r1500 = r1490 * r1499;
        return r1500;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1501, r1502, r1503, r1504, r1505, r1506, r1507, r1508, r1509, r1510, r1511, r1512, r1513, r1514, r1515, r1516, r1517;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1501);
        mpfr_init(r1502);
        mpfr_init(r1503);
        mpfr_init(r1504);
        mpfr_init(r1505);
        mpfr_init(r1506);
        mpfr_init_set_str(r1507, "1", 10, MPFR_RNDN);
        mpfr_init(r1508);
        mpfr_init(r1509);
        mpfr_init(r1510);
        mpfr_init(r1511);
        mpfr_init(r1512);
        mpfr_init(r1513);
        mpfr_init(r1514);
        mpfr_init(r1515);
        mpfr_init(r1516);
        mpfr_init(r1517);
}

double f_im(double a, double b, double eps) {
        mpfr_set_d(r1501, eps, MPFR_RNDN);
        mpfr_set_d(r1502, a, MPFR_RNDN);
        mpfr_set_d(r1503, b, MPFR_RNDN);
        mpfr_add(r1504, r1502, r1503, MPFR_RNDN);
        mpfr_mul(r1505, r1504, r1501, MPFR_RNDN);
        mpfr_exp(r1506, r1505, MPFR_RNDN);
        ;
        mpfr_sub(r1508, r1506, r1507, MPFR_RNDN);
        mpfr_mul(r1509, r1501, r1508, MPFR_RNDN);
        mpfr_mul(r1510, r1502, r1501, MPFR_RNDN);
        mpfr_exp(r1511, r1510, MPFR_RNDN);
        mpfr_sub(r1512, r1511, r1507, MPFR_RNDN);
        mpfr_mul(r1513, r1503, r1501, MPFR_RNDN);
        mpfr_exp(r1514, r1513, MPFR_RNDN);
        mpfr_sub(r1515, r1514, r1507, MPFR_RNDN);
        mpfr_mul(r1516, r1512, r1515, MPFR_RNDN);
        mpfr_div(r1517, r1509, r1516, MPFR_RNDN);
        return mpfr_get_d(r1517, MPFR_RNDN);
}

static mpfr_t r1518, r1519, r1520, r1521, r1522, r1523, r1524, r1525, r1526, r1527, r1528, r1529, r1530, r1531, r1532, r1533, r1534, r1535, r1536, r1537, r1538;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1518);
        mpfr_init_set_str(r1519, "1", 10, MPFR_RNDN);
        mpfr_init(r1520);
        mpfr_init(r1521);
        mpfr_init(r1522);
        mpfr_init(r1523);
        mpfr_init(r1524);
        mpfr_init(r1525);
        mpfr_init(r1526);
        mpfr_init(r1527);
        mpfr_init(r1528);
        mpfr_init(r1529);
        mpfr_init(r1530);
        mpfr_init(r1531);
        mpfr_init(r1532);
        mpfr_init(r1533);
        mpfr_init(r1534);
        mpfr_init(r1535);
        mpfr_init(r1536);
        mpfr_init(r1537);
        mpfr_init(r1538);
}

double f_fm(double a, double b, double eps) {
        mpfr_set_d(r1518, eps, MPFR_RNDN);
        ;
        mpfr_set_d(r1520, a, MPFR_RNDN);
        mpfr_mul(r1521, r1518, r1520, MPFR_RNDN);
        mpfr_exp(r1522, r1521, MPFR_RNDN);
        mpfr_sqrt(r1523, r1522, MPFR_RNDN);
        mpfr_sub(r1524, r1523, r1519, MPFR_RNDN);
        mpfr_add(r1525, r1519, r1523, MPFR_RNDN);
        mpfr_mul(r1526, r1524, r1525, MPFR_RNDN);
        mpfr_div(r1527, r1519, r1526, MPFR_RNDN);
        mpfr_mul(r1528, r1518, r1527, MPFR_RNDN);
        mpfr_set_d(r1529, b, MPFR_RNDN);
        mpfr_add(r1530, r1520, r1529, MPFR_RNDN);
        mpfr_mul(r1531, r1518, r1530, MPFR_RNDN);
        mpfr_exp(r1532, r1531, MPFR_RNDN);
        mpfr_sub(r1533, r1532, r1519, MPFR_RNDN);
        mpfr_mul(r1534, r1518, r1529, MPFR_RNDN);
        mpfr_exp(r1535, r1534, MPFR_RNDN);
        mpfr_sub(r1536, r1535, r1519, MPFR_RNDN);
        mpfr_div(r1537, r1533, r1536, MPFR_RNDN);
        mpfr_mul(r1538, r1528, r1537, MPFR_RNDN);
        return mpfr_get_d(r1538, MPFR_RNDN);
}

static mpfr_t r1539, r1540, r1541, r1542, r1543, r1544, r1545, r1546, r1547, r1548, r1549, r1550, r1551, r1552, r1553, r1554, r1555, r1556, r1557, r1558, r1559;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1539);
        mpfr_init_set_str(r1540, "1", 10, MPFR_RNDN);
        mpfr_init(r1541);
        mpfr_init(r1542);
        mpfr_init(r1543);
        mpfr_init(r1544);
        mpfr_init(r1545);
        mpfr_init(r1546);
        mpfr_init(r1547);
        mpfr_init(r1548);
        mpfr_init(r1549);
        mpfr_init(r1550);
        mpfr_init(r1551);
        mpfr_init(r1552);
        mpfr_init(r1553);
        mpfr_init(r1554);
        mpfr_init(r1555);
        mpfr_init(r1556);
        mpfr_init(r1557);
        mpfr_init(r1558);
        mpfr_init(r1559);
}

double f_dm(double a, double b, double eps) {
        mpfr_set_d(r1539, eps, MPFR_RNDN);
        ;
        mpfr_set_d(r1541, a, MPFR_RNDN);
        mpfr_mul(r1542, r1539, r1541, MPFR_RNDN);
        mpfr_exp(r1543, r1542, MPFR_RNDN);
        mpfr_sqrt(r1544, r1543, MPFR_RNDN);
        mpfr_sub(r1545, r1544, r1540, MPFR_RNDN);
        mpfr_add(r1546, r1540, r1544, MPFR_RNDN);
        mpfr_mul(r1547, r1545, r1546, MPFR_RNDN);
        mpfr_div(r1548, r1540, r1547, MPFR_RNDN);
        mpfr_mul(r1549, r1539, r1548, MPFR_RNDN);
        mpfr_set_d(r1550, b, MPFR_RNDN);
        mpfr_add(r1551, r1541, r1550, MPFR_RNDN);
        mpfr_mul(r1552, r1539, r1551, MPFR_RNDN);
        mpfr_exp(r1553, r1552, MPFR_RNDN);
        mpfr_sub(r1554, r1553, r1540, MPFR_RNDN);
        mpfr_mul(r1555, r1539, r1550, MPFR_RNDN);
        mpfr_exp(r1556, r1555, MPFR_RNDN);
        mpfr_sub(r1557, r1556, r1540, MPFR_RNDN);
        mpfr_div(r1558, r1554, r1557, MPFR_RNDN);
        mpfr_mul(r1559, r1549, r1558, MPFR_RNDN);
        return mpfr_get_d(r1559, MPFR_RNDN);
}

