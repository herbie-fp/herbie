#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.2";

double f_if(float x, float eps) {
        float r1412 = x;
        float r1413 = eps;
        float r1414 = r1412 + r1413;
        float r1415 = tan(r1414);
        float r1416 = tan(r1412);
        float r1417 = r1415 - r1416;
        return r1417;
}

double f_id(double x, double eps) {
        double r1418 = x;
        double r1419 = eps;
        double r1420 = r1418 + r1419;
        double r1421 = tan(r1420);
        double r1422 = tan(r1418);
        double r1423 = r1421 - r1422;
        return r1423;
}


double f_of(float x, float eps) {
        float r1424 = eps;
        float r1425 = -0.10532079637050629;
        bool r1426 = r1424 < r1425;
        float r1427 = x;
        float r1428 = cos(r1427);
        float r1429 = sin(r1427);
        float r1430 = cos(r1424);
        float r1431 = r1429 * r1430;
        float r1432 = sin(r1424);
        float r1433 = r1428 * r1432;
        float r1434 = r1431 + r1433;
        float r1435 = r1428 * r1434;
        float r1436 = r1427 + r1424;
        float r1437 = cos(r1436);
        float r1438 = r1429 * r1437;
        float r1439 = r1435 - r1438;
        float r1440 = r1428 * r1430;
        float r1441 = r1429 * r1432;
        float r1442 = r1440 - r1441;
        float r1443 = r1428 * r1442;
        float r1444 = r1439 / r1443;
        float r1445 = 0.21416425704956055;
        bool r1446 = r1424 < r1445;
        float r1447 = -0.16666666666666666;
        float r1448 = 3.0;
        float r1449 = pow(r1424, r1448);
        float r1450 = r1447 * r1449;
        float r1451 = r1424 + r1450;
        float r1452 = r1451 / r1443;
        float r1453 = r1430 * r1429;
        float r1454 = r1453 + r1433;
        float r1455 = r1428 * r1454;
        float r1456 = pow(r1455, r1448);
        float r1457 = r1437 * r1429;
        float r1458 = pow(r1457, r1448);
        float r1459 = r1456 - r1458;
        float r1460 = r1455 * r1455;
        float r1461 = r1438 * r1438;
        float r1462 = r1460 + r1461;
        float r1463 = r1428 * r1429;
        float r1464 = r1463 * r1454;
        float r1465 = r1464 * r1437;
        float r1466 = r1462 + r1465;
        float r1467 = r1459 / r1466;
        float r1468 = r1428 * r1437;
        float r1469 = r1467 / r1468;
        float r1470 = r1446 ? r1452 : r1469;
        float r1471 = r1426 ? r1444 : r1470;
        return r1471;
}

double f_od(double x, double eps) {
        double r1472 = eps;
        double r1473 = -1221806947.8226738;
        bool r1474 = r1472 < r1473;
        double r1475 = x;
        double r1476 = cos(r1475);
        double r1477 = sin(r1475);
        double r1478 = 1.0;
        double r1479 = r1478 / r1475;
        double r1480 = r1478 / r1472;
        double r1481 = r1479 + r1480;
        double r1482 = cos(r1481);
        double r1483 = r1478 / r1478;
        double r1484 = r1483 / r1478;
        double r1485 = r1482 * r1484;
        double r1486 = r1477 * r1485;
        double r1487 = r1475 + r1472;
        double r1488 = sin(r1487);
        double r1489 = r1486 / r1488;
        double r1490 = r1476 - r1489;
        double r1491 = 1.0 / tan(r1487);
        double r1492 = r1476 * r1491;
        double r1493 = r1490 / r1492;
        double r1494 = 6.35115643818464e-07;
        bool r1495 = r1472 < r1494;
        double r1496 = -0.16666666666666666;
        double r1497 = r1472 * r1472;
        double r1498 = r1472 * r1497;
        double r1499 = r1478 * r1478;
        double r1500 = r1498 * r1499;
        double r1501 = r1496 * r1500;
        double r1502 = r1472 * r1499;
        double r1503 = r1478 * r1502;
        double r1504 = r1501 + r1503;
        double r1505 = cos(r1487);
        double r1506 = r1476 * r1505;
        double r1507 = r1504 / r1506;
        double r1508 = r1495 ? r1507 : r1493;
        double r1509 = r1474 ? r1493 : r1508;
        return r1509;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1510, r1511, r1512, r1513, r1514, r1515;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1510);
        mpfr_init(r1511);
        mpfr_init(r1512);
        mpfr_init(r1513);
        mpfr_init(r1514);
        mpfr_init(r1515);
}

double f_im(double x, double eps) {
        mpfr_set_d(r1510, x, MPFR_RNDN);
        mpfr_set_d(r1511, eps, MPFR_RNDN);
        mpfr_add(r1512, r1510, r1511, MPFR_RNDN);
        mpfr_tan(r1513, r1512, MPFR_RNDN);
        mpfr_tan(r1514, r1510, MPFR_RNDN);
        mpfr_sub(r1515, r1513, r1514, MPFR_RNDN);
        return mpfr_get_d(r1515, MPFR_RNDN);
}

static mpfr_t r1516, r1517, r1518, r1519, r1520, r1521, r1522, r1523, r1524, r1525, r1526, r1527, r1528, r1529, r1530, r1531, r1532, r1533, r1534, r1535, r1536, r1537, r1538, r1539, r1540, r1541, r1542, r1543, r1544, r1545, r1546, r1547, r1548, r1549, r1550, r1551, r1552, r1553, r1554, r1555, r1556, r1557, r1558, r1559, r1560, r1561, r1562, r1563;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1516);
        mpfr_init_set_str(r1517, "-0.10532079637050629", 10, MPFR_RNDN);
        mpfr_init(r1518);
        mpfr_init(r1519);
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
        mpfr_init_set_str(r1537, "0.21416425704956055", 10, MPFR_RNDN);
        mpfr_init(r1538);
        mpfr_init_set_str(r1539, "-1/6", 10, MPFR_RNDN);
        mpfr_init_set_str(r1540, "3", 10, MPFR_RNDN);
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
        mpfr_init(r1560);
        mpfr_init(r1561);
        mpfr_init(r1562);
        mpfr_init(r1563);
}

double f_fm(double x, double eps) {
        mpfr_set_d(r1516, eps, MPFR_RNDN);
        ;
        mpfr_set_si(r1518, mpfr_cmp(r1516, r1517) < 0, MPFR_RNDN);
        mpfr_set_d(r1519, x, MPFR_RNDN);
        mpfr_cos(r1520, r1519, MPFR_RNDN);
        mpfr_sin(r1521, r1519, MPFR_RNDN);
        mpfr_cos(r1522, r1516, MPFR_RNDN);
        mpfr_mul(r1523, r1521, r1522, MPFR_RNDN);
        mpfr_sin(r1524, r1516, MPFR_RNDN);
        mpfr_mul(r1525, r1520, r1524, MPFR_RNDN);
        mpfr_add(r1526, r1523, r1525, MPFR_RNDN);
        mpfr_mul(r1527, r1520, r1526, MPFR_RNDN);
        mpfr_add(r1528, r1519, r1516, MPFR_RNDN);
        mpfr_cos(r1529, r1528, MPFR_RNDN);
        mpfr_mul(r1530, r1521, r1529, MPFR_RNDN);
        mpfr_sub(r1531, r1527, r1530, MPFR_RNDN);
        mpfr_mul(r1532, r1520, r1522, MPFR_RNDN);
        mpfr_mul(r1533, r1521, r1524, MPFR_RNDN);
        mpfr_sub(r1534, r1532, r1533, MPFR_RNDN);
        mpfr_mul(r1535, r1520, r1534, MPFR_RNDN);
        mpfr_div(r1536, r1531, r1535, MPFR_RNDN);
        ;
        mpfr_set_si(r1538, mpfr_cmp(r1516, r1537) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_pow(r1541, r1516, r1540, MPFR_RNDN);
        mpfr_mul(r1542, r1539, r1541, MPFR_RNDN);
        mpfr_add(r1543, r1516, r1542, MPFR_RNDN);
        mpfr_div(r1544, r1543, r1535, MPFR_RNDN);
        mpfr_mul(r1545, r1522, r1521, MPFR_RNDN);
        mpfr_add(r1546, r1545, r1525, MPFR_RNDN);
        mpfr_mul(r1547, r1520, r1546, MPFR_RNDN);
        mpfr_pow(r1548, r1547, r1540, MPFR_RNDN);
        mpfr_mul(r1549, r1529, r1521, MPFR_RNDN);
        mpfr_pow(r1550, r1549, r1540, MPFR_RNDN);
        mpfr_sub(r1551, r1548, r1550, MPFR_RNDN);
        mpfr_mul(r1552, r1547, r1547, MPFR_RNDN);
        mpfr_mul(r1553, r1530, r1530, MPFR_RNDN);
        mpfr_add(r1554, r1552, r1553, MPFR_RNDN);
        mpfr_mul(r1555, r1520, r1521, MPFR_RNDN);
        mpfr_mul(r1556, r1555, r1546, MPFR_RNDN);
        mpfr_mul(r1557, r1556, r1529, MPFR_RNDN);
        mpfr_add(r1558, r1554, r1557, MPFR_RNDN);
        mpfr_div(r1559, r1551, r1558, MPFR_RNDN);
        mpfr_mul(r1560, r1520, r1529, MPFR_RNDN);
        mpfr_div(r1561, r1559, r1560, MPFR_RNDN);
        if (mpfr_get_si(r1538, MPFR_RNDN)) { mpfr_set(r1562, r1544, MPFR_RNDN); } else { mpfr_set(r1562, r1561, MPFR_RNDN); };
        if (mpfr_get_si(r1518, MPFR_RNDN)) { mpfr_set(r1563, r1536, MPFR_RNDN); } else { mpfr_set(r1563, r1562, MPFR_RNDN); };
        return mpfr_get_d(r1563, MPFR_RNDN);
}

static mpfr_t r1564, r1565, r1566, r1567, r1568, r1569, r1570, r1571, r1572, r1573, r1574, r1575, r1576, r1577, r1578, r1579, r1580, r1581, r1582, r1583, r1584, r1585, r1586, r1587, r1588, r1589, r1590, r1591, r1592, r1593, r1594, r1595, r1596, r1597, r1598, r1599, r1600, r1601;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init(r1564);
        mpfr_init_set_str(r1565, "-1221806947.8226738", 10, MPFR_RNDN);
        mpfr_init(r1566);
        mpfr_init(r1567);
        mpfr_init(r1568);
        mpfr_init(r1569);
        mpfr_init_set_str(r1570, "1", 10, MPFR_RNDN);
        mpfr_init(r1571);
        mpfr_init(r1572);
        mpfr_init(r1573);
        mpfr_init(r1574);
        mpfr_init(r1575);
        mpfr_init(r1576);
        mpfr_init(r1577);
        mpfr_init(r1578);
        mpfr_init(r1579);
        mpfr_init(r1580);
        mpfr_init(r1581);
        mpfr_init(r1582);
        mpfr_init(r1583);
        mpfr_init(r1584);
        mpfr_init(r1585);
        mpfr_init_set_str(r1586, "6.35115643818464e-07", 10, MPFR_RNDN);
        mpfr_init(r1587);
        mpfr_init_set_str(r1588, "-1/6", 10, MPFR_RNDN);
        mpfr_init(r1589);
        mpfr_init(r1590);
        mpfr_init(r1591);
        mpfr_init(r1592);
        mpfr_init(r1593);
        mpfr_init(r1594);
        mpfr_init(r1595);
        mpfr_init(r1596);
        mpfr_init(r1597);
        mpfr_init(r1598);
        mpfr_init(r1599);
        mpfr_init(r1600);
        mpfr_init(r1601);
}

double f_dm(double x, double eps) {
        mpfr_set_d(r1564, eps, MPFR_RNDN);
        ;
        mpfr_set_si(r1566, mpfr_cmp(r1564, r1565) < 0, MPFR_RNDN);
        mpfr_set_d(r1567, x, MPFR_RNDN);
        mpfr_cos(r1568, r1567, MPFR_RNDN);
        mpfr_sin(r1569, r1567, MPFR_RNDN);
        ;
        mpfr_div(r1571, r1570, r1567, MPFR_RNDN);
        mpfr_div(r1572, r1570, r1564, MPFR_RNDN);
        mpfr_add(r1573, r1571, r1572, MPFR_RNDN);
        mpfr_cos(r1574, r1573, MPFR_RNDN);
        mpfr_div(r1575, r1570, r1570, MPFR_RNDN);
        mpfr_div(r1576, r1575, r1570, MPFR_RNDN);
        mpfr_mul(r1577, r1574, r1576, MPFR_RNDN);
        mpfr_mul(r1578, r1569, r1577, MPFR_RNDN);
        mpfr_add(r1579, r1567, r1564, MPFR_RNDN);
        mpfr_sin(r1580, r1579, MPFR_RNDN);
        mpfr_div(r1581, r1578, r1580, MPFR_RNDN);
        mpfr_sub(r1582, r1568, r1581, MPFR_RNDN);
        mpfr_cot(r1583, r1579, MPFR_RNDN);
        mpfr_mul(r1584, r1568, r1583, MPFR_RNDN);
        mpfr_div(r1585, r1582, r1584, MPFR_RNDN);
        ;
        mpfr_set_si(r1587, mpfr_cmp(r1564, r1586) < 0, MPFR_RNDN);
        ;
        mpfr_mul(r1589, r1564, r1564, MPFR_RNDN);
        mpfr_mul(r1590, r1564, r1589, MPFR_RNDN);
        mpfr_mul(r1591, r1570, r1570, MPFR_RNDN);
        mpfr_mul(r1592, r1590, r1591, MPFR_RNDN);
        mpfr_mul(r1593, r1588, r1592, MPFR_RNDN);
        mpfr_mul(r1594, r1564, r1591, MPFR_RNDN);
        mpfr_mul(r1595, r1570, r1594, MPFR_RNDN);
        mpfr_add(r1596, r1593, r1595, MPFR_RNDN);
        mpfr_cos(r1597, r1579, MPFR_RNDN);
        mpfr_mul(r1598, r1568, r1597, MPFR_RNDN);
        mpfr_div(r1599, r1596, r1598, MPFR_RNDN);
        if (mpfr_get_si(r1587, MPFR_RNDN)) { mpfr_set(r1600, r1599, MPFR_RNDN); } else { mpfr_set(r1600, r1585, MPFR_RNDN); };
        if (mpfr_get_si(r1566, MPFR_RNDN)) { mpfr_set(r1601, r1585, MPFR_RNDN); } else { mpfr_set(r1601, r1600, MPFR_RNDN); };
        return mpfr_get_d(r1601, MPFR_RNDN);
}

