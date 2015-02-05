#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.4.3";

double f_if(float eps) {
        float r1560 = 1.0;
        float r1561 = eps;
        float r1562 = r1560 - r1561;
        float r1563 = r1560 + r1561;
        float r1564 = r1562 / r1563;
        float r1565 = log(r1564);
        return r1565;
}

double f_id(double eps) {
        double r1566 = 1.0;
        double r1567 = eps;
        double r1568 = r1566 - r1567;
        double r1569 = r1566 + r1567;
        double r1570 = r1568 / r1569;
        double r1571 = log(r1570);
        return r1571;
}


double f_of(float eps) {
        float r1572 = -0.4;
        float r1573 = eps;
        float r1574 = r1573 * r1573;
        float r1575 = r1573 * r1574;
        float r1576 = r1574 * r1575;
        float r1577 = 1.0;
        float r1578 = r1576 * r1577;
        float r1579 = r1572 * r1578;
        float r1580 = -0.6666666666666666;
        float r1581 = r1575 * r1577;
        float r1582 = r1580 * r1581;
        float r1583 = -2.0;
        float r1584 = r1573 * r1577;
        float r1585 = r1583 * r1584;
        float r1586 = r1582 + r1585;
        float r1587 = r1579 + r1586;
        return r1587;
}

double f_od(double eps) {
        double r1588 = -0.4;
        double r1589 = eps;
        double r1590 = r1589 * r1589;
        double r1591 = r1589 * r1590;
        double r1592 = r1590 * r1591;
        double r1593 = 1.0;
        double r1594 = r1592 * r1593;
        double r1595 = r1588 * r1594;
        double r1596 = -0.6666666666666666;
        double r1597 = r1591 * r1593;
        double r1598 = r1596 * r1597;
        double r1599 = -2.0;
        double r1600 = r1589 * r1593;
        double r1601 = r1599 * r1600;
        double r1602 = r1598 + r1601;
        double r1603 = r1595 + r1602;
        return r1603;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1604, r1605, r1606, r1607, r1608, r1609;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r1604, "1", 10, MPFR_RNDN);
        mpfr_init(r1605);
        mpfr_init(r1606);
        mpfr_init(r1607);
        mpfr_init(r1608);
        mpfr_init(r1609);
}

double f_im(double eps) {
        ;
        mpfr_set_d(r1605, eps, MPFR_RNDN);
        mpfr_sub(r1606, r1604, r1605, MPFR_RNDN);
        mpfr_add(r1607, r1604, r1605, MPFR_RNDN);
        mpfr_div(r1608, r1606, r1607, MPFR_RNDN);
        mpfr_log(r1609, r1608, MPFR_RNDN);
        return mpfr_get_d(r1609, MPFR_RNDN);
}

static mpfr_t r1610, r1611, r1612, r1613, r1614, r1615, r1616, r1617, r1618, r1619, r1620, r1621, r1622, r1623, r1624, r1625;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r1610, "-2/5", 10, MPFR_RNDN);
        mpfr_init(r1611);
        mpfr_init(r1612);
        mpfr_init(r1613);
        mpfr_init(r1614);
        mpfr_init_set_str(r1615, "1", 10, MPFR_RNDN);
        mpfr_init(r1616);
        mpfr_init(r1617);
        mpfr_init_set_str(r1618, "-2/3", 10, MPFR_RNDN);
        mpfr_init(r1619);
        mpfr_init(r1620);
        mpfr_init_set_str(r1621, "-2", 10, MPFR_RNDN);
        mpfr_init(r1622);
        mpfr_init(r1623);
        mpfr_init(r1624);
        mpfr_init(r1625);
}

double f_fm(double eps) {
        ;
        mpfr_set_d(r1611, eps, MPFR_RNDN);
        mpfr_mul(r1612, r1611, r1611, MPFR_RNDN);
        mpfr_mul(r1613, r1611, r1612, MPFR_RNDN);
        mpfr_mul(r1614, r1612, r1613, MPFR_RNDN);
        ;
        mpfr_mul(r1616, r1614, r1615, MPFR_RNDN);
        mpfr_mul(r1617, r1610, r1616, MPFR_RNDN);
        ;
        mpfr_mul(r1619, r1613, r1615, MPFR_RNDN);
        mpfr_mul(r1620, r1618, r1619, MPFR_RNDN);
        ;
        mpfr_mul(r1622, r1611, r1615, MPFR_RNDN);
        mpfr_mul(r1623, r1621, r1622, MPFR_RNDN);
        mpfr_add(r1624, r1620, r1623, MPFR_RNDN);
        mpfr_add(r1625, r1617, r1624, MPFR_RNDN);
        return mpfr_get_d(r1625, MPFR_RNDN);
}

static mpfr_t r1626, r1627, r1628, r1629, r1630, r1631, r1632, r1633, r1634, r1635, r1636, r1637, r1638, r1639, r1640, r1641;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(2448);
        mpfr_init_set_str(r1626, "-2/5", 10, MPFR_RNDN);
        mpfr_init(r1627);
        mpfr_init(r1628);
        mpfr_init(r1629);
        mpfr_init(r1630);
        mpfr_init_set_str(r1631, "1", 10, MPFR_RNDN);
        mpfr_init(r1632);
        mpfr_init(r1633);
        mpfr_init_set_str(r1634, "-2/3", 10, MPFR_RNDN);
        mpfr_init(r1635);
        mpfr_init(r1636);
        mpfr_init_set_str(r1637, "-2", 10, MPFR_RNDN);
        mpfr_init(r1638);
        mpfr_init(r1639);
        mpfr_init(r1640);
        mpfr_init(r1641);
}

double f_dm(double eps) {
        ;
        mpfr_set_d(r1627, eps, MPFR_RNDN);
        mpfr_mul(r1628, r1627, r1627, MPFR_RNDN);
        mpfr_mul(r1629, r1627, r1628, MPFR_RNDN);
        mpfr_mul(r1630, r1628, r1629, MPFR_RNDN);
        ;
        mpfr_mul(r1632, r1630, r1631, MPFR_RNDN);
        mpfr_mul(r1633, r1626, r1632, MPFR_RNDN);
        ;
        mpfr_mul(r1635, r1629, r1631, MPFR_RNDN);
        mpfr_mul(r1636, r1634, r1635, MPFR_RNDN);
        ;
        mpfr_mul(r1638, r1627, r1631, MPFR_RNDN);
        mpfr_mul(r1639, r1637, r1638, MPFR_RNDN);
        mpfr_add(r1640, r1636, r1639, MPFR_RNDN);
        mpfr_add(r1641, r1633, r1640, MPFR_RNDN);
        return mpfr_get_d(r1641, MPFR_RNDN);
}

