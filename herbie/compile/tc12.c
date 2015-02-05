#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE problem 3.3.3";

double f_if(float x) {
        float r1602 = 1.0;
        float r1603 = x;
        float r1604 = r1603 + r1602;
        float r1605 = r1602 / r1604;
        float r1606 = 2.0;
        float r1607 = r1606 / r1603;
        float r1608 = r1605 - r1607;
        float r1609 = r1603 - r1602;
        float r1610 = r1602 / r1609;
        float r1611 = r1608 + r1610;
        return r1611;
}

double f_id(double x) {
        double r1612 = 1.0;
        double r1613 = x;
        double r1614 = r1613 + r1612;
        double r1615 = r1612 / r1614;
        double r1616 = 2.0;
        double r1617 = r1616 / r1613;
        double r1618 = r1615 - r1617;
        double r1619 = r1613 - r1612;
        double r1620 = r1612 / r1619;
        double r1621 = r1618 + r1620;
        return r1621;
}


double f_of(float x) {
        float r1622 = 1.0;
        float r1623 = x;
        float r1624 = r1622 / r1623;
        float r1625 = 2.0;
        float r1626 = r1623 + r1622;
        float r1627 = r1623 - r1622;
        float r1628 = r1626 * r1627;
        float r1629 = r1625 / r1628;
        float r1630 = r1624 * r1629;
        return r1630;
}

double f_od(double x) {
        double r1631 = x;
        double r1632 = -4503599627370495.0;
        bool r1633 = r1631 < r1632;
        double r1634 = 2.0;
        double r1635 = 1.0;
        double r1636 = r1631 * r1631;
        double r1637 = r1631 * r1636;
        double r1638 = r1636 * r1637;
        double r1639 = r1635 / r1638;
        double r1640 = r1634 * r1639;
        double r1641 = r1635 / r1637;
        double r1642 = r1634 * r1641;
        double r1643 = r1640 + r1642;
        double r1644 = 754746990.979506;
        bool r1645 = r1631 < r1644;
        double r1646 = r1631 - r1635;
        double r1647 = r1631 * r1646;
        double r1648 = r1635 + r1631;
        double r1649 = r1647 * r1648;
        double r1650 = r1646 + r1631;
        double r1651 = r1634 * r1646;
        double r1652 = r1650 - r1651;
        double r1653 = r1652 * r1631;
        double r1654 = r1631 - r1651;
        double r1655 = r1653 + r1654;
        double r1656 = r1649 / r1655;
        double r1657 = r1635 / r1656;
        double r1658 = r1645 ? r1657 : r1643;
        double r1659 = r1633 ? r1643 : r1658;
        return r1659;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1660, r1661, r1662, r1663, r1664, r1665, r1666, r1667, r1668, r1669;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1660, "1", 10, MPFR_RNDN);
        mpfr_init(r1661);
        mpfr_init(r1662);
        mpfr_init(r1663);
        mpfr_init_set_str(r1664, "2", 10, MPFR_RNDN);
        mpfr_init(r1665);
        mpfr_init(r1666);
        mpfr_init(r1667);
        mpfr_init(r1668);
        mpfr_init(r1669);
}

double f_im(double x) {
        ;
        mpfr_set_d(r1661, x, MPFR_RNDN);
        mpfr_add(r1662, r1661, r1660, MPFR_RNDN);
        mpfr_div(r1663, r1660, r1662, MPFR_RNDN);
        ;
        mpfr_div(r1665, r1664, r1661, MPFR_RNDN);
        mpfr_sub(r1666, r1663, r1665, MPFR_RNDN);
        mpfr_sub(r1667, r1661, r1660, MPFR_RNDN);
        mpfr_div(r1668, r1660, r1667, MPFR_RNDN);
        mpfr_add(r1669, r1666, r1668, MPFR_RNDN);
        return mpfr_get_d(r1669, MPFR_RNDN);
}

static mpfr_t r1670, r1671, r1672, r1673, r1674, r1675, r1676, r1677, r1678;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init_set_str(r1670, "1", 10, MPFR_RNDN);
        mpfr_init(r1671);
        mpfr_init(r1672);
        mpfr_init_set_str(r1673, "2", 10, MPFR_RNDN);
        mpfr_init(r1674);
        mpfr_init(r1675);
        mpfr_init(r1676);
        mpfr_init(r1677);
        mpfr_init(r1678);
}

double f_fm(double x) {
        ;
        mpfr_set_d(r1671, x, MPFR_RNDN);
        mpfr_div(r1672, r1670, r1671, MPFR_RNDN);
        ;
        mpfr_add(r1674, r1671, r1670, MPFR_RNDN);
        mpfr_sub(r1675, r1671, r1670, MPFR_RNDN);
        mpfr_mul(r1676, r1674, r1675, MPFR_RNDN);
        mpfr_div(r1677, r1673, r1676, MPFR_RNDN);
        mpfr_mul(r1678, r1672, r1677, MPFR_RNDN);
        return mpfr_get_d(r1678, MPFR_RNDN);
}

static mpfr_t r1679, r1680, r1681, r1682, r1683, r1684, r1685, r1686, r1687, r1688, r1689, r1690, r1691, r1692, r1693, r1694, r1695, r1696, r1697, r1698, r1699, r1700, r1701, r1702, r1703, r1704, r1705, r1706, r1707;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1679);
        mpfr_init_set_str(r1680, "-4503599627370495.0", 10, MPFR_RNDN);
        mpfr_init(r1681);
        mpfr_init_set_str(r1682, "2", 10, MPFR_RNDN);
        mpfr_init_set_str(r1683, "1", 10, MPFR_RNDN);
        mpfr_init(r1684);
        mpfr_init(r1685);
        mpfr_init(r1686);
        mpfr_init(r1687);
        mpfr_init(r1688);
        mpfr_init(r1689);
        mpfr_init(r1690);
        mpfr_init(r1691);
        mpfr_init_set_str(r1692, "754746990.979506", 10, MPFR_RNDN);
        mpfr_init(r1693);
        mpfr_init(r1694);
        mpfr_init(r1695);
        mpfr_init(r1696);
        mpfr_init(r1697);
        mpfr_init(r1698);
        mpfr_init(r1699);
        mpfr_init(r1700);
        mpfr_init(r1701);
        mpfr_init(r1702);
        mpfr_init(r1703);
        mpfr_init(r1704);
        mpfr_init(r1705);
        mpfr_init(r1706);
        mpfr_init(r1707);
}

double f_dm(double x) {
        mpfr_set_d(r1679, x, MPFR_RNDN);
        ;
        mpfr_set_si(r1681, mpfr_cmp(r1679, r1680) < 0, MPFR_RNDN);
        ;
        ;
        mpfr_mul(r1684, r1679, r1679, MPFR_RNDN);
        mpfr_mul(r1685, r1679, r1684, MPFR_RNDN);
        mpfr_mul(r1686, r1684, r1685, MPFR_RNDN);
        mpfr_div(r1687, r1683, r1686, MPFR_RNDN);
        mpfr_mul(r1688, r1682, r1687, MPFR_RNDN);
        mpfr_div(r1689, r1683, r1685, MPFR_RNDN);
        mpfr_mul(r1690, r1682, r1689, MPFR_RNDN);
        mpfr_add(r1691, r1688, r1690, MPFR_RNDN);
        ;
        mpfr_set_si(r1693, mpfr_cmp(r1679, r1692) < 0, MPFR_RNDN);
        mpfr_sub(r1694, r1679, r1683, MPFR_RNDN);
        mpfr_mul(r1695, r1679, r1694, MPFR_RNDN);
        mpfr_add(r1696, r1683, r1679, MPFR_RNDN);
        mpfr_mul(r1697, r1695, r1696, MPFR_RNDN);
        mpfr_add(r1698, r1694, r1679, MPFR_RNDN);
        mpfr_mul(r1699, r1682, r1694, MPFR_RNDN);
        mpfr_sub(r1700, r1698, r1699, MPFR_RNDN);
        mpfr_mul(r1701, r1700, r1679, MPFR_RNDN);
        mpfr_sub(r1702, r1679, r1699, MPFR_RNDN);
        mpfr_add(r1703, r1701, r1702, MPFR_RNDN);
        mpfr_div(r1704, r1697, r1703, MPFR_RNDN);
        mpfr_div(r1705, r1683, r1704, MPFR_RNDN);
        if (mpfr_get_si(r1693, MPFR_RNDN)) { mpfr_set(r1706, r1705, MPFR_RNDN); } else { mpfr_set(r1706, r1691, MPFR_RNDN); };
        if (mpfr_get_si(r1681, MPFR_RNDN)) { mpfr_set(r1707, r1691, MPFR_RNDN); } else { mpfr_set(r1707, r1706, MPFR_RNDN); };
        return mpfr_get_d(r1707, MPFR_RNDN);
}

