#include <tgmath.h>
#include <gmp.h>
#include <mpfr.h>
#include <stdio.h>
#include <stdbool.h>

char *name = "NMSE section 3.11";

double f_if(float x) {
        float r1642 = x;
        float r1643 = exp(r1642);
        float r1644 = 1.0;
        float r1645 = r1643 - r1644;
        float r1646 = r1643 / r1645;
        return r1646;
}

double f_id(double x) {
        double r1647 = x;
        double r1648 = exp(r1647);
        double r1649 = 1.0;
        double r1650 = r1648 - r1649;
        double r1651 = r1648 / r1650;
        return r1651;
}


double f_of(float x) {
        float r1652 = x;
        float r1653 = exp(r1652);
        float r1654 = 0.16666666666666666;
        float r1655 = r1652 * r1652;
        float r1656 = r1652 * r1655;
        float r1657 = 1.0;
        float r1658 = r1656 * r1657;
        float r1659 = r1654 * r1658;
        float r1660 = 0.5;
        float r1661 = r1652 * r1652;
        float r1662 = r1661 * r1657;
        float r1663 = r1660 * r1662;
        float r1664 = r1652 * r1657;
        float r1665 = r1657 * r1664;
        float r1666 = r1663 + r1665;
        float r1667 = r1659 + r1666;
        float r1668 = r1653 / r1667;
        return r1668;
}

double f_od(double x) {
        double r1669 = x;
        double r1670 = exp(r1669);
        double r1671 = 0.16666666666666666;
        double r1672 = r1669 * r1669;
        double r1673 = r1669 * r1672;
        double r1674 = 1.0;
        double r1675 = r1673 * r1674;
        double r1676 = r1671 * r1675;
        double r1677 = 0.5;
        double r1678 = r1669 * r1669;
        double r1679 = r1678 * r1674;
        double r1680 = r1677 * r1679;
        double r1681 = r1669 * r1674;
        double r1682 = r1674 * r1681;
        double r1683 = r1680 + r1682;
        double r1684 = r1676 + r1683;
        double r1685 = r1670 / r1684;
        return r1685;
}

void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {
        mpfr_fmod(r, n, d, MPFR_RNDN);
        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);
}


static mpfr_t r1686, r1687, r1688, r1689, r1690;

void setup_mpfr_f_im() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1686);
        mpfr_init(r1687);
        mpfr_init_set_str(r1688, "1", 10, MPFR_RNDN);
        mpfr_init(r1689);
        mpfr_init(r1690);
}

double f_im(double x) {
        mpfr_set_d(r1686, x, MPFR_RNDN);
        mpfr_exp(r1687, r1686, MPFR_RNDN);
        ;
        mpfr_sub(r1689, r1687, r1688, MPFR_RNDN);
        mpfr_div(r1690, r1687, r1689, MPFR_RNDN);
        return mpfr_get_d(r1690, MPFR_RNDN);
}

static mpfr_t r1691, r1692, r1693, r1694, r1695, r1696, r1697, r1698, r1699, r1700, r1701, r1702, r1703, r1704, r1705, r1706, r1707;

void setup_mpfr_f_fm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1691);
        mpfr_init(r1692);
        mpfr_init_set_str(r1693, "1/6", 10, MPFR_RNDN);
        mpfr_init(r1694);
        mpfr_init(r1695);
        mpfr_init_set_str(r1696, "1", 10, MPFR_RNDN);
        mpfr_init(r1697);
        mpfr_init(r1698);
        mpfr_init_set_str(r1699, "1/2", 10, MPFR_RNDN);
        mpfr_init(r1700);
        mpfr_init(r1701);
        mpfr_init(r1702);
        mpfr_init(r1703);
        mpfr_init(r1704);
        mpfr_init(r1705);
        mpfr_init(r1706);
        mpfr_init(r1707);
}

double f_fm(double x) {
        mpfr_set_d(r1691, x, MPFR_RNDN);
        mpfr_exp(r1692, r1691, MPFR_RNDN);
        ;
        mpfr_mul(r1694, r1691, r1691, MPFR_RNDN);
        mpfr_mul(r1695, r1691, r1694, MPFR_RNDN);
        ;
        mpfr_mul(r1697, r1695, r1696, MPFR_RNDN);
        mpfr_mul(r1698, r1693, r1697, MPFR_RNDN);
        ;
        mpfr_mul(r1700, r1691, r1691, MPFR_RNDN);
        mpfr_mul(r1701, r1700, r1696, MPFR_RNDN);
        mpfr_mul(r1702, r1699, r1701, MPFR_RNDN);
        mpfr_mul(r1703, r1691, r1696, MPFR_RNDN);
        mpfr_mul(r1704, r1696, r1703, MPFR_RNDN);
        mpfr_add(r1705, r1702, r1704, MPFR_RNDN);
        mpfr_add(r1706, r1698, r1705, MPFR_RNDN);
        mpfr_div(r1707, r1692, r1706, MPFR_RNDN);
        return mpfr_get_d(r1707, MPFR_RNDN);
}

static mpfr_t r1708, r1709, r1710, r1711, r1712, r1713, r1714, r1715, r1716, r1717, r1718, r1719, r1720, r1721, r1722, r1723, r1724;

void setup_mpfr_f_dm() {
        mpfr_set_default_prec(3216);
        mpfr_init(r1708);
        mpfr_init(r1709);
        mpfr_init_set_str(r1710, "1/6", 10, MPFR_RNDN);
        mpfr_init(r1711);
        mpfr_init(r1712);
        mpfr_init_set_str(r1713, "1", 10, MPFR_RNDN);
        mpfr_init(r1714);
        mpfr_init(r1715);
        mpfr_init_set_str(r1716, "1/2", 10, MPFR_RNDN);
        mpfr_init(r1717);
        mpfr_init(r1718);
        mpfr_init(r1719);
        mpfr_init(r1720);
        mpfr_init(r1721);
        mpfr_init(r1722);
        mpfr_init(r1723);
        mpfr_init(r1724);
}

double f_dm(double x) {
        mpfr_set_d(r1708, x, MPFR_RNDN);
        mpfr_exp(r1709, r1708, MPFR_RNDN);
        ;
        mpfr_mul(r1711, r1708, r1708, MPFR_RNDN);
        mpfr_mul(r1712, r1708, r1711, MPFR_RNDN);
        ;
        mpfr_mul(r1714, r1712, r1713, MPFR_RNDN);
        mpfr_mul(r1715, r1710, r1714, MPFR_RNDN);
        ;
        mpfr_mul(r1717, r1708, r1708, MPFR_RNDN);
        mpfr_mul(r1718, r1717, r1713, MPFR_RNDN);
        mpfr_mul(r1719, r1716, r1718, MPFR_RNDN);
        mpfr_mul(r1720, r1708, r1713, MPFR_RNDN);
        mpfr_mul(r1721, r1713, r1720, MPFR_RNDN);
        mpfr_add(r1722, r1719, r1721, MPFR_RNDN);
        mpfr_add(r1723, r1715, r1722, MPFR_RNDN);
        mpfr_div(r1724, r1709, r1723, MPFR_RNDN);
        return mpfr_get_d(r1724, MPFR_RNDN);
}

