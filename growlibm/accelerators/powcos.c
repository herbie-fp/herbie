#include <math.h>
#include "accelerators.h"

int __ieee754_rem_pio2(double x, double *y);

/*
 * Sollya scripts used for the polynomial generation:
 *
 *   display = hexadecimal!;
 *   hi = (pi/4)^2;
 *   lo = 0x1p-40;
 *
 *   p2 = remez(cos(sqrt(x))^2, 10, [0;hi]);
 *   q2 = remez((sin(sqrt(x))/sqrt(x))^2, 10, [lo;hi]);
 *
 *   p4 = remez(cos(sqrt(x))^4, 12, [0;hi]);
 *   q4 = remez((sin(sqrt(x))/sqrt(x))^4, 11, [lo;hi]);
 *
 *   p6 = remez(cos(sqrt(x))^6, 14, [0;hi]);
 *   q6 = remez((sin(sqrt(x))/sqrt(x))^6, 12, [lo;hi]);
 *
 *   lcos = remez(log(cos(sqrt(x))), 12, [0;hi]);
 *   lsinc = remez(log(sin(sqrt(x))/sqrt(x)), 12, [lo;hi]);
 *
 *   print(round(coeff(p, i), D, RN))
 */

static inline double powcos_clamp01(double x) {
  if (x < 0.0) return 0.0;
  if (x > 1.0) return 1.0;
  return x;
}

static inline int powcos_reduce_full(double x, double *r, double *z) {
  double y[2];
  int n = __ieee754_rem_pio2(x, y);
  double zz = fma(y[0], y[0], fma(2.0 * y[0], y[1], y[1] * y[1]));

  if (zz < 0.0) zz = 0.0;
  *r = y[0] + y[1];
  *z = zz;
  return n;
}

static inline void powcos_reduce(double x, int *odd_quadrant, double *z) {
  double r;
  int n = powcos_reduce_full(x, &r, z);
  *odd_quadrant = n & 1;
}

static inline double powcos_logcos_even(double z) {
  const double c0 = -0x1.dff88a50f5f76p-53;
  const double c1 = -0x1.ffffffffff821p-2;
  const double c2 = -0x1.5555555604894p-4;
  const double c3 = -0x1.6c16c10c71537p-6;
  const double c4 = -0x1.ba1bbcce3c31fp-8;
  const double c5 = -0x1.1ea38d4844306p-9;
  const double c6 = -0x1.837c25a39f34dp-11;
  const double c7 = -0x1.0acedb5665233p-12;
  const double c8 = -0x1.9966d194fdbc9p-14;
  const double c9 = -0x1.3cc234c60e236p-16;
  const double c10 = -0x1.05705e8b74741p-15;
  const double c11 = 0x1.8e8eab6384f26p-17;
  const double c12 = -0x1.23482b5eb1f1ap-17;
  double p = c12;

  if (z == 0.0) return 0.0;
  p = fma(z, p, c11);
  p = fma(z, p, c10);
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  return fma(z, p, c0);
}

static inline double powcos_logsin_ratio(double z) {
  const double c0 = -0x1.ddc88ce92f353p-81;
  const double c1 = -0x1.5555555555555p-3;
  const double c2 = -0x1.6c16c16c16c17p-8;
  const double c3 = -0x1.71de3a556c71bp-12;
  const double c4 = -0x1.bbd7793350bbcp-16;
  const double c5 = -0x1.1eed8efef055cp-19;
  const double c6 = -0x1.8355d22007d4ep-23;
  const double c7 = -0x1.0d0f7ce5d465p-26;
  const double c8 = -0x1.7da6e9f40418ep-30;
  const double c9 = -0x1.12d5a8461fb08p-33;
  const double c10 = -0x1.947704deca229p-37;
  const double c11 = -0x1.0f279d85ef3f6p-40;
  const double c12 = -0x1.42999227657dfp-43;
  double p = c12;

  if (z == 0.0) return 0.0;
  p = fma(z, p, c11);
  p = fma(z, p, c10);
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  return fma(z, p, c0);
}

static inline int powcos_logcos_positive(int k, double r, double z, double *logcos_out) {
  double ar = fabs(r);

  switch (k & 3) {
    case 0:
      if (z < 0x1p-8) {
        double sh = sin(0.5 * r);
        *logcos_out = log1p(-2.0 * sh * sh);
      } else {
        *logcos_out = powcos_logcos_even(z);
      }
      return 1;
    case 1:
      if (r < 0.0) {
        if (ar == 0.0) return 0;
        *logcos_out = log(ar) + powcos_logsin_ratio(z);
        return 1;
      }
      return 0;
    case 2:
      return 0;
    default:
      if (r > 0.0) {
        if (ar == 0.0) return 0;
        *logcos_out = log(ar) + powcos_logsin_ratio(z);
        return 1;
      }
      return 0;
  }
}

static inline double powcos_reduced_cos(int k, double r) {
  double ar = fabs(r);
  double m = (k & 1) ? sin(ar) : cos(ar);

  switch (k & 3) {
    case 0:
      return m;
    case 1:
      if (r < 0.0 || (r == 0.0 && signbit(r))) return m;
      return -m;
    case 2:
      return -m;
    default:
      if (r > 0.0 || (r == 0.0 && !signbit(r))) return m;
      return -m;
  }
}

static inline double powcos2_even(double z) {
  const double c0 = 0x1p0;
  const double c1 = -0x1p0;
  const double c2 = 0x1.5555555555555p-2;
  const double c3 = -0x1.6c16c16c16c16p-5;
  const double c4 = 0x1.a01a01a019fc8p-9;
  const double c5 = -0x1.27e4fb778788p-13;
  const double c6 = 0x1.1eed8efe1ebaap-18;
  const double c7 = -0x1.93974976073cap-24;
  const double c8 = 0x1.ae7eb70bd5813p-30;
  const double c9 = -0x1.67fe89745f754p-36;
  const double c10 = 0x1.d73498c8bd4a1p-43;

  double p = c10;
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  return fma(z, p, c0);
}

static inline double powcos2_odd(double z) {
  const double c0 = 0x1p0;
  const double c1 = -0x1.5555555555555p-2;
  const double c2 = 0x1.6c16c16c16c17p-5;
  const double c3 = -0x1.a01a01a01a01ap-9;
  const double c4 = 0x1.27e4fb7789f53p-13;
  const double c5 = -0x1.1eed8eff8cf8cp-18;
  const double c6 = 0x1.93974a8b5d10cp-24;
  const double c7 = -0x1.ae7f3df1dcb7fp-30;
  const double c8 = 0x1.6827473c57c4ap-36;
  const double c9 = -0x1.e51c9a372e0a1p-43;
  const double c10 = 0x1.065b5bbc3e42fp-49;

  double p = c10;
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  p = fma(z, p, c0);
  return z * p;
}

static inline double powcos4_even(double z) {
  const double c0 = 0x1p0;
  const double c1 = -0x1p1;
  const double c2 = 0x1.aaaaaaaaaaaabp0;
  const double c3 = -0x1.82d82d82d82d7p-1;
  const double c4 = 0x1.a69a69a69a646p-3;
  const double c5 = -0x1.290ce07300967p-5;
  const double c6 = 0x1.1f354a6319c66p-8;
  const double c7 = -0x1.93b083f15cdc7p-12;
  const double c8 = 0x1.ae85f553203f1p-16;
  const double c9 = -0x1.68287f29a8505p-20;
  const double c10 = 0x1.e52e382a474a4p-25;
  const double c11 = -0x1.0b9bdd32afa63p-29;
  const double c12 = 0x1.c3d9afe7af577p-35;

  double p = c12;
  p = fma(z, p, c11);
  p = fma(z, p, c10);
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  return fma(z, p, c0);
}

static inline double powcos4_odd(double z) {
  const double c0 = 0x1p0;
  const double c1 = -0x1.5555555555555p-1;
  const double c2 = 0x1.999999999999ap-3;
  const double c3 = -0x1.26bd167c126bcp-5;
  const double c4 = 0x1.1ea5d39bcd99dp-8;
  const double c5 = -0x1.937e111757f5dp-12;
  const double c6 = 0x1.ae788473aac71p-16;
  const double c7 = -0x1.68261d744f3d9p-20;
  const double c8 = 0x1.e5420d88ee091p-25;
  const double c9 = -0x1.0cde0e2e8326cp-29;
  const double c10 = 0x1.f1543e4cbb6fcp-35;
  const double c11 = -0x1.6b49952869679p-40;

  double p = c11;
  double z2 = z * z;

  p = fma(z, p, c10);
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  p = fma(z, p, c0);
  return z2 * p;
}

static inline double powcos6_even(double z) {
  const double c0 = 0x1p0;
  const double c1 = -0x1.8p1;
  const double c2 = 0x1p2;
  const double c3 = -0x1.9111111111111p1;
  const double c4 = 0x1.9c09c09c09bfcp0;
  const double c5 = -0x1.2669de1558b3p-1;
  const double c6 = 0x1.304cf1280299ap-3;
  const double c7 = -0x1.d5b1ee95d42b1p-6;
  const double c8 = 0x1.16a9c7a8acb5p-8;
  const double c9 = -0x1.04f3c49fc766p-11;
  const double c10 = 0x1.8aa8b914aad39p-15;
  const double c11 = -0x1.eb85a420b4fa4p-19;
  const double c12 = 0x1.000eb090ba619p-22;
  const double c13 = -0x1.be9d7261577e9p-27;
  const double c14 = 0x1.1d995a7fa5433p-31;

  double p = c14;

  p = fma(z, p, c13);
  p = fma(z, p, c12);
  p = fma(z, p, c11);
  p = fma(z, p, c10);
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  return fma(z, p, c0);
}

static inline double powcos6_odd(double z) {
  const double c0 = 0x1p0;
  const double c1 = -0x1p0;
  const double c2 = 0x1.ddddddddddddep-2;
  const double c3 = -0x1.1566abc011564p-3;
  const double c4 = 0x1.c2c6d7181c111p-6;
  const double c5 = -0x1.119e49fd6e1fbp-8;
  const double c6 = 0x1.02d78b0f1c4cap-11;
  const double c7 = -0x1.893d0ae3fd69ep-15;
  const double c8 = 0x1.eac36f66eb4ebp-19;
  const double c9 = -0x1.00290d2310d7fp-22;
  const double c10 = 0x1.c5e0f549a38fp-27;
  const double c11 = -0x1.56a60708e72b9p-31;
  const double c12 = 0x1.8c1897ddf077dp-36;

  double p = c12;
  double z2 = z * z;

  p = fma(z, p, c11);
  p = fma(z, p, c10);
  p = fma(z, p, c9);
  p = fma(z, p, c8);
  p = fma(z, p, c7);
  p = fma(z, p, c6);
  p = fma(z, p, c5);
  p = fma(z, p, c4);
  p = fma(z, p, c3);
  p = fma(z, p, c2);
  p = fma(z, p, c1);
  p = fma(z, p, c0);
  return z2 * z * p;
}

double powcos2(double x) {
  int odd_quadrant;
  double z, out;

  if (!isfinite(x)) return x - x;

  powcos_reduce(x, &odd_quadrant, &z);
  out = odd_quadrant ? powcos2_odd(z) : powcos2_even(z);
  return powcos_clamp01(out);
}

double powcos4(double x) {
  int odd_quadrant;
  double z, out;

  if (!isfinite(x)) return x - x;

  powcos_reduce(x, &odd_quadrant, &z);
  out = odd_quadrant ? powcos4_odd(z) : powcos4_even(z);
  return powcos_clamp01(out);
}

double powcos6(double x) {
  int odd_quadrant;
  double z, out;

  if (!isfinite(x)) return x - x;

  powcos_reduce(x, &odd_quadrant, &z);
  out = odd_quadrant ? powcos6_odd(z) : powcos6_even(z);
  return powcos_clamp01(out);
}

double powcos(double x, double y) {
  double r, z, c;
  int n, k;

  if (!isfinite(x) || !isfinite(y)) return pow(cos(x), y);

  n = powcos_reduce_full(x, &r, &z);
  k = n & 3;
  c = powcos_reduced_cos(k, r);

  if (c > 0.0) {
    double logc;
    if (powcos_logcos_positive(k, r, z, &logc)) return exp(y * logc);
    return exp(y * log(c));
  }

  return pow(c, y);
}
