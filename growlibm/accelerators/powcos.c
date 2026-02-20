#include <math.h>
#include <stdint.h>
#include "accelerators.h"

int __ieee754_rem_pio2(double x, double *y);

#if (defined(__clang__) && __clang_major__ >= 14) || \
  (defined(__GNUC__) && __GNUC__ >= 14 && __BITINT_MAXWIDTH__ && __BITINT_MAXWIDTH__ >= 128)
typedef unsigned _BitInt(128) u128;
#else
typedef unsigned __int128 u128;
#endif

typedef union {
  double f;
  uint64_t u;
} b64u64_u;

/* 1/(2*pi) approximated downward in 64-bit limbs, copied from cos.c. */
static const uint64_t T[20] = {
  0x28be60db9391054a, 0x7f09d5f47d4d3770, 0x36d8a5664f10e410,
  0x7f9458eaf7aef158, 0x6dc91b8e909374b8, 0x01924bba82746487,
  0x3f877ac72c4a69cf, 0xba208d7d4baed121, 0x3a671c09ad17df90,
  0x4e64758e60d4ce7d, 0x272117e2ef7e4a0e, 0xc7fe25fff7816603,
  0xfbcbc462d6829b47, 0xdb4d9fb3c9f2c26d, 0xd3d18fd9a797fa8b,
  0x5d49eeb1faf97c5e, 0xcf41ce7de294a4ba, 0x9afed7ec47e35742,
  0x1580cc11bf1edaea, 0xfc33ef0826bd0d87,
};

static inline void a_mul(double *hi, double *lo, double a, double b) {
  *hi = a * b;
  *lo = fma(a, b, -*hi);
}

/* h+l <- c1/2^64 + c0/2^128 */
static void set_dd(double *h, double *l, uint64_t c1, uint64_t c0) {
  uint64_t e, f, g;
  b64u64_u t;

  if (c1) {
    e = __builtin_clzll(c1);
    if (e) {
      c1 = (c1 << e) | (c0 >> (64 - e));
      c0 = c0 << e;
    }
    f = 0x3fe - e;
    t.u = (f << 52) | ((c1 << 1) >> 12);
    *h = t.f;
    c0 = (c1 << 53) | (c0 >> 11);
    if (c0) {
      g = __builtin_clzll(c0);
      if (g) c0 = c0 << g;
      t.u = ((f - 53 - g) << 52) | ((c0 << 1) >> 12);
      *l = t.f;
    } else {
      *l = 0.0;
    }
  } else if (c0) {
    e = __builtin_clzll(c0);
    f = 0x3fe - 64 - e;
    c0 = c0 << (e + 1); // most significant bit shifted out
    t.u = (f << 52) | (c0 >> 12);
    *h = t.f;
    c0 = c0 << 52;
    if (c0) {
      g = __builtin_clzll(c0);
      c0 = c0 << (g + 1);
      t.u = ((f - 64 - g) << 52) | (c0 >> 12);
      *l = t.f;
    } else {
      *l = 0.0;
    }
  } else {
    *h = *l = 0.0;
  }
}

/* Fast reduction copied/adapted from cos.c::reduce_fast. */
static int fast_reduce(double *h, double *l, double x, double *err1) {
  if (__builtin_expect(x <= 0x1.921fb54442d17p+2, 1)) { // x < 2*pi
#define CH 0x1.45f306dc9c883p-3
#define CL -0x1.6b01ec5417056p-57
    a_mul(h, l, CH, x);
    *l = fma(CL, x, *l);
    *err1 = 0x1.d9p-105 * *h;
#undef CH
#undef CL
  } else {
    b64u64_u t = {.f = x};
    int e = (t.u >> 52) & 0x7ff;
    uint64_t m = (1ull << 52) | (t.u & 0xfffffffffffffull);
    uint64_t c[3];
    u128 u;

    if (e <= 1074) {
      u = (u128)m * (u128)T[1];
      c[0] = (uint64_t)u;
      c[1] = (uint64_t)(u >> 64);
      u = (u128)m * (u128)T[0];
      c[1] += (uint64_t)u;
      c[2] = (uint64_t)(u >> 64) + (c[1] < (uint64_t)u);
      e = 1075 - e;
    } else {
      int i = (e - 1138 + 63) / 64;
      u = (u128)m * (u128)T[i + 2];
      c[0] = (uint64_t)u;
      c[1] = (uint64_t)(u >> 64);
      u = (u128)m * (u128)T[i + 1];
      c[1] += (uint64_t)u;
      c[2] = (uint64_t)(u >> 64) + (c[1] < (uint64_t)u);
      u = (u128)m * (u128)T[i];
      c[2] += (uint64_t)u;
      e = 1139 + (i << 6) - e;
    }

    if (e == 64) {
      c[0] = c[1];
      c[1] = c[2];
    } else {
      c[0] = (c[1] << (64 - e)) | (c[0] >> e);
      c[1] = (c[2] << (64 - e)) | (c[1] >> e);
    }

    set_dd(h, l, c[1], c[0]);
    *err1 = 0x1.01p-76;
  }

  double i = floor(*h * 0x1p11);
  *h = fma(i, -0x1p-11, *h);
  return (int)i;
}

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

static inline int powcos_reduce_full_slow(double x, double *r, double *z) {
  double y[2];
  int n = __ieee754_rem_pio2(x, y);
  double zz = fma(y[0], y[0], fma(2.0 * y[0], y[1], y[1] * y[1]));

  if (zz < 0.0) zz = 0.0;
  *r = y[0] + y[1];
  *z = zz;
  return n;
}

static inline int powcos_reduce_full(double x, double *r, double *z) {
  const long double two_pi = 0x1.921fb54442d18469898cc51701b8p+2L;
  const double min_fast = 0x1.6a09e667f3bccp-27;
  double ax = fabs(x);

  if (ax <= min_fast) {
    *r = x;
    *z = x * x;
    return 0;
  }

  double h, l, err1;
  int i = fast_reduce(&h, &l, ax, &err1);
  long double frac = (long double)i * 0x1p-11L + (long double)h + (long double)l;
  long double s = 4.0L * frac;
  int n_abs = (int)floorl(s + 0.5L);
  long double dist = fabsl(s - (long double)n_abs);
  long double tol = fmaxl(4.0L * (long double)err1 + 0x1p-68L, 0x1p-60L);

  if (dist <= tol) {
    return powcos_reduce_full_slow(x, r, z);
  }

  long double delta = (long double)(i - (n_abs << 9)) * 0x1p-11L + (long double)h + (long double)l;
  /* Slow fallback is only needed very close to cos-zero crossings. */
  if ((n_abs & 1) && fabsl(delta) < 0x1p-12L) {
    return powcos_reduce_full_slow(x, r, z);
  }
  long double rr = delta * two_pi;
  int n = n_abs;

  if (x < 0.0) {
    rr = -rr;
    n = -n_abs;
  }

  *r = (double)rr;
  *z = (double)(rr * rr);
  if (*z < 0.0) *z = 0.0;
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
