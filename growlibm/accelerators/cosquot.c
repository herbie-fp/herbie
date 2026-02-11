#include <float.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

typedef struct {
  double hi;
  double lo;
} dd_t;

static inline dd_t dd_make(double hi, double lo) {
  double s = hi + lo;
  double e = lo - (s - hi);
  dd_t out = {s, e};
  return out;
}

static inline dd_t dd_from_double(double x) {
  dd_t out = {x, 0.0};
  return out;
}

static inline void two_sum(double a, double b, double *s, double *e) {
  *s = a + b;
  {
    double bb = *s - a;
    *e = (a - (*s - bb)) + (b - bb);
  }
}

static inline void two_prod(double a, double b, double *p, double *e) {
  *p = a * b;
  *e = fma(a, b, -*p);
}

static inline dd_t dd_add(dd_t a, dd_t b) {
  double s, e, t;
  two_sum(a.hi, b.hi, &s, &e);
  t = e + a.lo + b.lo;
  return dd_make(s, t);
}

static inline dd_t dd_add_d(dd_t a, double b) {
  double s, e;
  two_sum(a.hi, b, &s, &e);
  return dd_make(s, e + a.lo);
}

static inline dd_t dd_sub(dd_t a, dd_t b) {
  dd_t nb = {-b.hi, -b.lo};
  return dd_add(a, nb);
}

static inline dd_t dd_mul_d(dd_t a, double b) {
  double p, e;
  two_prod(a.hi, b, &p, &e);
  e += a.lo * b;
  return dd_make(p, e);
}

static inline dd_t dd_div_d(dd_t a, double b) {
  double q1, q2, q3;
  dd_t q, r;

  q1 = a.hi / b;
  q = dd_from_double(q1);

  r = dd_sub(a, dd_mul_d(q, b));
  q2 = r.hi / b;
  q = dd_add_d(q, q2);

  r = dd_sub(a, dd_mul_d(q, b));
  q3 = r.hi / b;
  q = dd_add_d(q, q3);

  return q;
}

/* 3.243F6A8885A3... in base 2^16 limbs. */
static const uint16_t PI_LIMBS[] = {
  0x0003,
    0x243F,
    0x6A88,
    0x85A3,
    0x08D3,
    0x1319,
    0x8A2E,
    0x0370,
    0x7344,
    0xA409,
    0x3822,
    0x299F,
    0x31D0,
    0x082E,
    0xFA98,
    0xEC4E,
    0x6C89,
    0x4528,
    0x21E6,
    0x38D0,
    0x1377,
    0xBE54,
    0x66CF,
    0x34E9,
    0x0C6C,
    0xC0AC,
    0x29B7,
    0xC97C,
    0x50DD,
    0x3F84,
    0xD5B5,
    0xB547,
    0x0917,
    0x9216,
    0xD5D9,
    0x8979,
    0xFB1B,
    0xD131,
    0x0BA6,
    0x98DF,
    0xB5AC,
    0x2FFD,
    0x72DB,
    0xD01A,
    0xDFB7,
    0xB8E1,
    0xAFED,
    0x6A26,
    0x7E96,
    0xBA7C,
    0x9045,
    0xF12C,
    0x7F99,
    0x24A1,
    0x9947,
    0xB391,
    0x6CF7,
    0x0801,
    0xF2E2,
    0x858E,
    0xFC16,
    0x6369,
    0x20D8,
    0x7157,
    0x4E69,
    0xA458,
    0xFEA3,
    0xF493,
    0x3D7E,
    0x0D95,
    0x748F,
    0x728E,
    0xB658,
    0x718B,
    0xCD58,
    0x8215,
    0x4AEE,
    0x7B54,
    0xA41D,
    0xC25A,
    0x59B5,
    0x9C30,
    0xD539,
    0x2AF2,
    0x6013,
    0xC5D1,
    0xB023,
    0x2860,
    0x85F0,
    0xCA41,
    0x7918,
    0xB8DB,
    0x38EF,
    0x8E79,
    0xDCB0,
    0x603A,
    0x180E,
    0x6C9E,
    0x0E8B,
    0xB01E,
    0x8A3E,
    0xD715,
    0x77C1,
    0xBD31,
    0x4B27,
    0x78AF,
    0x2FDA,
    0x5560,
    0x5C60,
    0xE655,
    0x25F3,
    0xAA55,
    0xAB94,
    0x5748,
    0x9862,
    0x63E8,
    0x1440,
    0x55CA,
    0x396A,
    0x2AAB,
    0x10B6,
    0xB4CC,
    0x5C34,
    0x1141,
    0xE8CE,
    0xA154,
    0x86AF,
    0x7C72,
    0xE993,
    0xB3EE,
    0x1411,
    0x636F,
    0xBC2A,
    0x2BA9,
    0xC55D,
    0x7418,
    0x31F6,
    0xCE5C,
    0x3E16,
    0x9B87,
    0x931E,
    0xAFD6,
    0xBA33,
    0x6C24,
    0xCF5C,
    0x7A32,
    0x5381,
    0x2895,
    0x8677,
    0x3B8F,
    0x4898,
    0x6B4B,
    0xB9AF,
    0xC4BF,
    0xE81B,
    0x6628,
    0x2193,
    0x61D8,
    0x09CC,
    0xFB21,
    0xA991,
    0x487C,
    0xAC60,
    0x5DEC,
    0x8032,
    0xEF84,
    0x5D5D,
    0xE985,
    0x75B1,
    0xDC26};

/* 2/pi = 0.A2F9836E4E44... in base 2^16 limbs. */
static const uint16_t TWO_OVER_PI_LIMBS[] = {
  0xA2F9,
    0x836E,
    0x4E44,
    0x1529,
    0xFC27,
    0x57D1,
    0xF534,
    0xDDC0,
    0xDB62,
    0x9599,
    0x3C43,
    0x9041,
    0xFE51,
    0x63AB,
    0xDEBB,
    0xC561,
    0xB724,
    0x6E3A,
    0x424D,
    0xD2E0,
    0x0649,
    0x2EEA,
    0x09D1,
    0x921C,
    0xFE1D,
    0xEB1C,
    0xB129,
    0xA73E,
    0xE882,
    0x35F5,
    0x2EBB,
    0x4484,
    0xE99C,
    0x7026,
    0xB45F,
    0x7E41,
    0x3991,
    0xD639,
    0x8353,
    0x39F4,
    0x9C84,
    0x5F8B,
    0xBDF9,
    0x283B,
    0x1FF8,
    0x97FF,
    0xDE05,
    0x980F,
    0xEF2F,
    0x118B,
    0x5A0A,
    0x6D1F,
    0x6D36,
    0x7ECF,
    0x27CB,
    0x09B7,
    0x4F46,
    0x3F66,
    0x9E5F,
    0xEA2D,
    0x7527,
    0xBAC7,
    0xEBE5,
    0xF17B,
    0x3D07,
    0x39F7,
    0x8A52,
    0x92EA,
    0x6BFB,
    0x5FB1,
    0x1F8D,
    0x5D08,
    0x5603,
    0x3046,
    0xFC7B,
    0x6BAB,
    0xF0CF,
    0xBC20,
    0x9AF4,
    0x361D,
    0xA9E3,
    0x9161,
    0x5EE6,
    0x1B08,
    0x6599,
    0x855F,
    0x14A0,
    0x6840,
    0x8DFF,
    0xD880,
    0x4D73,
    0x2731,
    0x0606,
    0x1556,
    0xCA73,
    0xA8C9,
    0x60E2,
    0x7BC0,
    0x8C6B,
    0x47C4,
    0x19C3,
    0x67CD,
    0xDCE8,
    0x092A,
    0x8359,
    0xC476,
    0x8B96,
    0x1CA6,
    0xDDAF,
    0x44D1,
    0x5719,
    0x053E,
    0xA5FF,
    0x0705,
    0x3F7E,
    0x33E8,
    0x32C2,
    0xDE4F,
    0x9832,
    0x7DBB,
    0xC33D,
    0x26EF,
    0x6B1E,
    0x5EF8,
    0x9F3A,
    0x1F35,
    0xCAF2,
    0x7F1D,
    0x87F1,
    0x2190,
    0x7C7C,
    0x246A,
    0xFA6E,
    0xD577,
    0x2D30,
    0x433B,
    0x15C6,
    0x14B5,
    0x9D19,
    0xC3C2,
    0xC4AD,
    0x414D,
    0x2C5D,
    0x000C,
    0x467D,
    0x862D,
    0x71E3,
    0x9AC6,
    0x9B00,
    0x6233,
    0x7CD2,
    0xB497,
    0xA7B4,
    0xD555,
    0x37F6,
    0x3ED7,
    0x1810,
    0xA3FC,
    0x764D,
    0x2A9D,
    0x64AB,
    0xD770,
    0xF87C,
    0x6357,
    0xB07A,
    0xE715,
    0x1756,
    0x49C0,
    0xD9D6,
    0x3B38};
enum { N_PI_LIMBS = (int)(sizeof(PI_LIMBS) / sizeof(PI_LIMBS[0])) };

typedef struct {
  double y_hi;
  double y_lo;
} ypi_stream_t;

#define EXP_CAP 1024

/*
 * Split a into hi + lo with <= 32 mantissa bits in hi and <= 21 in lo.
 * This keeps y_part * limb (limb < 2^16) exact in double.
 */
static void split_32(double a, double *hi, double *lo) {
  if (!isfinite(a) || a == 0.0) {
    *hi = a;
    *lo = 0.0;
    return;
  }

  {
    int exp2;
    double m = frexp(a, &exp2);
    double sigd = ldexp(fabs(m), 53);
    uint64_t sig = (uint64_t)nearbyint(sigd);
    uint64_t hi_sig;
    double hi_m;

    if (sig == (1ULL << 53)) {
      sig >>= 1;
      exp2 += 1;
    }

    hi_sig = sig & ~((1ULL << 21) - 1ULL);
    hi_m = ldexp((double)hi_sig, -53);
    if (m < 0.0) hi_m = -hi_m;

    *hi = ldexp(hi_m, exp2);
    *lo = a - *hi;
  }
}

static dd_t two_over_pi_dd(void) {
  dd_t acc = dd_from_double(0.0);
  int i;
  for (i = 0; i < (int)(sizeof(TWO_OVER_PI_LIMBS) / sizeof(TWO_OVER_PI_LIMBS[0])); ++i) {
    double term = ldexp((double)TWO_OVER_PI_LIMBS[i], -16 * (i + 1));
    acc = dd_add_d(acc, term);
  }
  return acc;
}

static void build_y_pi_stream(double y, ypi_stream_t *stream) {
  split_32(y, &stream->y_hi, &stream->y_lo);
}

/* Shewchuk-style expansion grow: exact add of one scalar into an expansion. */
static int grow_expansion_zeroelim(int elen, const double *e, double b, double *h) {
  double Q = b;
  int hlen = 0;
  int i;

  for (i = 0; i < elen; ++i) {
    double sum, hh;
    two_sum(Q, e[i], &sum, &hh);
    if (hh != 0.0) {
      if (hlen < EXP_CAP) h[hlen++] = hh;
    }
    Q = sum;
  }

  if (Q != 0.0 || hlen == 0) {
    if (hlen < EXP_CAP) {
      h[hlen++] = Q;
    } else {
      h[EXP_CAP - 1] = Q;
      hlen = EXP_CAP;
    }
  }

  return hlen;
}

static int expansion_add_term(int len, double *exp, double term, double *scratch) {
  int out = grow_expansion_zeroelim(len, exp, term, scratch);
  memcpy(exp, scratch, (size_t)out * sizeof(double));
  return out;
}

static int expansion_add_terms_signed(int len,
                                      double *exp,
                                      int nterms,
                                      const double *terms,
                                      double sign,
                                      double *scratch) {
  int i;
  for (i = 0; i < nterms; ++i) {
    len = expansion_add_term(len, exp, sign * terms[i], scratch);
  }
  return len;
}

static int expansion_sign(int len, const double *exp) {
  int i;
  for (i = len - 1; i >= 0; --i) {
    if (exp[i] > 0.0) return 1;
    if (exp[i] < 0.0) return -1;
  }
  return 0;
}

static int expansion_is_finite(int len, const double *exp) {
  int i;
  for (i = 0; i < len; ++i) {
    if (!isfinite(exp[i])) return 0;
  }
  return 1;
}

static dd_t expansion_to_dd(int len, const double *exp) {
  dd_t acc = dd_from_double(0.0);
  int i;
  for (i = len - 1; i >= 0; --i) {
    acc = dd_add_d(acc, exp[i]);
  }
  return acc;
}

/*
 * Build y*pi*2^shift as an expansion by alternating hi/lo streams from
 * smallest to largest term.
 */
static int build_candidate_expansion(const ypi_stream_t *stream,
                                     int shift,
                                     double *cand,
                                     double *scratch) {
  int len = 1;
  int n = N_PI_LIMBS;
  int i;
  cand[0] = 0.0;

  for (i = n - 1; i >= 0; --i) {
    int e = shift - 16 * i;
    double limb = (double)PI_LIMBS[i];
    double t_hi = ldexp(stream->y_hi, e) * limb;
    double t_lo = ldexp(stream->y_lo, e) * limb;
    len = expansion_add_term(len, cand, t_hi, scratch);
    len = expansion_add_term(len, cand, t_lo, scratch);
  }

  return len;
}

typedef struct {
  dd_t remainder;
  int quotient_mod2;
} reduce_out_t;

static reduce_out_t hp_reduce_x_mod_y_pi(double ax, double ay, const ypi_stream_t *stream) {
  reduce_out_t out;
  double rem[EXP_CAP], cand[EXP_CAP], diff[EXP_CAP], mod[EXP_CAP], scratch[EXP_CAP];
  int rem_len = 1;
  int mod_len;
  dd_t mod_dd;
  dd_t two_over_pi;
  dd_t inv_pi;
  dd_t inv_mod;
  dd_t q_seed;
  int shift;
  int i;

  out.remainder = dd_from_double(NAN);
  out.quotient_mod2 = 0;

  rem[0] = ax;
  mod_len = build_candidate_expansion(stream, 0, mod, scratch);
  if (!expansion_is_finite(mod_len, mod)) return out;

  mod_dd = expansion_to_dd(mod_len, mod);
  if (!(mod_dd.hi > 0.0) || !isfinite(mod_dd.hi)) return out;

  /* 2/pi table gives a quotient seed; exponent ratio backs it up. */
  two_over_pi = two_over_pi_dd();
  inv_pi = dd_mul_d(two_over_pi, 0.5);
  inv_mod = dd_div_d(inv_pi, ay);
  q_seed = dd_mul_d(inv_mod, ax);

  shift = ilogb(ax) - ilogb(mod_dd.hi);
  if (q_seed.hi > 0.0 && isfinite(q_seed.hi)) {
    int qexp = ilogb(q_seed.hi);
    if (qexp > shift) shift = qexp;
  }
  if (shift < 0) shift = 0;

  while (shift > 0) {
    int probe_len = build_candidate_expansion(stream, shift, cand, scratch);
    if (expansion_is_finite(probe_len, cand)) break;
    --shift;
  }

  for (i = shift; i >= 0; --i) {
    int cand_len = build_candidate_expansion(stream, i, cand, scratch);
    int diff_len;

    if (!expansion_is_finite(cand_len, cand)) continue;

    memcpy(diff, rem, (size_t)rem_len * sizeof(double));
    diff_len = rem_len;
    diff_len = expansion_add_terms_signed(diff_len, diff, cand_len, cand, -1.0, scratch);

    if (expansion_sign(diff_len, diff) >= 0) {
      rem_len = diff_len;
      memcpy(rem, diff, (size_t)rem_len * sizeof(double));
      if (i == 0) out.quotient_mod2 ^= 1;
    }
  }

  for (;;) {
    int diff_len;
    memcpy(diff, rem, (size_t)rem_len * sizeof(double));
    diff_len = rem_len;
    diff_len = expansion_add_terms_signed(diff_len, diff, mod_len, mod, -1.0, scratch);
    if (expansion_sign(diff_len, diff) < 0) break;
    rem_len = diff_len;
    memcpy(rem, diff, (size_t)rem_len * sizeof(double));
    out.quotient_mod2 ^= 1;
  }

  if (expansion_sign(rem_len, rem) < 0) {
    rem_len = expansion_add_terms_signed(rem_len, rem, mod_len, mod, 1.0, scratch);
    out.quotient_mod2 ^= 1;
  }

  out.remainder = expansion_to_dd(rem_len, rem);
  return out;
}

/*
 * Public reduction helper for future sin/tan variants.
 * Returns 0 on success.
 */
int reduce_x_mod_y_pi(double x, double y, double *r_hi, double *r_lo, int *quotient_mod2) {
  const double pi_hi = 3.14159265358979323846264338327950288;
  double ax, ay;
  ypi_stream_t stream;
  reduce_out_t red;

  if (r_hi == 0 || r_lo == 0 || quotient_mod2 == 0) return -1;
  if (!isfinite(x) || !isfinite(y) || y == 0.0) return -1;

  ax = fabs(x);
  ay = fabs(y);

  if (ax == 0.0) {
    *r_hi = 0.0;
    *r_lo = 0.0;
    *quotient_mod2 = 0;
    return 0;
  }

  if (isinf(ay)) {
    *r_hi = 0.0;
    *r_lo = 0.0;
    *quotient_mod2 = 0;
    return 0;
  }

  /* If y*pi is guaranteed above all finite x, quotient is zero. */
  if (ay > DBL_MAX / pi_hi) {
    *r_hi = ax;
    *r_lo = 0.0;
    *quotient_mod2 = 0;
    return 0;
  }

  build_y_pi_stream(ay, &stream);
  red = hp_reduce_x_mod_y_pi(ax, ay, &stream);

  if (!isfinite(red.remainder.hi)) return -1;

  *r_hi = red.remainder.hi;
  *r_lo = red.remainder.lo;
  *quotient_mod2 = red.quotient_mod2 & 1;
  return 0;
}

/* Optional helper: directly return (x/y) mod pi in double-double chunks. */
int reduce_x_over_y_mod_pi(double x, double y, double *z_hi, double *z_lo, int *quotient_mod2) {
  double r_hi, r_lo;
  dd_t z;

  if (z_hi == 0 || z_lo == 0 || quotient_mod2 == 0) return -1;
  if (reduce_x_mod_y_pi(x, y, &r_hi, &r_lo, quotient_mod2) != 0) return -1;

  z = dd_div_d(dd_make(r_hi, r_lo), fabs(y));
  *z_hi = z.hi;
  *z_lo = z.lo;
  return 0;
}

static double cos_dd(dd_t z) {
  double ch = cos(z.hi);
  double sh = sin(z.hi);
  double cl = cos(z.lo);
  double sl = sin(z.lo);
  return ch * cl - sh * sl;
}

/*
 * Compute cos(x/y) using x mod (y*pi) reduction, then divide by y in dd.
 */
double cosquot(double x, double y) {
  double z_hi, z_lo;
  int qmod2;
  dd_t z;
  double out;

  if (isnan(x) || isnan(y)) return NAN;
  if (y == 0.0) return NAN;
  if (isinf(x)) return NAN;
  if (isinf(y)) return 1.0;
  if (x == 0.0) return 1.0;

  if (reduce_x_over_y_mod_pi(x, y, &z_hi, &z_lo, &qmod2) != 0) return NAN;

  z = dd_make(z_hi, z_lo);
  out = cos_dd(z);
  if (qmod2) out = -out;
  return out;
}
