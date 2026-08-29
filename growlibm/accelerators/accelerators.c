/*
clang -dynamiclib -O3 -o growlibm/accelerators/libaccelerators.dylib \
growlibm/accelerators/accelerators.c \
growlibm/accelerators/cosquot.c \
growlibm/accelerators/e_rem_pio2.c \
growlibm/accelerators/powcos.c \
growlibm/accelerators/invgud.c \
-lm
*/


#define _GNU_SOURCE
#include <math.h>
#include <stdio.h>
#include "fdlibm.h"
#include "accelerators.h"

static inline void sincos_wrapper(double x, double* s, double* c) {
#if defined(__APPLE__)
    __sincos(x, s, c);
#else
    sincos(x, s, c);
#endif
}

static inline void fast_two_sum(double a, double b, double *s, double *e) {
    double sum = a + b;
    double z   = sum - a;
    *s = sum;
    *e = b - z;
}

static inline void unsorted_two_sum(double a, double b, double *s, double *e) {
    if (fabs(a) < fabs(b)) {
        double tmp = a; a = b; b = tmp;
    }
    fast_two_sum(a, b, s, e);
}

static inline double root5_positive(double x) {
    const double one_fifth = 0.2;
    int e;
    double m = frexp(x, &e);
    int q = e / 5;
    int r = e % 5;

    if (r < 0) {
        r += 5;
        q -= 1;
    }

    // x = 2^(5q) * s, with s in [0.5, 16). This keeps Newton updates stable.
    double s = ldexp(m, r);
    double u = exp(one_fifth * log1p(s - 1.0));

    for (int i = 0; i < 2; ++i) {
        double u2 = u * u;
        double u4 = u2 * u2;
        double ratio = s / u4;
        u = one_fifth * fma(4.0, u, ratio);
    }

    return ldexp(u, q);
}


double sinprod(double x, double y) {
    double p = x * y;
    double q = fma(x, y, -p); 

    double sin_p, cos_p, sin_q, cos_q;
    sincos_wrapper(p, &sin_p, &cos_p);
    sincos_wrapper(q, &sin_q, &cos_q);

    return fma(sin_p, cos_q, cos_p * sin_q);
}

double cosprod(double x, double y) {
    double p = x * y;
    double q = fma(x, y, -p); 

    double sin_p, cos_p, sin_q, cos_q;
    sincos_wrapper(p, &sin_p, &cos_p);
    sincos_wrapper(q, &sin_q, &cos_q);

    return fma(cos_p, cos_q, -sin_p * sin_q);
}

// double cos_quotient_xy(double x, double y) {
//     double p = x / y;
//     double r = fma(-p, y, x);
//     double q = r / y;

//     double sin_p, cos_p, sin_q, cos_q;
//     sincos_wrapper(p, &sin_p, &cos_p);
//     sincos_wrapper(q, &sin_q, &cos_q);

//     return fma(cos_p, cos_q, -sin_p * sin_q);
// }

double sindivpz(double x, double y, double z) {
    // quotient p+q
    double p = x / y;
    double r = fma(-p, y, x);
    double q = r / y;

    // p + z as (s_hi + s_lo)
    double s_hi, s_lo;
    unsorted_two_sum(p, z, &s_hi, &s_lo);

    // fold in q, then renormalize
    double t = s_lo + q;

    double P, Q;
    unsorted_two_sum(s_hi, t, &P, &Q);

    // sin(P+Q)
    double sinP, cosP, sinQ, cosQ;
    sincos_wrapper(P, &sinP, &cosP);
    sincos_wrapper(Q, &sinQ, &cosQ);

    return fma(sinP, cosQ, cosP * sinQ);
}

double cosdivpz(double x, double y, double z) {
    // Step 1: quotient as p + q
    double p = x / y;
    double r = fma(-p, y, x);   // remainder x - p*y (with one rounding)
    double q = r / y;

    // Step 2: add z to p with compensated sum (via reordered FastTwoSum)
    double s_hi, s_lo;
    unsorted_two_sum(p, z, &s_hi, &s_lo);

    // Step 3: fold in quotient residual and renormalize
    double t = s_lo + q;

    double P, Q;
    unsorted_two_sum(s_hi, t, &P, &Q);

    // Step 4: cos(P+Q) = cos(P)cos(Q) - sin(P)sin(Q)
    double sinP, cosP, sinQ, cosQ;
    sincos_wrapper(P, &sinP, &cosP);
    sincos_wrapper(Q, &sinQ, &cosQ);

    return fma(cosP, cosQ, -(sinP * sinQ));
}


// double log1pmd(double x) {
//     static const double 
//     Lp1 = 6.666666666666735130e-01,  // 3FE55555 55555593
//     Lp2 = 3.999999999940941908e-01,  // 3FD99999 9997FA04
//     Lp3 = 2.857142874366239149e-01,  // 3FD24924 94229359
//     Lp4 = 2.222219843214978396e-01,  // 3FCC71C5 1D8E78AF
//     Lp5 = 1.818357216161805012e-01,  // 3FC74664 96CB03DE
//     Lp6 = 1.531383769920937332e-01,  // 3FC39A09 D078C69F
//     Lp7 = 1.479819860511658591e-01;  // 3FC2F112 DF3E5244

//     double z = x * x;
    
//     double R = z * (Lp1 + z * (Lp2 + z * (Lp3 + z * (Lp4 + z * (Lp5 + z * (Lp6 + z * Lp7))))));

//     return 2.0 * x + x * R;
// }

double log1pmd(double x) {
    static const double 
    Lp1 = 6.666666666666735130e-01, Lp2 = 3.999999999940941908e-01,
    Lp3 = 2.857142874366239149e-01, Lp4 = 2.222219843214978396e-01,
    Lp5 = 1.818357216161805012e-01, Lp6 = 1.531383769920937332e-01,
    Lp7 = 1.479819860511658591e-01;

    double z = x * x;

    double R = fma(z, fma(z, fma(z, fma(z, fma(z, fma(z, Lp7, Lp6), Lp5), Lp4), Lp3), Lp2), Lp1);

    return fma(x * z, R, 2.0 * x);
}

// double log_tan(double x) {
//     double y[2];
    
//     int n = __ieee754_rem_pio2(x, y);
//     double r = y[0] + y[1];

//     const double r2 = r * r;
    
//     static const double c0  = 0x1.00000006d6ee1p0;
//     static const double c2  = 0x1.5555340b483e1p-3;
//     static const double c4  = 0x1.55627da178f7ap-5;
//     static const double c6  = 0x1.8aa9f1d42a76cp-7;
//     static const double c8  = 0x1.0b2c2f2c6a051p-8;
//     static const double c10 = 0x1.6c82aa9a13c14p-11;
//     static const double c12 = 0x1.07ef77f8abdfp-10;

//     double p = c12;
//     p = fma(r2, p, c10);
//     p = fma(r2, p, c8);
//     p = fma(r2, p, c6);
//     p = fma(r2, p, c4);
//     p = fma(r2, p, c2);
//     p = fma(r2, p, c0);

//     return r * p;
// }


/* @(#)e_hypot.c 1.3 95/01/18 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* __ieee754_hypot(x,y)
 *
 * Method :                  
 *	If (assume round-to-nearest) z=x*x+y*y 
 *	has error less than sqrt(2)/2 ulp, than 
 *	sqrt(z) has error less than 1 ulp (exercise).
 *
 *	So, compute sqrt(x*x+y*y) with some care as 
 *	follows to get the error below 1 ulp:
 *
 *	Assume x>y>0;
 *	(if possible, set rounding to round-to-nearest)
 *	1. if x > 2y  use
 *		x1*x1+(y*y+(x2*(x+x1))) for x*x+y*y
 *	where x1 = x with lower 32 bits cleared, x2 = x-x1; else
 *	2. if x <= 2y use
 *		t1*y1+((x-y)*(x-y)+(t1*y2+t2*y))
 *	where t1 = 2x with lower 32 bits cleared, t2 = 2x-t1, 
 *	y1= y with lower 32 bits chopped, y2 = y-y1.
 *		
 *	NOTE: scaling may be necessary if some argument is too 
 *	      large or too tiny
 *
 * Special cases:
 *	hypot(x,y) is INF if x or y is +INF or -INF; else
 *	hypot(x,y) is NAN if x or y is NAN.
 *
 * Accuracy:
 * 	hypot(x,y) returns sqrt(x^2+y^2) with error less 
 * 	than 1 ulps (units in the last place) 
 */


double hypot(double x, double y)
{
	double a=x,b=y,t1,t2,y1,y2,w;
	int j,k,ha,hb;

	ha = __HI(x)&0x7fffffff;	/* high word of  x */
	hb = __HI(y)&0x7fffffff;	/* high word of  y */
	if(hb > ha) {a=y;b=x;j=ha; ha=hb;hb=j;} else {a=x;b=y;}
	__HI(a) = ha;	/* a <- |a| */
	__HI(b) = hb;	/* b <- |b| */
	if((ha-hb)>0x3c00000) {return a+b;} /* x/y > 2**60 */
	k=0;
	if(ha > 0x5f300000) {	/* a>2**500 */
	   if(ha >= 0x7ff00000) {	/* Inf or NaN */
	       w = a+b;			/* for sNaN */
	       if(((ha&0xfffff)|__LO(a))==0) w = a;
	       if(((hb^0x7ff00000)|__LO(b))==0) w = b;
	       return w;
	   }
	   /* scale a and b by 2**-600 */
	   ha -= 0x25800000; hb -= 0x25800000;	k += 600;
	   __HI(a) = ha;
	   __HI(b) = hb;
	}
	if(hb < 0x20b00000) {	/* b < 2**-500 */
	    if(hb <= 0x000fffff) {	/* subnormal b or 0 */	
		if((hb|(__LO(b)))==0) return a;
		t1=0;
		__HI(t1) = 0x7fd00000;	/* t1=2^1022 */
		b *= t1;
		a *= t1;
		k -= 1022;
	    } else {		/* scale a and b by 2^600 */
	        ha += 0x25800000; 	/* a *= 2^600 */
		hb += 0x25800000;	/* b *= 2^600 */
		k -= 600;
	   	__HI(a) = ha;
	   	__HI(b) = hb;
	    }
	}
    /* medium size a and b */
	w = a-b;
	if (w>b) {
	    t1 = 0;
	    __HI(t1) = ha;
	    t2 = a-t1;
	    w  = sqrt(t1*t1-(b*(-b)-t2*(a+t1)));
	} else {
	    a  = a+a;
	    y1 = 0;
	    __HI(y1) = hb;
	    y2 = b - y1;
	    t1 = 0;
	    __HI(t1) = ha+0x00100000;
	    t2 = a - t1;
	    w  = sqrt(t1*y1-(w*(-w)-(t1*y2+t2*b)));
	}
	if(k!=0) {
	    t1 = 1.0;
	    __HI(t1) += (k<<20);
	    return t1*w;
	} else return w;
}


// double approx_sin_xy(double x, double y) {
//     double p = x * y;
//     double q = fma(x, y, -p); 

//     double sin_p, cos_p;
//     __sincos(p, &sin_p, &cos_p);

//     return fma(sin_p, 1, cos_p);
// }

// double approx_cos_xy(double x, double y) {
//     double p = x * y;
//     double q = fma(x, y, -p); 

//     double sin_p, cos_p;
//     __sincos(p, &sin_p, &cos_p);

//     return fma(cos_p, 1, -sin_p);
// }


double verdcos(double x){
    double sin_x = sin(x);
	return -2.0 * sin_x * sin_x;
}

double ncos1p(double z0) {
	return tan((0.5 * z0)) * sin(z0);
}

double pown2o3(double z0) {
    double r = cbrt(z0);
    return 1.0 / (r * r);
}

double pow2o5(double z0) {
    if (isnan(z0)) return z0 + z0;
    if (z0 == 0.0) return 0.0;
    if (isinf(z0)) return INFINITY;

    double r = root5_positive(fabs(z0));
    return r * r;
}

double pow3o5(double z0) {
    if (isnan(z0)) return z0 + z0;
    if (z0 == 0.0) return z0;
    if (isinf(z0)) return z0;

    double r = root5_positive(fabs(z0));
    double mag = r * r * r;
    return (z0 < 0.0) ? -mag : mag;
}

double pow5o3(double z0) {
    double r = cbrt(z0);
    return z0 * r * r;
}

double pown16o5(double z0) {
    if (isnan(z0)) return z0 + z0;
    if (z0 == 0.0) return INFINITY;
    if (isinf(z0)) return 0.0;

    double r = root5_positive(fabs(z0));
    int e;
    double m = frexp(r, &e);
    double m2 = m * m;
    double m4 = m2 * m2;
    double m8 = m4 * m4;
    double m16 = m8 * m8;
    return ldexp(1.0 / m16, -16 * e);
}

double pow1ms(double x, double y) {
	// (1 - x)^2 = 1 + x * (x - 2); use log1p for accuracy near 1.
	if (y == 0.0) {
		return 1.0;
	}

	double ax = fabs(x);
	if (ax > 0x1.0p+511) {
		// Avoid overflow in x*(x-2); use log|1-x| = log|x| + log|1-1/x|.
		double inv = 1.0 / x;
		double log_base = 2.0 * (log(ax) + log1p(-inv));
		return exp(y * log_base);
	}

	double delta = fma(x, x, -2.0 * x);

	if (delta < -1.0) {
		delta = -1.0;
	}

	double log_base = log1p(delta);
	return exp(y * log_base);
}
