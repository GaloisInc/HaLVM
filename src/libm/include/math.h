// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_MATH_H
#define XEN_MATH_H

double acosh(double);
float acoshf(float);
double cos(double);
float cosf(float);
double expm1(double);
float expm1f(float);
double fabs(double);
float fabsf(float);
int finite(double);
int finitef(float);
double floor(double);
float floorf(float);
double ldexp(double, int);
float ldexpf(float, int);
double log(double);
float logf(float);
double log1p(double);
float log1pf(float);
double remainder(double, double);
float remainderf(float, float);
double scalbn(double, int);
float scalbnf(float, int);
double sin(double);
float sinf(float);
double sqrt(double);
float sqrtf(float);
int __fpclassifyf(float);
int __fpclassifyd(double);

double modf(double, double *);

/* I've stolen this stuff from NetBSD. See this licence:
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 *
 */

#define FP_INFINITE     0x00
#define FP_NAN          0x01
#define FP_NORMAL       0x02
#define FP_SUBNORMAL    0x03
#define FP_ZERO         0x04

#ifdef __HAVE_LONG_DOUBLE
#define __fpmacro_unary_floating(__name, __arg0)    \
        /* LINTED */                                \
        ((sizeof (__arg0) == sizeof (float))        \
         ?   __ ## __name ## f (__arg0)             \
         : (sizeof (__arg0) == sizeof (double))     \
         ?   __ ## __name ## d (__arg0)             \
         :   __ ## __name ## l (__arg0))
#else
#define __fpmacro_unary_floating(__name, __arg0)    \
      /* LINTED */                                  \
    ((sizeof (__arg0) == sizeof (float))            \
         ?   __ ## __name ## f (__arg0)             \
         :   __ ## __name ## d (__arg0))
#endif /* __HAVE_LONG_DOUBLE */


#define fpclassify(__x) __fpmacro_unary_floating(fpclassify, __x)

#endif
