/* $NetBSD: llrint.c,v 1.2 2004/10/13 15:18:32 drochner Exp $ */

/*
 * Written by Matthias Drochner <drochner@NetBSD.org>.
 * Public domain.
 */

#include <math.h>
#include <sys/ieee754.h>
#include <machine/limits.h>
#include "math_private.h"

#define LRINTNAME llrint
#define RESTYPE long long int
#define RESTYPE_MIN LLONG_MIN
#define RESTYPE_MAX LLONG_MAX
#define RESTYPE_BITS (sizeof(RESTYPE) * 8)

static const double
TWO52[2]={
  4.50359962737049600000e+15, /* 0x43300000, 0x00000000 */
 -4.50359962737049600000e+15, /* 0xC3300000, 0x00000000 */
};

RESTYPE
LRINTNAME(double x)
{
	u_int32_t i0, i1;
	int e, s, shift;
	RESTYPE res;

	GET_HIGH_WORD(i0, x);
	e = i0 >> 20;
	s = (uint32_t)e >> DBL_EXPBITS;
	e = (e & 0x7ff) - DBL_EXP_BIAS;

	/* 1.0 x 2^-1 is the smallest number which can be rounded to 1 */
	if (e < -1)
		return (0);
	/* 1.0 x 2^31 (or 2^63) is already too large */
	if (e >= (int)RESTYPE_BITS - 1)
		return (s ? RESTYPE_MIN : RESTYPE_MAX); /* ??? unspecified */

	/* >= 2^52 is already an exact integer */
	if (e < DBL_FRACBITS) {
		/* round, using current direction */
		x += TWO52[s];
		x -= TWO52[s];
	}

	EXTRACT_WORDS(i0, i1, x);
	e = ((i0 >> 20) & 0x7ff) - DBL_EXP_BIAS;
	i0 &= 0xfffff;
	i0 |= (1 << 20);

	shift = e - DBL_FRACBITS;
	if (shift >=0)
		res = (shift < 32 ? (RESTYPE)i1 << shift : 0);
	else
		res = (shift > -32 ? i1 >> -shift : 0);
	shift += 32;
	if (shift >=0)
		res |= (shift < 32 ? (RESTYPE)i0 << shift : 0);
	else
		res |= (shift > -32 ? i0 >> -shift : 0);

	return (s ? -res : res);
}
