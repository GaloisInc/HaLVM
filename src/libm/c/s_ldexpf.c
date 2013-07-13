/* s_ldexp0f.c -- float version of s_ldexp0.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

#if defined(LIBM_SCCS) && !defined(lint)
__RCSID("$NetBSD: s_ldexpf.c,v 1.7 2008/04/25 22:21:53 christos Exp $");
#endif

#include "math.h"
#include "math_private.h"

float
ldexpf(float value, int exp0)
{
	if(!finitef(value)||value==(float)0.0) return value;
	value = scalbnf(value,exp0);
	if(!finitef(value)||value==(float)0.0) errno = ERANGE;
	return value;
}
