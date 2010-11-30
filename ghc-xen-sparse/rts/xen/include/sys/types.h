// mainly from mini-os
/* -*-  Mode:C; c-basic-offset:4; tab-width:4 -*-
 ****************************************************************************
 * (C) 2003 - Rolf Neugebauer - Intel Research Cambridge
 ****************************************************************************
 *
 *        File: types.h
 *      Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
 *     Changes: 
 *              
 *        Date: May 2003
 * 
 * Environment: Xen Minimal OS
 * Description: a random collection of type definitions
 *
 ****************************************************************************
 * $Id: h-insert.h,v 1.4 2002/11/08 16:03:55 rn Exp $
 ****************************************************************************
 */

#ifndef _TYPES_H_
#define _TYPES_H_

// Comapatability: some people imprt stdio.h for NULL.
#ifndef NULL
#define NULL 0
#endif

typedef signed char         s8;
typedef unsigned char       u8;
typedef signed short        s16;
typedef unsigned short      u16;
typedef signed int          s32;
typedef unsigned int        u32;
#ifdef __i386__
typedef signed long long    s64;
typedef unsigned long long  u64;
#elif defined(__x86_64__)
typedef signed long         s64;
typedef unsigned long       u64;
#endif

typedef long unsigned int   size_t;

/* FreeBSD compat types */
typedef unsigned char       u_char;
typedef unsigned int        u_int;
typedef unsigned long       u_long;
#ifdef __i386__
typedef long long           quad_t;
typedef unsigned long long  u_quad_t;
typedef unsigned long int   uintptr_t;
typedef long int            intptr_t;

# ifdef CONFIG_X86_PAE
typedef struct { unsigned long pte_low, pte_high; } pte_t;
# else
typedef struct { unsigned long pte_low; } pte_t;
# endif
#elif defined(__x86_64__)
typedef long                quad_t;
typedef unsigned long       u_quad_t;
typedef unsigned long       uintptr_t;
typedef long int            intptr_t;

typedef struct { unsigned long pte; } pte_t;
#endif

typedef  u8 uint8_t;
typedef  s8 int8_t;
typedef u16 uint16_t;
typedef s16 int16_t;
typedef u32 uint32_t;
typedef u32 u_int32_t;
typedef s32 int32_t;
typedef u64 uint64_t;
typedef s64 int64_t;

typedef int                bool;

typedef int off_t;

typedef char *__caddr_t;
typedef __caddr_t caddr_t;
typedef unsigned long pid_t;

typedef uint32_t ino_t;
typedef uint64_t dev_t;

#define LITTLE_ENDIAN 1234

#endif /* _TYPES_H_ */
