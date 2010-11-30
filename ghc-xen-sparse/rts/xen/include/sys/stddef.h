// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_STDDEF_H
#define XEN_STDDEF_H

#include "types.h"

#define ptrdiff_t int // Taken from /usr/include/malloc.h

#define offsetof(type, member)  __builtin_offsetof (type, member)

#endif
