// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#if defined(__x86_64__)
#include "hypercall-x86_64.h"
#else
#include "hypercall-x86_32.h"
#endif

