// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_DOM_XBMXEN_INCLUDE
#define XEN_DOM_XBMXEN_INCLUDE

#include <types.h>
#include <xen/xen.h> 

extern start_info_t *start_info;
extern shared_info_t *HYPERVISOR_shared_info;

void c_start(void *si);
void do_exit(void) __attribute__ ((__noreturn__));

#endif
