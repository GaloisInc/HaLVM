// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef HBM_XEN_PRIVILEGED_H
#define HBM_XEN_PRIVILEGED_H

#define __XEN__
#include <types.h>
#include <xen/xen.h>

#if __XEN_INTERFACE_VERSION__ > 0x00030101
#define SPLIT_PRIVILEGED
#endif

#ifdef SPLIT_PRIVILEGED
#include <xen/domctl.h>
#include <xen/platform.h>
#include <xen/sysctl.h>
#else
#include <xen/sched.h>
#include <xen/nmi.h>
#include <xen/dom0_ops.h>
#endif

int init_io_bitmap();
int update_va_mapping_otherdomain(unsigned long, unsigned long, 
                				  unsigned long, domid_t);
int populate_physmap(domid_t, unsigned long, void *);
int get_my_domid();

int set_iopl(u_int32_t);
int send_eoi(u_int32_t);
#ifdef SPLIT_PRIVILEGED
int do_domctl_op(xen_domctl_t *op);
int do_sysctl_op(xen_sysctl_t *op);
int do_platform_op(xen_platform_op_t *op);
#else
int do_dom0_operation(dom0_op_t *op);
#endif

#endif
