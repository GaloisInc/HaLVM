#ifndef HALVMCORE_HYPERCALLS_H
#define HALVMCORE_HYPERCALLS_H

#include <stdint.h>
#define __XEN_TOOLS__
#include <hypercall.h>
#include <xen/domctl.h>
#include <xen/sysctl.h>

int do_domctl_op(xen_domctl_t *);
int do_sysctl_op(xen_sysctl_t *);
int do_physdev_op(int, void *);
int do_memory_op(int, void *);
int update_va_mapping_otherdomain(unsigned long, unsigned long,
                                  unsigned long, domid_t);

#endif
