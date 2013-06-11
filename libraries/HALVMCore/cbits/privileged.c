// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com> and Iavor Diatchki <iavor@galois.com>
// BANNEREND
#include <privileged.h>
#include <hypercall.h>
#include <xen/xen.h>
#include <xen/memory.h>
#include <xen/physdev.h>
#include <sys/mman.h>
#include <string.h>
#include <mm.h>
#include <sys/strings.h>
#include <sys/errno.h>
#include <xen/xen.h>

#define IO_PORT_NUM 65536
#define IO_PORT_BYTES (IO_PORT_NUM / 8)

uint8_t io_bitmap[IO_PORT_BYTES];
extern start_info_t *start_info;

#ifndef SPLIT_PRIVILEGED
typedef physdevop_set_iobitmap_t physdev_set_iobitmap_t;
#endif

int init_io_bitmap() {
  physdev_set_iobitmap_t op;
#if __XEN_INTERFACE_VERSION__ >= 0x00030205
  typeof(op.bitmap) map;
  set_xen_guest_handle(map, io_bitmap);
  op.bitmap = map;
#else
/*
  bytes = mmap(0, IO_PORT_BYTES, 0, 0, -1, 0);
  if (bytes == 0) return -1; 
*/
  memset(io_bitmap, 0x00, IO_PORT_BYTES); /* enables all ports */
  op.bitmap   = io_bitmap;
#endif
  op.nr_ports = IO_PORT_NUM;
  HYPERVISOR_physdev_op(PHYSDEVOP_SET_IOBITMAP, &op);
  return 0;
}

int set_iopl(u_int32_t level)
{
  physdev_set_iopl_t op = { .iopl = level };
  return HYPERVISOR_physdev_op(PHYSDEVOP_set_iopl, &op);
}

int send_eoi(u_int32_t irq)
{
  physdev_eoi_t op = { .irq = irq };
  return HYPERVISOR_physdev_op(PHYSDEVOP_eoi, &op); 
}

#ifdef SPLIT_PRIVILEGED
int do_domctl_op(xen_domctl_t *op)
{
  return _hypercall1(int, domctl, op);
}

int do_sysctl_op(xen_sysctl_t *op)
{
  return _hypercall1(int, sysctl, op);
}

int do_platform_op(xen_platform_op_t *op)
{
  return _hypercall1(int, platform_op, op);
}

#else
int do_dom0_operation(dom0_op_t *op)
{
  return _hypercall1(int, dom0_op, op);
}
#endif

int update_va_mapping_otherdomain(unsigned long va, unsigned long new_val, 
				  unsigned long flags, domid_t domid)
{
#ifdef CONFIG_X86_PAE
  pte_t pte = { .pte_high = 0, .pte_low = new_val };
#else
# if defined(__x86_64__)
  pte_t pte = { .pte = new_val };
# else
  pte_t pte = { .pte_low = new_val };
# endif
#endif
  return HYPERVISOR_update_va_mapping_otherdomain(va, pte, flags, domid);
}

int populate_physmap(domid_t domid, unsigned long num_pages, void *ptr)
{
  int err;
  struct xen_memory_reservation reservation;

  set_xen_guest_handle(reservation.extent_start, ptr);
  // reservation.extent_start = ptr;
  reservation.nr_extents   = num_pages;
  reservation.extent_order = 0;
#if __XEN_INTERFACE_VERSION__ >= 0x00030209
  reservation.mem_flags    = 0;
#else
  reservation.address_bits = 0;
#endif
  reservation.domid        = domid;

  err = HYPERVISOR_memory_op(XENMEM_populate_physmap, &reservation);
  if ( err == num_pages )
    return 0;
  return err;
}

int get_my_domid()
{
#ifdef SPLIT_PRIVILEGED
  // This is a pretty repulsive, hackish way of doing this, but it seems
  // the only way to do it in Xen 3.1+
  static int saved_res = -1;
  xen_domctl_t dco;
  typeof(dco.u.getdomaininfo.shared_info_frame) myframeno;
  domid_t i = DOMID_SELF; /* This is a shortcut, for when it works */
  int res = 0;

  if(saved_res >= 0)
    return saved_res;

  myframeno = start_info->shared_info >> PAGE_SHIFT;
  do {
    bzero(&dco, sizeof(dco));
    dco.cmd = XEN_DOMCTL_getdomaininfo;
    dco.interface_version = XEN_DOMCTL_INTERFACE_VERSION;
    dco.domain = i;
    res = do_domctl_op(&dco);
    if(res == 0) {
      /* See if this is us by checking to see if our shared_info machine
       * frame is the same as the one identified. */
      if(dco.u.getdomaininfo.shared_info_frame == myframeno) {
        saved_res = dco.u.getdomaininfo.domain;
        return dco.u.getdomaininfo.domain;
      }
    }
    
    i += 1;
  } while(i != DOMID_SELF);

  return -ESRCH;
#else
  dom0_op_t dzo;
  int res;

  bzero(&dzo, sizeof(dzo));
  dzo.cmd = DOM0_GETDOMAININFO;
  dzo.interface_version = DOM0_INTERFACE_VERSION;
  dzo.u.getdomaininfo.domain = DOMID_SELF;
  res = do_dom0_operation(&dzo);
  if(res == 0)
    return dzo.u.getdomaininfo.domain;
  else
    return -res;
#endif
}

