// derived from mini-os

/* 
 ****************************************************************************
 * (C) 2006 - Cambridge University
 ****************************************************************************
 *
 *        File: gnttab.c
 *      Author: Steven Smith (sos22@cam.ac.uk) 
 *     Changes: Grzegorz Milos (gm281@cam.ac.uk)
 *              
 *        Date: July 2006
 * 
 * Environment: Xen Minimal OS
 * Description: Simple grant tables implementation. 
 *
 ****************************************************************************
 */
#include <gnttab.h>
#include <hypercall.h>
#include <mm.h>
#include <stdlib.h>
#include <arch.h>
#include <xen/memory.h>
#include <stdio.h>

#define NULL 0

static grant_entry_t *gnttab_table;

void init_gnttab(void)
{
    struct gnttab_setup_table setup;
    mfn_t frames[NR_GRANT_FRAMES];

    setup.dom        = DOMID_SELF;
    setup.nr_frames  = NR_GRANT_FRAMES;
    setup.frame_list = (unsigned long*)frames;

    HYPERVISOR_grant_table_op(GNTTABOP_setup_table, &setup, 1);
    gnttab_table = map_frames(DOMID_SELF, frames, NR_GRANT_FRAMES);
}

void gnttab_grant_access(grant_ref_t ref, domid_t domid, void *vaddr, 
                         int writable)
{
  maddr_t maddr = virtual_to_machine(vaddr);
  mfn_t mfn = maddr >> PAGE_SHIFT;
  gnttab_table[ref].frame = mfn;
  gnttab_table[ref].domid = domid;
  wmb();
  gnttab_table[ref].flags = GTF_permit_access | (writable? 0 : GTF_readonly);
}


int gnttab_end_access(grant_ref_t ref)
{
    u16 flags, nflags;

    nflags = gnttab_table[ref].flags;
    do {
        if ((flags = nflags) & (GTF_reading|GTF_writing)) {
            printf("GT: WARNING: g.e. still in use!\n");
            return 0;
        }
    } while ((nflags = synch_cmpxchg(&gnttab_table[ref].flags, flags, 0)) !=
            flags);

    return 1;
}

void *gnttab_address_of(grant_ref_t ref)
{
  mfn_t mfn = gnttab_table[ref].frame;
  maddr_t maddr = ((maddr_t)mfn) << PAGE_SHIFT;
  return machine_to_virtual(maddr);
}

extern start_info_t *start_info;

grant_handle_t gnttab_map_grant_ref(void *host_addr, 
                                    domid_t dom,
                                    grant_ref_t ref,
                                    int writable) 
{
    struct gnttab_map_grant_ref map;
    int res;
#ifdef GNTMAP_application_map
    uint32_t flags = GNTMAP_host_map | GNTMAP_application_map | (writable ? 0 : GNTMAP_readonly);
#else
    uint32_t flags = GNTMAP_host_map | (writable? 0 : GNTMAP_readonly);
#endif

    map.host_addr = (maddr_t)((vaddr_num_t)host_addr);
    map.flags = flags;
    map.ref = ref;
    map.dom = dom;
    res = HYPERVISOR_grant_table_op(GNTTABOP_map_grant_ref, &map, 1);
    if (map.status < 0)
      return map.status;
    else 
      return map.handle;
}

s32 gnttab_unmap_grant_ref(void *host_addr, grant_handle_t handle) 
{
    gnttab_unmap_grant_ref_t unmap;
    int res;

    unmap.host_addr = (maddr_t)((vaddr_num_t) host_addr);
    unmap.dev_bus_addr = 0;
    unmap.handle = handle;

    res = HYPERVISOR_grant_table_op(GNTTABOP_unmap_grant_ref, &unmap, 1);
    if (res < 0)
      return res;
    else
      return unmap.status;
}

static s32 grant_foreign_transfer_ref(grant_ref_t ref, domid_t dom, 
		               		          pfn_t pframe, mfn_t mframe)
{
  xen_memory_reservation_t reservation;
  int res;

  /* Set up the grant table entry */
  gnttab_table[ref].frame = pframe;
  gnttab_table[ref].domid = dom;
  wmb();
  gnttab_table[ref].flags = GTF_accept_transfer;
  /* Free the physical frame so that we can accept the new one */
  reservation.extent_start = (unsigned long*)&mframe;
  reservation.nr_extents = 1;
  reservation.extent_order = 0;
  reservation.address_bits = 0;
  reservation.domid = DOMID_SELF;
  res = HYPERVISOR_memory_op(XENMEM_decrease_reservation, &reservation);
  return res;
}

s32 gnttab_grant_foreign_transfer_ref(grant_ref_t ref, domid_t dom)
{
  unsigned long mframe = get_free_machine_frame();
  unsigned long pframe = machine_to_phys_mapping[mframe];
  return grant_foreign_transfer_ref(ref, dom, pframe, mframe);
}

s32 gnttab_reset_foreign_transfer_ref(grant_ref_t ref)
{
  // Yank the domain information off immediately
  unsigned int dom = gnttab_table[ref].domid;

  if(!(gnttab_table[ref].flags & GTF_transfer_committed) && 
     !(gnttab_table[ref].flags & GTF_transfer_completed)) {
    // If we haven't even committed yet, invalidate the old entry
    gnttab_table[ref].flags = GTF_invalid;
    wmb();
    return gnttab_grant_foreign_transfer_ref(ref, dom);
  } else {
    pfn_t pframe;
    mfn_t mframe;
    maddr_t maddr;
    void *ptr;
    
    // Otherwise, spin until the transfer completes before resetting
    while(!(gnttab_table[ref].flags & GTF_transfer_completed)) {
      __asm__ __volatile__ ( "rep;nop" : : : "memory" );
    }
    // Alright, it's committed, so release the frame and any associated
    // pages
    mframe = gnttab_table[ref].frame;
    pframe = machine_to_phys_mapping[mframe];
    maddr = ((maddr_t)mframe << PAGE_SHIFT);
    ptr = machine_to_virtual(maddr);
    if(ptr) disclaim_vspace(ptr, ptr + PAGE_SIZE);
    return grant_foreign_transfer_ref(ref, dom, pframe, mframe);
  }
}

void *gnttab_finish_foreign_transfer_ref(grant_ref_t ref)
{
  unsigned int flags = gnttab_table[ref].flags;
  mfn_t frame;
  
  /* If the transfer hasn't even been committed, give up */
  if(!(flags & GTF_transfer_committed)) {
    gnttab_table[ref].flags = GTF_invalid;
    wmb();
    return NULL;
  }

  /* Busy wait until the transfer is complete */
  while(!(flags & GTF_transfer_completed)) {
    flags = gnttab_table[ref].flags;
    __asm__ __volatile__ ( "rep;nop" : : : "memory" );
  }
  rmb();
  frame = gnttab_table[ref].frame;
  return map_frames(DOMID_SELF, &frame, 1);
}

int gnttab_transfer_page_to_dom(void *page, domid_t dom, grant_ref_t ref)
{
  maddr_t mptr = virtual_to_machine(page);
  mfn_t mfn = mptr >> PAGE_SHIFT;
  pfn_t pfn = machine_to_phys_mapping[mfn];
  gnttab_transfer_t transfer;
  int res;
  
  transfer.mfn = mfn;
  transfer.domid = dom;
  transfer.ref = ref;
  transfer.status = 0;

  free_page(page);
  assert( !virtual_to_machine(page) );
  replace_mfn(mfn);
  assert( pfn_to_mfn(pfn) != mfn );
  res = HYPERVISOR_grant_table_op(GNTTABOP_transfer, &transfer, 1);

  if(res == 0)
    return transfer.status;
  else
    return res;
}

int gnttab_grant_copy(unsigned long src, 
		      unsigned long src_is_ref, 
		      domid_t src_dom,
		      uint16_t src_off,
		      unsigned long dest, 
		      unsigned long dest_is_ref, 
		      domid_t dest_dom,
		      uint16_t dest_off,
		      uint16_t len)
{
  gnttab_copy_t copy;
  int res;

  copy.source.domid = src_dom;
  copy.source.offset = src_off;
  copy.dest.domid = dest_dom;
  copy.dest.offset = dest_off;
  copy.len = len;
  copy.flags = 0;
  if(src_is_ref) {
    copy.source.u.ref = src;
    copy.flags += GNTCOPY_source_gref;
  } else copy.source.u.gmfn = src;
  if(dest_is_ref) {
    copy.dest.u.ref = dest;
    copy.flags += GNTCOPY_dest_gref;
  } else copy.dest.u.gmfn = dest;
  res = HYPERVISOR_grant_table_op(GNTTABOP_copy, &copy, 1);

  if(res == 0)
    return copy.status;
  else
    return res;
}
