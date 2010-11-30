// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef __GNTTAB_H__
#define __GNTTAB_H__

#include <types.h>
#include <xen/xen.h>
#include <xen/grant_table.h>

#define NR_GRANT_FRAMES 4
#define NR_GRANT_ENTRIES (NR_GRANT_FRAMES * PAGE_SIZE / sizeof(grant_entry_t))

void init_gnttab(void);
void gnttab_grant_access(grant_ref_t ref, domid_t domid, void *vaddr,int writable);
int gnttab_end_access(grant_ref_t ref);
void *gnttab_address_of(grant_ref_t ref);
s32 gnttab_grant_foreign_transfer_ref(grant_ref_t ref, domid_t domid);
s32 gnttab_reset_foreign_transfer_ref(grant_ref_t ref);
void *gnttab_finish_foreign_transfer_ref(grant_ref_t ref);
int gnttab_transfer_page_to_dom(void *page, domid_t dom, grant_ref_t ref);
void *gnttab_address_of(grant_ref_t ref);
grant_handle_t gnttab_map_grant_ref(void *host_addr,domid_t dom,grant_ref_t ref,int writable); 
s32 gnttab_unmap_grant_ref(void *p, grant_handle_t handle);
int gnttab_grant_copy(unsigned long src, 
		      unsigned long src_is_ref, 
		      domid_t src_dom,
		      uint16_t src_off,
		      unsigned long dest, 
		      unsigned long dest_is_ref, 
		      domid_t dest_dom,
		      uint16_t dest_off,
		      uint16_t len);

#endif /* !__GNTTAB_H__ */
