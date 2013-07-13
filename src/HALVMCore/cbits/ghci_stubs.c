// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
// 
// These will never be called, or if they are, someone's done something very
// silly in their template haskell macro. So having bad types should be OK.
//
void *hypercall_page;
void *start_info;
void set_port_handler(void) {}
void unset_port_handler(void) {}
void evtchn_alloc_unbound(void) {}
void evtchn_bind_interdomain() {}
void evtchn_close() {}
void evtchn_send() {}
void mask_evtchn() {}
void unmask_evtchn() {}
void emergency_console_msg() {}
void gnttab_grant_copy() {}
void gnttab_grant_access() {}
void gnttab_end_access() {}
void gnttab_grant_foreign_transfer_ref() {}
void gnttab_reset_foreign_transfer_ref() {}
void gnttab_finish_foreign_transfer_ref() {}
void gnttab_transfer_page_to_dom() {}
void gnttab_address_of() {}
void gnttab_map_grant_ref() {}
void gnttab_unmap_grant_ref() {}
void claim_vspace() {}
void disclaim_vspace() {}
void alloc_page() {}
void free_page() {}
void virtual_to_machine() {}
void machine_to_virtual() {}
void address_mapped() {}
void mark_as_page_table() {}
void mark_as_page_table_mfn() {}
void set_page_writable() {}
void map_frames() {}
void bind_virq() {}
void unback_pages() {}
void system_wmb() {}
void system_rmb() {}
void system_mb() {}
void bind_pirq() {}
void map_readonly_frames() {}
void do_exit() {}
void irq_send_eoi() {}
void irq_get_status() {}
