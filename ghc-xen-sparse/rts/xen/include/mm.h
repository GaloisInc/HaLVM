// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND

#ifndef _MM_H_
#define _MM_H_

#include <xen/xen.h>

#define PAGE_SHIFT			    12
#define PAGE_SIZE			    (1UL << PAGE_SHIFT)
#define PAGE_MASK			    (~(PAGE_SIZE - 1))
#define PAGE_OFFMASK(x)			(((uint64_t)x) & ~((0UL - 1) << PAGE_SHIFT))
#define PAGE_ALIGNED(x)			!PAGE_OFFMASK(x)
#define ADDRESS_SPACE_SIZE		(1ULL << 32)
#define round2page(x)           (((x) + (PAGE_SIZE - 1)) & PAGE_MASK)

#if defined(__x86_64__)
typedef uint64_t mfn_t; // machine frame number
typedef uint64_t pfn_t; // pseudo-physical frame number
#else
typedef uint32_t mfn_t;
typedef uint32_t pfn_t; // pseudo-physical frame number
#endif
#if defined(CONFIG_X86_PAE) || defined(__x86_64__)
typedef uint64_t maddr_t; // machine address
#else
typedef uint32_t maddr_t;
#endif
typedef uint32_t paddr_t; // pseudo-physical address
typedef void *vaddr_t;    // virtual address
typedef unsigned long vaddr_num_t; // virtual address as a number

void    init_mm(void);
mfn_t   get_free_machine_frame(void);
mfn_t   pfn_to_mfn(pfn_t);
vaddr_t alloc_page(void);
void    free_page(vaddr_t);
vaddr_t map_frames(domid_t, mfn_t*, size_t);
vaddr_t map_readonly_frames(domid_t, mfn_t*, size_t);
vaddr_t claim_vspace(vaddr_t, size_t length);
void    disclaim_vspace(vaddr_t, vaddr_t);
maddr_t virtual_to_machine(vaddr_t);
vaddr_t machine_to_virtual(maddr_t);
int     address_mapped(vaddr_t);
int     mark_as_page_table(int, vaddr_t, domid_t);
int     mark_as_page_table_mfn(int, mfn_t, domid_t);
int     set_page_writable(vaddr_t, int, domid_t);
void    replace_mfn(mfn_t);
void    unback_pages(vaddr_t, vaddr_t, int);
int     back_pages(vaddr_t, vaddr_t, int);
vaddr_t find_unmapped_vspace(size_t);
int     get_page_protection(vaddr_t);
void    system_wmb(void);
void    system_rmb(void);
void    system_mb(void);

extern uint64_t current_memory_reservation; /* In PAGES */

#endif
