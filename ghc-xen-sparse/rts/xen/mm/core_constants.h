// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND

#include <hypercall.h>

// Intel VM Constants
#define PTE_PRESENT         0x00000001
#define PTE_RW              0x00000002
#define PTE_USER            0x00000004
#define PTE_WRITETHROUGH    0x00000008
#define PTE_CACHEDISABLE    0x00000010
#define PTE_ACCESSED        0x00000020
#define PTE_DIRTY           0x00000040
#define PTE_PTATTR          0x00000080
#define PTE_GLOBAL          0x00000100
#define PTE_CLAIMED         0x00000200 // Avail bit, used for claiming vspace
#define PDE_BIGPAGE         0x00000080

#if defined(__x86_64__) || defined(CONFIG_X86_PAE)
# define L4E_NX              0x8000000000000000ULL
#endif

#define PT_BASE_PROT        (PTE_ACCESSED | PTE_USER | PTE_PRESENT)
#define PD_PROT             (PTE_PRESENT | PTE_RW | PTE_ACCESSED | PTE_USER)
#define PAGE_SHIFT 12

#if defined(__x86_64__)
# define MFN_BITS               36
# define VADDR_TO_L4_INDEX(x)   ((((vaddr_num_t)x) >> 39) & 511)
# define VADDR_TO_L3_INDEX(x)   ((((vaddr_num_t)x) >> 30) & 511)
# define VADDR_TO_PD_INDEX(x)   ((((vaddr_num_t)x) >> 21) & 511)
# define VADDR_TO_PT_INDEX(x)   ((((vaddr_num_t)x) >> PAGE_SHIFT) & 511)
# define L4_PROT                (PD_PROT | L4E_NX)
# define L3_PROT                PD_PROT
# define MASK9(x)               (((vaddr_num_t)x) & 511)
# define BUILD_ADDR(a,b,c,d,e)  ((vaddr_t)((MASK9(a) << 39) | \
                                           (MASK9(b) << 30) | \
                                           (MASK9(c) << 21) | \
                                           (MASK9(d) << 12) | \
                                           (e & 0xfff)))
#elif defined(CONFIG_X86_PAE)
# define MFN_BITS               20
# define VADDR_TO_L3_INDEX(x)   ((((vaddr_num_t)x) >> 30) & 3)
# define VADDR_TO_PD_INDEX(x)   ((((vaddr_num_t)x) >> 21) & 511)
# define VADDR_TO_L3PD_INDEX(x) (((vaddr_num_t)x) >> 21)
# define VADDR_TO_PT_INDEX(x)   ((((vaddr_num_t)x) >> PAGE_SHIFT) & 511)
# define L3_PROT                PTE_PRESENT
#else
# define MFN_BITS               20
# define VADDR_TO_PD_INDEX(x)   (((vaddr_num_t)x) >> 22)
# define VADDR_TO_PT_INDEX(x)   ((((vaddr_num_t)x) >> PAGE_SHIFT) & 1023)
#endif

#define VADDR_TO_PAGE_INDEX(x)  (((vaddr_num_t)x) & (PAGE_SIZE - 1))

#define MFN_MASK                ((((pt_entry_t)1) << MFN_BITS) - 1)
#define MADDR_MASK              (((pt_entry_t)(MFN_MASK)) << PAGE_SHIFT)
#define ENTRY_MADDR(x)          (((maddr_t)x) & MADDR_MASK)
#define ENTRY_MFN(x)            ((((maddr_t)x) >> PAGE_SHIFT) & MFN_MASK)
#define ENTRY_FLAGS(x)          (((vaddr_num_t)x) & ~MADDR_MASK)
#define ENTRY_PRESENT(x)        (((maddr_t)x) & PTE_PRESENT)
#define ENTRY_READWRITE(x)      (((maddr_t)x) & PTE_RW)
#define ENTRY_CLAIMED(x)        (((maddr_t)x) & PTE_CLAIMED)

#define PT_LOOKUP(table,ix)     (((maddr_t)(table)) + (ix) * sizeof(pt_entry_t))

