// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <string.h>

typedef uint64_t pt_entry_t;

static pt_entry_t *l4_table             = NULL;
static maddr_t     l4_base              = NULL;

// static pt_entry_t  temp_tab[512]      __attribute__ ((aligned(PAGE_SIZE)));
// static pt_entry_t  clear_block[512]   __attribute__ ((aligned(PAGE_SIZE)));

static volatile pt_entry_t *temp_tab             = NULL;
static pt_entry_t* clear_block;

static maddr_t     temp_tab_pt_entry    = NULL;
static maddr_t     clear_block_pt_entry = NULL;


// Update an entry in a page table.
// The arguments are:
//    physical address of the page table
//    offset in the page table
//    the new value for the entry.
static void bind_page_table(maddr_t base, int entry_off, pt_entry_t val)
{
  mmu_update_t update;

  update.ptr = PT_LOOKUP(base & PAGE_MASK, entry_off);
  update.val = val;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}



// Associate the virtual space pointed to by "temp_tab"
// with the provided physical address.  The physical address should
// be that of a memory area containing a page table.
// NOTE: Modifies temp_table.
static void temporarily_map_page_table(maddr_t ptr)
{
  mmu_update_t update;
  mmuext_op_t flushreq;

  // flush the old TLB entry.
  flushreq.cmd = MMUEXT_INVLPG_LOCAL;
  flushreq.arg1.linear_addr = (vaddr_num_t)temp_tab;
  assert(HYPERVISOR_mmuext_op(&flushreq, 1, NULL, DOMID_SELF) >= 0);

  // and then map the table.
  update.ptr = temp_tab_pt_entry;
  update.val = (ptr & PAGE_MASK) | PT_BASE_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}



// Given a virtual address, compute the physical address
// of its L1 page table entry.
// If there is no corresponding L1 table entry,
// we return 1, which is not sizeOf(pt_entry_t) aligned,
// and so is not a valid adderess.
// NOTE: Modifies temp_table.
static maddr_t pte_phys_address(vaddr_t ptr)
{
  pt_entry_t entry;

  entry = l4_table[VADDR_TO_L4_INDEX(ptr)];
  if (ENTRY_PRESENT(entry)) {
    temporarily_map_page_table(ENTRY_MADDR(entry));
    entry = temp_tab[VADDR_TO_L3_INDEX(ptr)];
    if (ENTRY_PRESENT(entry)) {
      temporarily_map_page_table(ENTRY_MADDR(entry));
      entry = temp_tab[VADDR_TO_PD_INDEX(ptr)];
      if (ENTRY_PRESENT(entry)) {
        return PT_LOOKUP(ENTRY_MADDR(entry), VADDR_TO_PT_INDEX(ptr));
      }
    }
  }
  return 1;  // Not sizeOf(entry_t) aligned.
}


// Given a virtual address, map its corresponding (L1) page table
// to the addres pointed by "temp_tab".
// On success, returns a pointer to the L1 pagetable entry
//                                            corresponding to the address.
// XXX: This interface is odd. We always dereference the pointer,
//      so why not just return the entry?
// On failure, returns 0.
// NOTE: Modifies temp_table.
static pt_entry_t pte_virt_address(vaddr_t ptr) {
  maddr_t addr = pte_phys_address(ptr);
  if (addr == 1) return -1;

  temporarily_map_page_table(addr);
  return *(temp_tab + VADDR_TO_PT_INDEX(ptr));
}


// Given a virtual address, map its corresponding (L1) page table
// to the addres pointed by "temp_tab".
// On success, returns the L1 pagetable entry corresponding to the address.
// On failure, returns 0.
// NOTE: Modifies temp_table.
static pt_entry_t map_page_table_for(vaddr_t ptr) {
  pt_entry_t entry = pte_virt_address(ptr);
  return entry == (-1) ? 0 : entry;
}


// Use the page tables to lookup the physical address for a virtual address.
// On error, returns 0.
// NOTE: Modifies temp_table.
// XXX: Perhaps we should return some other unlikely physical address on error?
maddr_t virtual_to_machine(vaddr_t ptr)
{
  pt_entry_t entry = map_page_table_for(ptr);
  return ENTRY_PRESENT(entry) ? ENTRY_MADDR(entry) + VADDR_TO_PAGE_INDEX(ptr)
                              : 0;
}


// Check if we have a mapping for a virtual address.
// NOTE: Modifies temp_table.
int address_mapped(vaddr_t ptr)
{
  return virtual_to_machine(ptr) != 0;
}


// Search the page tables for the first occurance of a machine address.
// Returns the corresponding virtual address, or 0, if not found.
// XXX: Technically 0 could be a valid answer!
// NOTE: Modifies temp_table.
vaddr_t machine_to_virtual(maddr_t mptr)
{
  int i, j, k, l;
  mptr &= PAGE_MASK;

  for(i = 0; i < 512; i++) {
    // XXX: Hopefully, we are not skipping too much here?
    if (i == VADDR_TO_L4_INDEX(HYPERVISOR_VIRT_START)) {
      i = VADDR_TO_L4_INDEX(HYPERVISOR_VIRT_END);
    }
    if(ENTRY_PRESENT(l4_table[i])) {
      temporarily_map_page_table(ENTRY_MADDR(l4_table[i]));
      for(j = 0; j < 512; j++) {
        if(ENTRY_PRESENT(temp_tab[j])) {
          maddr_t l2_addr = ENTRY_MADDR(temp_tab[j]);
          temporarily_map_page_table(l2_addr);
          for(k = 0; k < 512; k++) {
            if(ENTRY_PRESENT(temp_tab[k])) {
              // Map and search L1 table
              temporarily_map_page_table(ENTRY_MADDR(temp_tab[k]));
              for(l = 0; l < 512; l++) {
                if(ENTRY_PRESENT(temp_tab[l])) {
                  if(mptr == ENTRY_MADDR(temp_tab[l])) {
                    return BUILD_ADDR(i, j, k, l, mptr);
                  }
                }
              }
              // Restore L2 table
              temporarily_map_page_table(l2_addr);
            }
          }
          // Restore L3 table
          temporarily_map_page_table(ENTRY_MADDR(l4_table[i]));
        }
      }
    }
  }
  return 0;
}


// Check if a virtual address has been claimed.
// This means that either the address is mapped to a physical page,
// or we have marked it as "claimed" (see PTE_CLAIMED), explicitly.

// NOTE: Modifies temp_table.
static int address_claimed(void *ptr)
{
  pt_entry_t entry = map_page_table_for(ptr);
  return ENTRY_PRESENT(entry) || ENTRY_CLAIMED(entry);
}


// Allocate a new page table at the given level.
// Returns an entry suitable for inserting into the level above.
static pt_entry_t generate_new_page_table(int level)
{
  mfn_t newmfn = get_free_machine_frame();
  mmu_update_t update;
  mmuext_op_t extreq;
  pt_entry_t perms = 0;

  // flush the TLB entry for the clearing region.
  extreq.cmd = MMUEXT_INVLPG_LOCAL;
  extreq.arg1.linear_addr = (vaddr_num_t)clear_block;
  assert(HYPERVISOR_mmuext_op(&extreq, 1, NULL, DOMID_SELF) >= 0);
  // map the page in our clearing region.
  assert(newmfn);
  update.ptr = clear_block_pt_entry;
  update.val = (newmfn << PAGE_SHIFT) | PT_BASE_PROT | PTE_RW;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
  // clear it
  bzero(clear_block, PAGE_SIZE);
  // unmap it
  update.ptr = clear_block_pt_entry;
  update.val = 0; // PTE_CLAIMED;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
  // pin it as a page table.
  extreq.cmd = level;
  extreq.arg1.mfn = newmfn;
  assert(HYPERVISOR_mmuext_op(&extreq, 1, NULL, DOMID_SELF) >= 0);

  // NOTE: The entry is in the table _above_ us.
  switch (level) {
    case MMUEXT_PIN_L3_TABLE: perms = L4_PROT; break;
    case MMUEXT_PIN_L2_TABLE: perms = L3_PROT; break;
    case MMUEXT_PIN_L1_TABLE: perms = PD_PROT; break;
  }

  // return (newmfn << PAGE_SHIFT) | PT_BASE_PROT | L4E_NX;
  return (((pt_entry_t)newmfn) << PAGE_SHIFT) | perms;
}



// Ensure that the page tables that are needed to map the given
// virtual address are present.  If some of the page tables are
// missing, then we allocate them.
static void force_page_table_for_vaddr(vaddr_t ptr)
{
  maddr_t entry_base  = l4_base;
  pt_entry_t entry    = l4_table[VADDR_TO_L4_INDEX(ptr)];

  // Is there an entry in the L4 table for this address?
  if(!ENTRY_PRESENT(entry)) {
    entry = generate_new_page_table(MMUEXT_PIN_L3_TABLE);
    bind_page_table(entry_base, VADDR_TO_L4_INDEX(ptr), entry);
  }

  // There is now. So map the L3 table, and then find out if there's a page
  // directory for the address.
  temporarily_map_page_table(ENTRY_MADDR(entry));
  entry_base = ENTRY_MADDR(entry);
  entry      = temp_tab[VADDR_TO_L3_INDEX(ptr)];
  if(!ENTRY_PRESENT(entry)) {
    entry = generate_new_page_table(MMUEXT_PIN_L2_TABLE);
    bind_page_table(entry_base, VADDR_TO_L3_INDEX(ptr), entry);
  }

  // There is now a page directory for the address. So see if there's a
  // page table.
  temporarily_map_page_table(ENTRY_MADDR(entry));
  entry_base = ENTRY_MADDR(entry);
  entry      = temp_tab[VADDR_TO_PD_INDEX(ptr)];
  if(!ENTRY_PRESENT(entry)) {
    entry = generate_new_page_table(MMUEXT_PIN_L1_TABLE);
    bind_page_table(entry_base, VADDR_TO_PD_INDEX(ptr), entry);
  }
}

// Compute the pseudo-physical page containing a particular virtual address.
static inline pfn_t vaddr_to_pfn(vaddr_t ptr) {
  return ((unsigned long)ptr - (unsigned long)text_start) >> PAGE_SHIFT;

}

// Given a virtual address, compute the physical address
// of the page table entry that maps it.
// NOTE: This is similar to "pte_phys_address" but instead of
// looking addresses up in page tables, we assume the initial
// linear mapping setup by the domain builder.
static inline maddr_t pre_initialization_get_pte(vaddr_t ptr)
{
  pt_entry_t *table = l4_table;
  pt_entry_t entry;
  mfn_t mfn;
  pfn_t pfn;

  // Because this is pre-initialization, we trust that Xen has already
  // got our page tables mapped in its preferred way. Thus, we can use
  // some somewhat scary math to convert machine->virtual data.
  entry = table[VADDR_TO_L4_INDEX(ptr)];
  mfn   = ENTRY_MFN(entry);
  pfn   = machine_to_phys_mapping[mfn];
  table = (pt_entry_t*)((unsigned long)text_start + 
			(((maddr_t)pfn) << PAGE_SHIFT));    // 3

  entry = table[VADDR_TO_L3_INDEX(ptr)];
  mfn   = ENTRY_MFN(entry);
  pfn   = machine_to_phys_mapping[mfn];
  table = (pt_entry_t*)((unsigned long)text_start + 
			(((maddr_t)pfn) << PAGE_SHIFT));    // 2

  entry = table[VADDR_TO_PD_INDEX(ptr)];

  return PT_LOOKUP(ENTRY_MADDR(entry), VADDR_TO_PT_INDEX(ptr));

}

/*
#define text_page(x) ((x >= text_start) && (x <= text_end))
#define xen_page(x)  ((x >= (vaddr_t)HYPERVISOR_VIRT_START) && \
                (x <= (vaddr_t)HYPERVISOR_VIRT_END))
#define vmm_page(x) ((x == l4_table)           || \
                (x == temp_tab)      || \
                (x == clear_block_addr))
*/
#define all_xen(a,b) \
  ((BUILD_ADDR(a,b,  0,0,0) >= (vaddr_t)HYPERVISOR_VIRT_START) && \
   (BUILD_ADDR(a,b+1,0,0,0) <= (vaddr_t)HYPERVISOR_VIRT_END))

#define important_page(x) (text_page(x) || xen_page(x) || vmm_page(x))

// Initialize what physical memory is already used by the HaLVM.
// Our memory allocator uses one bit (MFN_IN_USE_BIT) for each
// entry in the physical to machine map to indicate if that particular
// page is allocated.  Upon start this bit is unset.
// Here we traverse the pages that are used by the HaLLVM and mark them as used.
static void setup_used(void) {
  unsigned long the_end = start_info->pt_base
                        + PAGE_SIZE * start_info->nr_pt_frames;

  unsigned long plus_stack = the_end + 512 * 1024;
  unsigned long four_mb_mask = 4 * 1024 * 1024 - 1;
  unsigned long rounded_up = (plus_stack + four_mb_mask) & ~four_mb_mask;
  unsigned long start      = (unsigned long)text_start & PAGE_MASK;
  unsigned long num_bytes = rounded_up - start;
  unsigned long page_num = (num_bytes >> PAGE_SHIFT);

  pfn_t pfn;
  for (pfn = 0; pfn < page_num; ++pfn) {
    set_pframe_used(pfn);
  }

  claim_vspace((void*)start, num_bytes);

  for (pfn = page_num + 0x100; pfn < page_num + 0x200; ++pfn) {
    set_pframe_used(pfn);
  }
}


static void initialize_mm_state(void)
{
  pt_entry_t l3_table[512], l2_table[512], l1_table[512];
  int i, j, k, l;
  pfn_t pfn;
  // void *ptr;

  // Figure out where the root page table (the L4 table) is, in both virtual
  // and physical memory.
  l4_table  = (pt_entry_t*)start_info->pt_base;
  pfn       = vaddr_to_pfn(l4_table);
  l4_base   = ((maddr_t)phys_to_machine_mapping[pfn]) << PAGE_SHIFT;

  // This system assumes that there are at least four page table items
  // mapped by Xen, including the root page table. We'll use the first two
  // as the temporary table linear address and clearing zone linear address,
  // since they're handy and should necessarily have L4, L3, and L2 entries.
  assert(start_info->nr_pt_frames >= 3);

  temp_tab              = l4_table + 512;
  clear_block           = (pt_entry_t*)(temp_tab + 512);

  temp_tab_pt_entry     = pre_initialization_get_pte((vaddr_t)temp_tab);
  clear_block_pt_entry  = (maddr_t)pre_initialization_get_pte(clear_block);

  setup_used();
  // For each mapped page, either (a) mark it as used if it's important, or
  // (b) unmap it.
  for(i = 0; i < 256; i++) {
    if(ENTRY_PRESENT(l4_table[i])) {
      temporarily_map_page_table(ENTRY_MADDR(l4_table[i]));
      memcpy((void*)l3_table, (void*)temp_tab, sizeof(l3_table));

      for(j = 0; j < 512; j++) {
        if(ENTRY_PRESENT(l3_table[j]) && !all_xen(i,j) ) {
          temporarily_map_page_table(ENTRY_MADDR(l3_table[j]));
          memcpy((void*)l2_table, (void*)temp_tab, sizeof(l2_table));

          for(k = 0; k < 512; k++) {
            if(ENTRY_PRESENT(l2_table[k])) {
              temporarily_map_page_table(ENTRY_MADDR(l2_table[k]));
              memcpy((void*)l1_table, (void*)temp_tab, sizeof(l1_table));

              for(l = 0; l < 512; l++) {
                if(ENTRY_PRESENT(l1_table[l])) {
                    pfn_t fn = machine_to_phys_mapping[ENTRY_MFN(l1_table[l])];
                    assert(MFN_IN_USE(phys_to_machine_mapping[fn]));
                }
              }
            }
          }
        }
      }
    }
  }
  // wow that's a lot of close braces, but I think that's it!
}
