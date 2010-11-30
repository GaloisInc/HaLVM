// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND

typedef uint32_t pt_entry_t;

#define NUM(x) ((uint32_t)x)

// We're going to arrange memory so that the page tables exist in
// the range given between (HYPERVISOR_VIRT_START-4M) and 
// HYPERVISOR_VIRT_START. Or, as of 4/9/08, between 0xFBC00000 and
// 0xFC000000.
//
// The page tables should be arranged in order, with gaps, so that
// it is a trivial computation from a virtual address to the 
// address of the given page table.

static pt_entry_t    *page_dir = NULL;
static void          *page_dir_phys = NULL;
static unsigned char  clearing_space[4096] __attribute__ ((aligned (4096) ));
static void          *page_tables_start = NULL;

#define ptable_for_vaddr(x) (page_tables_start + (4096 * VADDR_TO_PD_INDEX(x)))

int address_mapped(vaddr_t ptr)
{
  unsigned long pd_index = VADDR_TO_PD_INDEX(ptr);

  if(ENTRY_PRESENT(page_dir[pd_index])) {
    unsigned long *ptable = ptable_for_vaddr(ptr);

    return ENTRY_PRESENT(ptable[VADDR_TO_PT_INDEX(ptr)]);
  }

  return 0;
}

maddr_t virtual_to_machine(vaddr_t ptr)
{
  unsigned long pd_index = VADDR_TO_PD_INDEX(ptr);

  if(ENTRY_PRESENT(page_dir[pd_index])) {
    unsigned long *ptable = ptable_for_vaddr(ptr);
    unsigned long entry;
   
    entry = ptable[VADDR_TO_PT_INDEX(ptr)];
    if(ENTRY_PRESENT(entry)) {
      return (void*)((entry & 0xFFFFF000) | (NUM(ptr) & 0x00000FFF));
    }

    return NULL;
  }

  return NULL;
}

vaddr_t machine_to_virtual(maddr_t ptr)
{
  // This is slow. Sorry. Not too sorry, though.
  unsigned long aligned_ptr = NUM(ptr) & 0xFFFFF000;
  int i,j;

  for(i = 0; i < VADDR_TO_PD_INDEX(HYPERVISOR_VIRT_START); i++)
    if(ENTRY_PRESENT(page_dir[i])) {
      unsigned long *ptable = page_tables_start + (i << 12);
      for(j = 0; j < 1024; j++)
        if(ENTRY_PRESENT(ptable[j]))
          if( (ptable[j] & 0xFFFFF000) == aligned_ptr ) 
            return (void*)((i << 22) + (j << 12) + (NUM(ptr) & 0xFFF)); 
    }

  return NULL;
}

static pt_entry_t pte_phys_address(vaddr_t ptr)
{
  unsigned long pd_index = VADDR_TO_PD_INDEX(ptr);
  unsigned long pt_index = VADDR_TO_PT_INDEX(ptr);
  return (page_dir[pd_index] & 0xFFFFF000) + (4 * pt_index);
}

static pt_entry_t *pte_virt_address(vaddr_t ptr)
{
  unsigned long *ptable = ptable_for_vaddr(ptr);
  return &(ptable[VADDR_TO_PT_INDEX(ptr)]);
}

static inline void pin_table(mfn_t mfn, int level)
{
  mmuext_op_t pin_request;
  pin_request.cmd = level;
  pin_request.arg1.mfn = mfn;
  assert(HYPERVISOR_mmuext_op(&pin_request, 1, NULL, DOMID_SELF) >= 0);
}

static inline void clear_mfn(mfn_t mfn)
{
  mmu_update_t update;
  unsigned long cs_ptable;
 
  // Map the MFN
  cs_ptable = page_dir[VADDR_TO_PD_INDEX(clearing_space)] & 0xFFFFF000;
  update.ptr = cs_ptable + (4 * VADDR_TO_PT_INDEX(clearing_space));
  update.val = (mfn << PAGE_SHIFT) | PD_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
  // Clear it
  bzero(clearing_space, 4096);
  // Unmap it.
  update.ptr = cs_ptable + (4 * VADDR_TO_PT_INDEX(clearing_space));
  update.val = 0;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static inline void inject_new_pdir_entry(int num, mfn_t mfn)
{
  mmu_update_t update;

  update.ptr = NUM(page_dir_phys) + (num * 4);
  update.val = (mfn << PAGE_SHIFT) | PD_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static inline void map_new_ptable(vaddr_t for_addr, mfn_t mfn)
{
  mmu_update_t update;

  update.ptr = (page_dir[VADDR_TO_PD_INDEX(for_addr)] & 0xFFFFF000) +
               (4 * VADDR_TO_PT_INDEX(for_addr));
  update.val = (mfn << PAGE_SHIFT) | PT_BASE_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static void force_page_table_for_vaddr(vaddr_t ptr)
{
  if(!ENTRY_PRESENT(page_dir[VADDR_TO_PD_INDEX(ptr)])) {
    unsigned long new_mfn = get_free_machine_frame();

    clear_mfn(new_mfn);
    pin_table(new_mfn, MMUEXT_PIN_L1_TABLE);
    inject_new_pdir_entry(VADDR_TO_PD_INDEX(ptr), new_mfn);
    map_new_ptable(ptable_for_vaddr(ptr), new_mfn);
  }
}

static void unmap_page(vaddr_t ptr)
{
  unsigned long ptable = page_dir[VADDR_TO_PD_INDEX(ptr)] & 0xFFFFF000;
  mmu_update_t update;

  update.ptr = ptable + (4 * VADDR_TO_PT_INDEX(ptr));
  update.val = 0;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

#define text_page(x) ((x >= text_start) && (x <= text_end))
#define ptable_page(x) ((x >= page_tables_start) &&  \
                        (x <= (page_tables_start + (4 * 1024 * 1024))))
#define important_page(x) (text_page(x) || ptable_page(x))

static void initialize_mm_state(void)
{
  unsigned long ptable_space_mfn = 0, pfn;
  int i, j;

  page_dir = (unsigned long*)start_info->pt_base;
  pfn = (NUM(page_dir) - NUM(text_start)) >> PAGE_SHIFT;
  page_dir_phys = (void*)(phys_to_machine_mapping[pfn] << PAGE_SHIFT);
  
  // Update text_end and set up the vaddress engine.
  text_end = (void*)(start_info->pt_base + 4096);
  claim_vspace(text_start, text_end - text_start);
  claim_vspace((void*)HYPERVISOR_VIRT_START, ((~0)-HYPERVISOR_VIRT_START) + 1);

  // Find the MFN of the page behind clearing_space, save it, and unmap it.
  pfn = (NUM(clearing_space) - NUM(text_start)) >> PAGE_SHIFT;
  ptable_space_mfn = phys_to_machine_mapping[pfn];
  unmap_page(clearing_space);

  // Find a place to put the page table space.
  for(i = (HYPERVISOR_VIRT_START >> 22); i >= 0; i--)
    if(!ENTRY_PRESENT(page_dir[i])) {
      page_tables_start = (void*)(i << 22);
      claim_vspace(page_tables_start, 4 * 1024 * 1024);
      clear_mfn(ptable_space_mfn);
      pin_table(ptable_space_mfn, MMUEXT_PIN_L1_TABLE);
      inject_new_pdir_entry(i, ptable_space_mfn);
      map_new_ptable(page_tables_start + (i * 4096), ptable_space_mfn);
      break;
    }

  // Remap all available page tables into this space.
  for(i = 0; i < (int)(HYPERVISOR_VIRT_START >> 22); i++)
    if(ENTRY_PRESENT(page_dir[i]) && (i != (int)(NUM(page_tables_start)>>22))) {
      unsigned long mfn = page_dir[i] >> PAGE_SHIFT;
      unsigned long pfn = machine_to_phys_mapping[mfn];
      assert(!(page_dir[i] & PDE_BIGPAGE));
      map_new_ptable((void*)page_tables_start + (i * 4096), mfn);
      unmap_page((void*)(NUM(text_start) + (pfn << PAGE_SHIFT)));
    }

  // For each page mapped, either (a) mark it as used if it's important, or
  // (b) unmap it if it's not.
  for(i = 0; i < (int)(HYPERVISOR_VIRT_START >> 22); i++)
    if(ENTRY_PRESENT(page_dir[i])) {
      unsigned long *ptable = (unsigned long*)(page_tables_start + (i * 4096));

      // We don't call set_pframe_used for the data in the page directory 
      // because it will be set later when we hit the page table where we've
      // mapped our page tables.
      for(j = 0; j < 1024; j++)
        if(ENTRY_PRESENT(ptable[j])) {
          void *page = (void*)((i << 22) + (j << 12));

          if( important_page(page) ) {
            unsigned long pfn = machine_to_phys_mapping[ptable[j]>>PAGE_SHIFT];
            set_pframe_used(pfn);
          } else {
            unmap_page(page);
          }
        }
    }
}
