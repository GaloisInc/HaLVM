// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
typedef uint64_t pt_entry_t;

static pt_entry_t    *pdp_table = NULL;
// All of the page tables for this VM, in order. This spends some space
// in the binary image for convenience down the road.
static pt_entry_t     page_dir[2048] __attribute__ ((aligned (4096)));
static unsigned char  clearing_space[4096] __attribute__ ((aligned (4096) ));
static void          *page_tables_start = NULL;

#define PTENTRYP(x) ((pt_entry_t*)(x))
#define NUM(x) ((uintptr_t)(x))
#define ptable_for_vaddr(x) PTENTRYP(NUM(page_tables_start) + \
                                     (4096 * VADDR_TO_L3PD_INDEX(NUM(x))))
#define RM_PT_BITS(x) (x & 0xFFFFFFFFFFFFF000ULL)

int address_mapped(vaddr_t ptr)
{
  int pd_index = VADDR_TO_L3PD_INDEX(ptr);

  if(ENTRY_PRESENT(page_dir[pd_index])) {
    pt_entry_t *ptable = ptable_for_vaddr(ptr); 

    return ENTRY_PRESENT(ptable[VADDR_TO_PT_INDEX(ptr)]);
  }

  return 0;
}

maddr_t virtual_to_machine(vaddr_t ptr)
{
  int pd_index = VADDR_TO_L3PD_INDEX(ptr);

  if(ENTRY_PRESENT(page_dir[pd_index])) {
    pt_entry_t *ptable = ptable_for_vaddr(ptr);
    pt_entry_t entry;

    entry = ptable[VADDR_TO_PT_INDEX(ptr)];
    if(ENTRY_PRESENT(entry)) {
      maddr_t retval = RM_PT_BITS(entry);
      retval += ((vaddr_num_t)ptr) & 0xFFF;
      return retval;
    }
    return NULL;
  }
  return NULL;
}

vaddr_t machine_to_virtual(maddr_t mptr)
{
  maddr_t aligned_ptr = mptr & 0xFFFFFFFFFFFFF000ULL;
  uint32_t i, j;

  for(i = 0; i < (int)VADDR_TO_L3PD_INDEX(HYPERVISOR_VIRT_START); i++)
    if(ENTRY_PRESENT(page_dir[i])) {
      pt_entry_t *ptable = PTENTRYP(NUM(page_tables_start) + (i << 12));

      for(j = 0; j < 512; j++)
        if(ENTRY_PRESENT(ptable[j]))
          if( RM_PT_BITS(ptable[j]) == aligned_ptr )
            return (vaddr_t)((i << 21) + (j << 12) + (((uint32_t)mptr)&0xFFF));
            // NOTE: The above use of ((uint32_t)mptr) is safe because we don't
            // care about the high-order bits that may be lost.
    }

  return NULL;
}

static int address_claimed(void *ptr)
{
  int pd_index = VADDR_TO_L3PD_INDEX(ptr);

  if(ENTRY_PRESENT(page_dir[pd_index])) {
    pt_entry_t *ptable = ptable_for_vaddr(ptr);
    pt_entry_t entry   = ptable[VADDR_TO_PT_INDEX(ptr)];
    return ENTRY_PRESENT(entry) || ENTRY_CLAIMED(entry);
  } else {
    return ENTRY_CLAIMED(page_dir[pd_index]);
  }
  return NULL;
}

static pt_entry_t pte_phys_address(vaddr_t ptr)
{
  uint32_t pd_index = VADDR_TO_L3PD_INDEX(ptr);
  uint32_t pt_index = VADDR_TO_PT_INDEX(ptr);
  assert(ENTRY_PRESENT(page_dir[pd_index]));
  return RM_PT_BITS(page_dir[pd_index]) + (8 * pt_index);
  // NOTE: The use of uint32_t is safe here because (0 <= pt_index <= 512)
}

static pt_entry_t *pte_virt_address(vaddr_t ptr)
{
  pt_entry_t *ptable = ptable_for_vaddr(ptr);
  return &(ptable[VADDR_TO_PT_INDEX(ptr)]);
}

static void force_page_table_for_vaddr(vaddr_t);

static void clear_mfn(mfn_t mfn)
{
  mmu_update_t update = { 0, 0 };
  maddr_t cs_ptable;

  // Map the MFN
  cs_ptable = RM_PT_BITS(page_dir[VADDR_TO_L3PD_INDEX(clearing_space)]);
  update.ptr = cs_ptable + (8 * VADDR_TO_PT_INDEX(clearing_space));
  update.val = (((maddr_t)mfn) << PAGE_SHIFT) | PD_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
  // Clear it
  bzero(clearing_space, 4096);
  // Unmap it
  update.ptr = cs_ptable + (8 * VADDR_TO_PT_INDEX(clearing_space));
  update.val = 0;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static void pin_table(mfn_t mfn, int level)
{
  mmuext_op_t pin_request;
  pin_request.cmd = level;
  pin_request.arg1.mfn = mfn;
  assert(HYPERVISOR_mmuext_op(&pin_request, 1, NULL, DOMID_SELF) >= 0);
}

static void inject_new_pdir_entry(int num, mfn_t mfn)
{
  mmu_update_t update;

  update.ptr = RM_PT_BITS(pdp_table[num >> 9]) + ((num & 511) * 8);
  update.val = (((maddr_t)mfn) << PAGE_SHIFT) | PD_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static void map_new_ptable(vaddr_t for_addr, mfn_t mfn)
{
  mmu_update_t update;

  update.ptr = RM_PT_BITS(page_dir[VADDR_TO_L3PD_INDEX(for_addr)]) +
               (8 * VADDR_TO_PT_INDEX(for_addr));
  update.val = (((maddr_t)mfn) << PAGE_SHIFT) | PT_BASE_PROT;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static void unmap_page(vaddr_t ptr)
{
  pt_entry_t ptable = RM_PT_BITS(page_dir[VADDR_TO_L3PD_INDEX(ptr)]);
  mmu_update_t update;

  update.ptr = ptable + (8 * VADDR_TO_PT_INDEX(ptr));
  update.val = 0;
  assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
}

static void force_page_table_for_vaddr(vaddr_t ptr)
{
  if(!ENTRY_PRESENT(page_dir[VADDR_TO_L3PD_INDEX(ptr)])) {
    mfn_t new_mfn = get_free_machine_frame();

    clear_mfn(new_mfn);
    pin_table(new_mfn, MMUEXT_PIN_L1_TABLE);
    inject_new_pdir_entry(VADDR_TO_L3PD_INDEX(ptr), new_mfn);
    map_new_ptable(ptable_for_vaddr(ptr), new_mfn);
    assert(ENTRY_PRESENT(page_dir[VADDR_TO_L3PD_INDEX(ptr)]));
  }
}

#define text_page(x) ((x >= NUM(text_start)) && (x <= NUM(text_end)))
#define ptable_page(x) ((x >= NUM(page_tables_start)) &&  \
                        (x <= (NUM(page_tables_start) + (8 * 1024 * 1024))))
#define important_page(x) (text_page(NUM(x)) || ptable_page(NUM(x)))

static inline pt_entry_t pre_initialization_pte_address(vaddr_t ptr)
{
  pt_entry_t l3ent = pdp_table[VADDR_TO_L3_INDEX(ptr)];
  pfn_t pd_pfn = machine_to_phys_mapping[l3ent >> PAGE_SHIFT];
  pt_entry_t *ptrs_page_dir = PTENTRYP(NUM(text_start) + (pd_pfn * 4096));
  pt_entry_t pdent = ptrs_page_dir[VADDR_TO_PD_INDEX(ptr)];
  int pt_ent = VADDR_TO_PT_INDEX(ptr);

  return RM_PT_BITS(pdent) + (8 * pt_ent);
}

static void initialize_mm_state(void)
{
  mfn_t ptable_space_mfns[4] = { 0, 0, 0, 0 };
  pt_entry_t unmaps[4] = { 0, 0, 0, 0 };
  int i, j, saved_mfns = 0;
  mmuext_op_t pin_request;
  maddr_t pdp_table_phys;
  mmu_update_t update;
  pfn_t pfn;

  // Figure out where the page-directory-pointer table is. (That's L3
  // table for people who haven't beaten themselves over the head with
  // Intel manuals.).
  pdp_table = (pt_entry_t*)start_info->pt_base;
  pfn = ((vaddr_num_t)pdp_table - (vaddr_num_t)text_start) >> PAGE_SHIFT;
  pdp_table_phys = ((maddr_t)phys_to_machine_mapping[pfn]) << PAGE_SHIFT;

  // Go through the PDP entries. If there's an entry there, remap it into
  // our table. If there isn't, put one there. Oh, and by the way, save off
  // another MFN for use by our mapped page tables. There has to be at 
  // least one mapped, of course, because this code is taking up space.
  for(i = 0; i < 4; i++) {
    unsigned long pdvaddr = (unsigned long)&(page_dir[i * 512]);
    pt_entry_t l3ent = pdp_table[VADDR_TO_L3_INDEX(pdvaddr)];
    pfn_t pd_pfn = machine_to_phys_mapping[l3ent >> PAGE_SHIFT];
    pt_entry_t *pdv_pd = PTENTRYP(NUM(text_start) + (pd_pfn * 4096));
    pt_entry_t pdent = pdv_pd[VADDR_TO_PD_INDEX(pdvaddr)];
    int pt_ent = VADDR_TO_PT_INDEX(pdvaddr);
    pfn_t pt_pfn = machine_to_phys_mapping[pdent >> PAGE_SHIFT];
    pt_entry_t *page_table = PTENTRYP(NUM(text_start) + (pt_pfn * 4096));
    mmuext_op_t flush_req;

    if(ENTRY_PRESENT(pdp_table[i])) {
      if(saved_mfns < 4) {
        ptable_space_mfns[saved_mfns++] = page_table[pt_ent] >> PAGE_SHIFT;
      }
      // Remap the page into the right place.
      update.ptr = pre_initialization_pte_address((vaddr_t)pdvaddr);
      update.val = RM_PT_BITS(pdp_table[i]) | PT_BASE_PROT;
      assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
      // flush out the TLB entry for that place.
      flush_req.cmd = MMUEXT_INVLPG_LOCAL;
      flush_req.arg1.linear_addr = (vaddr_num_t)pdvaddr;
      assert(!HYPERVISOR_mmuext_op(&flush_req, 1, NULL, DOMID_SELF));
      // Unmap the page from its present position.
      l3ent = pdp_table[i];
      pd_pfn = machine_to_phys_mapping[l3ent >> PAGE_SHIFT];
      pdv_pd = PTENTRYP(NUM(text_start) + (pd_pfn * 4096));
      unmaps[i] = pre_initialization_pte_address(pdv_pd);
    } else {
      bzero((void*)pdvaddr, 4096);
      // Remap the page read-only.
      update.ptr = pre_initialization_pte_address((vaddr_t)pdvaddr);
      update.val = page_table[pt_ent] & (~PTE_RW);
      assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
      // Mark it as a L2 page table
      pin_request.cmd = MMUEXT_PIN_L2_TABLE;
      pin_request.arg1.mfn = page_table[pt_ent] >> PAGE_SHIFT;
      assert(HYPERVISOR_mmuext_op(&pin_request, 1, NULL, DOMID_SELF) >= 0);
      // Set it into place.
      update.ptr = (pt_entry_t)pdp_table_phys + (i * 8);
      update.val = RM_PT_BITS(page_table[pt_ent]) | L3_PROT;
      assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
    }
  }

  // We have to split this unmapping up because pre_initialize_pte_address
  // requires an invariant that these unmappings break in some cases. (It
  // expects the initial page tables be at their original locations.) This
  // only seems to matter when the top-level page tables are saturated on
  // boot.
  for(i = 0; i < 4; i++)
    if(unmaps[i]) {
      update.ptr = unmaps[i];
      update.val = 0;
      assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
    }

  // Now we need to buy ourselves some MFNs. We need at least four to
  // hold the page tables describing where we hold page tables (let that
  // settle in your head for a bit).
  if(saved_mfns < 4) {
    pfn_t pfn = ((vaddr_num_t)clearing_space - (vaddr_num_t)text_start) 
                    >> PAGE_SHIFT;
    mfn_t mfn = phys_to_machine_mapping[pfn];
    ptable_space_mfns[saved_mfns++] = mfn;

    // If the world is sane, we have at least three now. One that we just
    // generated, and two from the page directory stuff above. We know two
    // because the Xen virtual space and the space for this are sufficiently
    // different that they should have used two entries.
    assert(saved_mfns >= 3);
    if(saved_mfns < 4) {
      // Uh-oh. Still need one more. This is a bit tricky, and I'm not
      // 100% it's safe ...
      unsigned long free = start_info->pt_base+(4096*start_info->nr_pt_frames);
      pfn = (free - (vaddr_num_t)text_start) >> PAGE_SHIFT;
      assert(pfn < current_memory_reservation);
      ptable_space_mfns[saved_mfns++] = phys_to_machine_mapping[pfn];
      // Unmap the page
      update.ptr = pre_initialization_pte_address((void*)free);
      update.val = 0;
      assert(HYPERVISOR_mmu_update(&update, 1, NULL, DOMID_SELF) >= 0);
    }
  }
  assert(saved_mfns >= 4);

  // Yay. At this point:
  //   (1) page_dir points to all four page tables, in order.
  //   (2) saved_mfns holds four mfns which we can use
  // So now we put in the page tables for where we're going to put page
  // tables in virtual address space. (Let that sink in a second; trust
  // me, it makes some sense.)
  for(i = VADDR_TO_L3PD_INDEX(HYPERVISOR_VIRT_START); i > 2; i--)
    if(!(ENTRY_PRESENT(page_dir[i]) || ENTRY_PRESENT(page_dir[i-1])
          || ENTRY_PRESENT(page_dir[i-2]) || ENTRY_PRESENT(page_dir[i-3])))
    {
      i -= 3; // Just because otherwise I'm going to get things backwards.
      page_tables_start = (void*)(i << 21);
      for(j = 0; j < 4; j++) {
        clear_mfn(ptable_space_mfns[j]);
        pin_table(ptable_space_mfns[j], MMUEXT_PIN_L1_TABLE);
        inject_new_pdir_entry(i+j, ptable_space_mfns[j]);
      }
      // Now we can map them
      for(j = 0; j < 4; j++)
        map_new_ptable((vaddr_t)(NUM(page_tables_start) + ((i+j)*4096)),
                       ptable_space_mfns[j]);
      break;
    }

  // Remap all the page tables into this space.
  for(j = 0; j < (int)VADDR_TO_L3PD_INDEX(HYPERVISOR_VIRT_START); j++)
    if(ENTRY_PRESENT(page_dir[j]) && ((j < i) || (j > (i + 3)))) {
      pt_entry_t mfn = page_dir[j] >> PAGE_SHIFT;
      pfn_t pfn = machine_to_phys_mapping[mfn];
      assert(!(page_dir[j] & PDE_BIGPAGE));
      map_new_ptable((vaddr_t)(NUM(page_tables_start) + (j * 4096)), mfn);
      unmap_page((vaddr_t)(NUM(text_start) + (pfn << PAGE_SHIFT)));
    }

  // For each page mapped, either (a) mark it as used if it's important, or
  // (b) unmap it if it's not.
  for(i = 0; i < (int)VADDR_TO_L3PD_INDEX(HYPERVISOR_VIRT_START); i++)
    if(ENTRY_PRESENT(page_dir[i])) {
      pt_entry_t *ptable = PTENTRYP(NUM(page_tables_start) + (i * 4096));

      for(j = 0; j < 512; j++)
        if(ENTRY_PRESENT(ptable[j])) {
          void *page = (void*)((i << 21) + (j << 12));

          if( important_page(page) ) {
            pfn_t pfn = machine_to_phys_mapping[ptable[j]>>PAGE_SHIFT];
            set_pframe_used(pfn);
          } else {
            unmap_page(page);
          }
        }
    }
}

