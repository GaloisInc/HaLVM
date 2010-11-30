// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <stdlib.h>
#include <stdio.h>
#include <mman.h>
#include <mm.h>
#include <alloca.h>
#include <hbmxen.h>
#define QUEUE_MACRO_DEBUG	1
#include <queue.h>
#include <xen/memory.h>
#include <strings.h>
#include <xen/mm/core_constants.h>
#include <xen/mm/core_mb.c>

static void     *text_start                 = NULL;
static void     *text_end                   = NULL;
static uint64_t  maximum_memory_reservation = 0; // in pages
       uint64_t  current_memory_reservation = 0; // in pages
static mfn_t    *phys_to_machine_mapping    = NULL;
static pfn_t     last_found_pfn             = 0;

// ---------------------------------------------------------------------------
//
// PAGE TABLE ROUTINES
//
// The meat of these is in the mm/core_* files.
//
// ---------------------------------------------------------------------------

//
// The below are required to define:
//
//   typedef ... pt_entry_t;
//
//   int address_mapped(vaddr_t);
//   maddr_t virtual_to_machine(vaddr_t);
//   vaddr_t *machine_to_virtual(maddr_t);
//   static pt_entry_t pte_phys_address(vaddr_t);
//   static pt_entry_t *pte_virt_address(vaddr_t);
//   static void force_page_table_for_vaddr(vaddr_t);
//   static void initialize_mm_state();
//
// In doing so, they can assume that text_start and text_end are 
// properly defined, that the virtual address engine is initialized,
// and that the physical frame handling routines are functional.
//
#if defined(__x86_64__)
# define MFN_IN_USE_BIT     0x8000000000000000
#else
# define MFN_IN_USE_BIT     0x80000000
#endif

#define MFN_IN_USE(x)       (((mfn_t)x) & MFN_IN_USE_BIT)
#define SET_MFN_IN_USE(x)   ((x) | MFN_IN_USE_BIT) 
#define CLEAR_MFN_IN_USE(x) ((x) & (~MFN_IN_USE_BIT))

static inline void set_pframe_used(pfn_t pfn)
{
  assert(!MFN_IN_USE(phys_to_machine_mapping[pfn]));
  phys_to_machine_mapping[pfn] = SET_MFN_IN_USE(phys_to_machine_mapping[pfn]);
}

static inline void return_pframe(pfn_t pfn)
{
  assert(MFN_IN_USE(phys_to_machine_mapping[pfn]));
  phys_to_machine_mapping[pfn] = CLEAR_MFN_IN_USE(phys_to_machine_mapping[pfn]);
}

#if defined(__x86_64__)
#include <xen/mm/core_64.c>
#elif defined(CONFIG_X86_PAE)
#include <xen/mm/core_32p.c>
#else
#include <xen/mm/core_32.c>
#endif

int set_page_writable(vaddr_t addr, int write, domid_t dom)
{
  mmu_update_t request;
  pt_entry_t val;

  assert(PAGE_ALIGNED((vaddr_num_t)addr));
  val = *pte_virt_address(addr);
  if(write) val = val | PTE_RW; else val = val & (~PTE_RW);
  request.ptr = pte_phys_address(addr);
  request.val = val;
  return HYPERVISOR_mmu_update(&request, 1, NULL, dom);
}

int get_page_protection(vaddr_t ptr)
{
  pt_entry_t val;
  int res = 0;
  
  assert(PAGE_ALIGNED((vaddr_num_t)ptr));
  val = *pte_virt_address(ptr);

  if(ENTRY_PRESENT(val))
    res |= PROT_READ;
  if(ENTRY_READWRITE(val))
    res |= PROT_WRITE;
#if defined(L4E_NX)
  if(!(val & L4E_NX))
    res |= PROT_EXEC;
#else
  if((ptr >= text_start) && (ptr < text_end))
    res |= PROT_EXEC;
#endif

  return res;
}

int mark_as_page_table(int level, vaddr_t addr, domid_t dom)
{
  mmuext_op_t pin_request;

  switch(level) {
    case 1: pin_request.cmd = MMUEXT_PIN_L1_TABLE; break;
    case 2: pin_request.cmd = MMUEXT_PIN_L2_TABLE; break;
    case 3: pin_request.cmd = MMUEXT_PIN_L3_TABLE; break;
    case 4: pin_request.cmd = MMUEXT_PIN_L4_TABLE; break;
    default:
      pabort("MM: Internal error: Invalid argument to mark_as_page_table!\n");
  }

  pin_request.arg1.mfn = virtual_to_machine(addr) >> PAGE_SHIFT;
  return HYPERVISOR_mmuext_op(&pin_request, 1, NULL, dom);
}

int mark_as_page_table_mfn(int level, mfn_t mfn, domid_t dom)
{
  mmuext_op_t pin_request;

  switch(level) {
    case 1: pin_request.cmd = MMUEXT_PIN_L1_TABLE; break;
    case 2: pin_request.cmd = MMUEXT_PIN_L2_TABLE; break;
    case 3: pin_request.cmd = MMUEXT_PIN_L3_TABLE; break;
    case 4: pin_request.cmd = MMUEXT_PIN_L4_TABLE; break;
    default:
      pabort("MM: Internal error: Invalid argument to mark_as_page_table!\n");
  }

  pin_request.arg1.mfn = mfn;
  return HYPERVISOR_mmuext_op(&pin_request, 1, NULL, dom);
}

static void back_pages_with_mfns(domid_t dom, mfn_t *frames,
                                 size_t num_frames, vaddr_t dest, int prot)
{
  unsigned long mmu_prot = PT_BASE_PROT;
  mmu_update_t updates[16];
  size_t i, j;
  int res;

  assert(PAGE_ALIGNED((vaddr_num_t)dest));
  if(prot & PROT_READ)    mmu_prot |= PTE_PRESENT;
  if(prot & PROT_WRITE)   mmu_prot |= PTE_RW;
  if(prot & PROT_NOCACHE) mmu_prot |= PTE_CACHEDISABLE;

  for(i = 0; i < num_frames; i += 16) {
    for(j = 0; (j < 16) && ((i + j) < num_frames); j++) {
      vaddr_t current = (vaddr_t)(NUM(dest) + ((i + j) * PAGE_SIZE));
      force_page_table_for_vaddr(current);
      updates[j].ptr = pte_phys_address(current);
      updates[j].val = (((maddr_t)frames[i+j]) << PAGE_SHIFT) | mmu_prot;
    }
    res = HYPERVISOR_mmu_update(updates, j, NULL, dom);
    if(res < 0) pabort("MM: Back pages with mfns failed: %d\n", res);
  }
}

void unback_pages(vaddr_t start, vaddr_t end, int do_return_pframes)
{
  mmu_update_t requests[16];
  mmuext_op_t flush_req;

  assert(PAGE_ALIGNED((vaddr_num_t)start));
  assert(PAGE_ALIGNED((vaddr_num_t)end));
  assert(start < end);

  while(start < end) {
    unsigned long num_this_round = 0;

    while((start < end) && (num_this_round < 16)) {
      flush_req.cmd = MMUEXT_INVLPG_LOCAL;
      flush_req.arg1.linear_addr = (vaddr_num_t)start;
      assert(!HYPERVISOR_mmuext_op(&flush_req, 1, NULL, DOMID_SELF));
      requests[num_this_round].ptr = pte_phys_address(start);
      requests[num_this_round].val = 0;
      if(do_return_pframes) {
        maddr_t maddr = virtual_to_machine(start);
        mfn_t mfn = maddr >> PAGE_SHIFT;
        assert(maddr);
        return_pframe(machine_to_phys_mapping[mfn]);
      }
      num_this_round++; start = (vaddr_t)(NUM(start) + PAGE_SIZE);
    }
    assert(HYPERVISOR_mmu_update(requests,num_this_round,NULL,DOMID_SELF) >= 0);
  }
}

// ---------------------------------------------------------------------------
// 
// VIRTUAL ADDRESS SPACE HANDLING
//
// This code keeps track of what parts of virtual adress space have been 
// allocated, so we can try to do a little better than constantly rescanning
// the page directory and tables.
//
// This used to be something fancier, that kept a nice ptr set structure that
// made things faster. Unfortunately, it called malloc(). And since malloc()
// calls this code ... badness happens.
//
// ---------------------------------------------------------------------------

static void *start_searching_hint = (void*)0x1000;

static void claim_addresses(void *start, void *end)
{
  // XXX: If the addresses are not page aligned,
  // then this is going to round down!
  vaddr_num_t num_updates = ((vaddr_num_t)end - (vaddr_num_t)start) / PAGE_SIZE;
  void *ptr;

  // Force the page table to exist, because we're probably about to use
  // this space.
  for(ptr = start; ptr < end; ptr = (void*)(NUM(ptr) + PAGE_SIZE))
    force_page_table_for_vaddr(ptr);

  // Now do the updates.
  ptr = start; 
  while(num_updates > 0) {
    int updates_this_time = (num_updates > 16) ? 16 : num_updates;
    mmu_update_t *updates = alloca(updates_this_time * sizeof(mmu_update_t));
    int res, i;

    for(i = 0; i < updates_this_time; i++) {
      pt_entry_t curval = *pte_virt_address(ptr);
      updates[i].ptr = pte_phys_address(ptr);
      updates[i].val = curval | PTE_CLAIMED;
      ptr = (void*)(NUM(ptr) + PAGE_SIZE);
      num_updates -= 1;
    }

    res = HYPERVISOR_mmu_update(updates, updates_this_time, NULL, DOMID_SELF);
    assert(res >= 0);
  }
}

static void disclaim_addresses(void *start, void *end)
{
  vaddr_num_t num_updates = ((vaddr_num_t)end - (vaddr_num_t)start) / PAGE_SIZE;
  void *ptr;

  // Now do the updates.
  ptr = start; 
  while(num_updates > 0) {
    int updates_this_time = (num_updates > 16) ? 16 : num_updates; 
    mmu_update_t *updates = alloca(updates_this_time * sizeof(mmu_update_t));
    int res, i;

    for(i = 0; i < updates_this_time; i++) {
      pt_entry_t curval = *pte_virt_address(ptr);
      updates[i].ptr = pte_phys_address(ptr);
      updates[i].val = curval & (~PTE_CLAIMED);
      ptr = (void*)(NUM(ptr) + PAGE_SIZE);
      num_updates -= 1;
    }

    res = HYPERVISOR_mmu_update(updates, updates_this_time, NULL, DOMID_SELF);
    assert(res >= 0);
  }
}

static void initialize_vaddr_engine(void)
{
  claim_addresses(NULL, (void*)0x1000);
}


// Reserve some virtual space (not backed up by physical memory).
// Arguments:
//   "ptr" specified the virtual location that we want to claim.
//         If "ptr" is NULL, then we get to pick the location.
//   "user_size" is the size in bytes of the memory.
//         We always reserve an amount that is a whole page.
//         If the size is not a multiple of PAGE_SIZE, then
//         we round up to the next multiple.
// On success, we return the first address of the reserved space.
// On failure, we return NULL.
vaddr_t claim_vspace(vaddr_t ptr, size_t user_size)
{
  size_t size_pages = (user_size + PAGE_SIZE - 1) >> PAGE_SHIFT;
  size_t i;

  if (size_pages == 0) return NULL;

  if(ptr) {
    assert(((vaddr_num_t)ptr & ~PAGE_MASK) == 0);
    for(i = 0; i < size_pages; i++)
      if(address_claimed((void*)(NUM(ptr) + (i * PAGE_SIZE))))
        return NULL;
    claim_addresses(ptr, (void*)(NUM(ptr) + (size_pages * PAGE_SIZE)));
    return ptr; 
  } else {
    void *search_pos = start_searching_hint;
    void *free_start = NULL;
    size_t found_free_pages = 0;

    // We start searching at "start_searching_hint" and continue
    // searching until we either find enough free space, or else
    // we get back to "start_searching_hint".  We wrap around when
    // we reach the hypervisor.
    do {
      // If we've now found enough pages, yay! Reserve free_start up to
      // search_pos and return free_start.
      if(found_free_pages == size_pages) {
        claim_addresses(free_start, search_pos);
        start_searching_hint = search_pos;
        return free_start;
      }

      // Alright, still need more pages.
      if(address_claimed(search_pos)) {
        // It is. So reset found_free_pages;
        found_free_pages = 0;
        free_start = NULL;
      } else {
        // It isn't. So increment found_free_pages, and if this is our 
        // first free page, mark that down in free_start.
        if(found_free_pages == 0) {
          free_start = search_pos;
        }
        found_free_pages++;
      }
      // Advance and continue.
      search_pos = (void*)(NUM(search_pos) + PAGE_SIZE);

      // Wrap around?
      if(search_pos >= (void*)HYPERVISOR_VIRT_START) {
        found_free_pages = 0;
        search_pos       = (void*)PAGE_SIZE;
        free_start       = NULL;
      }
    } while(search_pos != start_searching_hint);

    printf("MM: Ran out of address space?!\n");
  }

  return NULL;
}

void disclaim_vspace(vaddr_t start, vaddr_t end)
{
  assert(start);
  assert(end);
  assert(end > start);
  assert( ((vaddr_num_t)start & 0x0fff) == 0);
  assert( ((vaddr_num_t)end & 0x0fff) == 0);
  disclaim_addresses(start, end);
}

// ---------------------------------------------------------------------------
//
// PHYSICAL FRAME MANAGEMENT
//
// These routines manage physical frames, including giving extra ones back to
// Xen, getting new ones from Xen, and translations to and from physical and
// machine addresses.
//
// ---------------------------------------------------------------------------

static void increase_memory_balloon(void)
{
  xen_memory_reservation_t reservation;
  pfn_t new_pfn_count = 512, res, i;

  if(current_memory_reservation == maximum_memory_reservation)
    pabort("MM: Out of memory!");

  if(current_memory_reservation + new_pfn_count > maximum_memory_reservation) 
    new_pfn_count = maximum_memory_reservation - current_memory_reservation;

  for(i = 0; i < new_pfn_count; i++)
    phys_to_machine_mapping[i + current_memory_reservation] = 
      i + current_memory_reservation;

  reservation.extent_start = 
    (unsigned long*)&(phys_to_machine_mapping[current_memory_reservation]);
  reservation.nr_extents = new_pfn_count;
  reservation.extent_order = 0;
  reservation.address_bits = 0;
  reservation.domid = DOMID_SELF;
  res = HYPERVISOR_memory_op(XENMEM_populate_physmap, &reservation);
  if(res <= 0) pabort("MM: Couldn't increase reservation: %d\n", res);
  current_memory_reservation += res;
  printf("MM: Increased memory reservation to %lld of %lld MB.\n",
         current_memory_reservation / 256, maximum_memory_reservation / 256);
}

static void reset_memory_balloon(void)
{
  domid_t domid = DOMID_SELF;
  pfn_t kill_after = 0, i;

  // Find the last used PFN
  for(i = 0; i < current_memory_reservation; i++)
    if(MFN_IN_USE(phys_to_machine_mapping[i]))
      kill_after = i;
  assert(kill_after > 0);
  // Keep a one meg buffer at the top.
  kill_after += 256;
  // Time to start dumping pages.
  while(kill_after < current_memory_reservation) {
    xen_memory_reservation_t reservation;
    unsigned long extents[1024];
    unsigned long num_extents, i;

    num_extents = current_memory_reservation - kill_after;
    if(num_extents > 1024) num_extents = 1024;

    for(i = 0; i < num_extents; i++, kill_after++) {
      assert(!MFN_IN_USE(phys_to_machine_mapping[kill_after]));
      extents[i] = phys_to_machine_mapping[kill_after];
    }
    reservation.extent_start = extents;
    reservation.nr_extents = num_extents;
    reservation.extent_order = 0;
    reservation.address_bits = 0;
    reservation.domid = DOMID_SELF;
    assert(HYPERVISOR_memory_op(XENMEM_decrease_reservation, &reservation)>=0);
  }
  current_memory_reservation = 
    HYPERVISOR_memory_op(XENMEM_current_reservation, &domid);
  if(last_found_pfn >= current_memory_reservation) last_found_pfn = 0;
}

mfn_t pfn_to_mfn(pfn_t pframe)
{
  unsigned long res = phys_to_machine_mapping[pframe];
  return CLEAR_MFN_IN_USE(res);
}

void replace_mfn(mfn_t mfn)
{
  pfn_t pfn = machine_to_phys_mapping[mfn];
  mfn_t new_mfn = 0;
  xen_memory_reservation_t reservation;
  int res = 0;

  assert(pfn); assert(!MFN_IN_USE(pfn));
  reservation.extent_start = (unsigned long*)&new_mfn;
  reservation.nr_extents = 1;
  reservation.extent_order = 0;
  reservation.address_bits = 0;
  reservation.domid = DOMID_SELF;
  res = HYPERVISOR_memory_op(XENMEM_increase_reservation, &reservation);
  assert(res > 0);
  phys_to_machine_mapping[pfn] = new_mfn;
}

mfn_t get_free_machine_frame(void)
{
  pfn_t i = last_found_pfn++, start = i, retries = 0;

 retry:
  assert(retries < 2);
  // This last_found / start / i nonsense is a bit of an optimization; 
  // instead of always searching the array from 0, we search from the 
  // last point we found something.
  do {
    if(!MFN_IN_USE(phys_to_machine_mapping[i])) {
      unsigned long retval = phys_to_machine_mapping[i];
      last_found_pfn = i;
      set_pframe_used(i);
      return retval;
    }
    i++; if(i == current_memory_reservation) i = 0;
  } while(i != start);

  // Ack! Searched the entire address space and couldn't find anything.
  // Embiggen it and try again.
  increase_memory_balloon();
  retries++;
  goto retry;
}

// ---------------------------------------------------------------------------
//
// INTERFACE MAPPING
//
// Several "users" of this module require slightly different interface to the
// standard functions set. This section defines the translations.
//
// ---------------------------------------------------------------------------

vaddr_t alloc_page(void)
{
  return mmap(NULL, PAGE_SIZE, PROT_READ | PROT_WRITE | PROT_EXEC, 0, -1, 0);
}

void free_page(vaddr_t ptr)
{
  munmap(ptr, PAGE_SIZE);
}

vaddr_t map_frames(domid_t dom, mfn_t *frames, size_t num_frames)
{
  vaddr_t dest = claim_vspace(NULL, num_frames * PAGE_SIZE);
  back_pages_with_mfns(dom, frames, num_frames, dest, PROT_READ|PROT_WRITE);
  return dest;
}

vaddr_t map_readonly_frames(domid_t dom, mfn_t *frames, size_t num)
{
  vaddr_t dest = claim_vspace(NULL, num * PAGE_SIZE);
  back_pages_with_mfns(dom, frames, num, dest, PROT_READ);
  return dest;
}

int back_pages(vaddr_t start, vaddr_t end, int prot)
{
  unsigned long num_frames, i;
  mfn_t frames[16];

  assert(PAGE_ALIGNED((vaddr_num_t)start));
  assert(PAGE_ALIGNED((vaddr_num_t)end));
  num_frames = (NUM(end) - NUM(start)) / PAGE_SIZE;
  assert(num_frames > 0);
  while(num_frames > 0) {
    unsigned long frames_this_round = num_frames > 16 ? 16 : num_frames;

    for(i = 0; i < frames_this_round; i++)
      frames[i] = get_free_machine_frame();
    back_pages_with_mfns(DOMID_SELF, frames, frames_this_round, start, prot);
    start = (vaddr_t)(NUM(start) + (frames_this_round * PAGE_SIZE));
    num_frames -= frames_this_round;
  }
  return 1;
}

// ---------------------------------------------------------------------------
//
// INITIALIZATION
//
// Sets up the system.
//
// ---------------------------------------------------------------------------

extern char _text, _etext;

void init_mm(void)
{
  unsigned long domid = DOMID_SELF;
  vaddr_t cur = NULL;

  assert(sizeof(unsigned long) == sizeof(void*));
  // Basic constants necessary for basically everything else.
  phys_to_machine_mapping = (mfn_t*)start_info->mfn_list;
  text_start = &(_text);
  text_end = (void*)(start_info->pt_base + (PAGE_SIZE * start_info->nr_pt_frames));

  maximum_memory_reservation =
    HYPERVISOR_memory_op(XENMEM_maximum_reservation, &domid);
  current_memory_reservation =
    HYPERVISOR_memory_op(XENMEM_current_reservation, &domid);
  // Force the maximum to have a positive value. This is just handy, although
  // probably not strictly correct. I'm not exactly sure how to solve this
  // problem in stock Xen, though, because querying the amount of memory
  // that's *really* available doesn't seem possible.
  if( ((long)maximum_memory_reservation) == -1 )
    maximum_memory_reservation = 1048576;

  // Alright set up all the internal state.
  initialize_mm_state();
  initialize_vaddr_engine();

  for(cur = (&_text);
      cur < ((vaddr_t)&_etext);
      cur = (vaddr_t)(NUM(cur) + PAGE_SIZE)) 
  {
    set_page_writable(cur, 0, DOMID_SELF);
  }

  // Give back some memory
  reset_memory_balloon();

}
