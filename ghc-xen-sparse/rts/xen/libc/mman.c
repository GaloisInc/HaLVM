// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <stdlib.h>
#include <mman.h>
#include <types.h>
#include <mm.h>
#include <strings.h>
#include <stdio.h>

void *mmap(void *start, size_t length, int prot, 
	       int flags __attribute__((unused)), 
	       int fd, off_t offset __attribute__((unused)))
{
  // Make sure the runtime isn't trying to map a file.
  if(fd != -1) return NULL;
  length = round2page(length);
  // If the caller has specified an address to use, see if the range is 
  // unmapped
  if(start) {
    void *cur = start, *end = start + length;
    int is_unmapped = 1;

    // In fact, see if the whole things is unmapped ...
    while(is_unmapped && (cur < end)) {
      is_unmapped = !address_mapped(cur);
      cur += PAGE_SIZE;
    }
    if(is_unmapped) {
      // Amazing. We can grant their request.
      if(claim_vspace(start, length)) {
        back_pages(start, end, prot);
        bzero(start, length);
	    return start;
      }
    }
    // The address range is not unmapped, so just make something up
    // as per usual.
  }
  // Find somewhere to put this.
  start = claim_vspace(NULL, length);
  if(start) {
    if(back_pages(start, start + length, prot)) {
      bzero(start, length);
      return start;
    }
    disclaim_vspace(start, start + length);
    return NULL;
  } else return NULL;
}

int munmap(void *start, size_t length)
{
  void *end = (void*)((unsigned long)start + round2page(length));
  assert(start);
  assert(start < end);
  unback_pages(start, end, 1);
  disclaim_vspace(start, end);
  return 0;
}

void *mremap(void *old_address, size_t old_size, size_t new_size, 
	     int flags __attribute__((unused)))
{
  old_size = round2page(old_size);
  new_size = round2page(new_size);
  if(old_size == new_size)
    return old_address;
  if(old_size < new_size) {
    // See if we can claim the relevant space
    if(claim_vspace(old_address + old_size, new_size - old_size)) {
      back_pages(old_address + old_size, old_address + new_size, 
		 get_page_protection(old_address));
      return old_address;
    } 
    // It's being used
    return NULL;
  } else {
    unback_pages(old_address + new_size, old_address + old_size, 1);
    disclaim_vspace(old_address + new_size, 
		    old_address + (old_size - new_size));
    return old_address;
  }
}
