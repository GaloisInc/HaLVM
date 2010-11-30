// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <Rts.h>
#include "GetTime.h"
#include "sm/OSMem.h"
#include "arch.h"
#include "mm.h"
#include "mman.h"
#include "stdio.h"

nat getPageFaults(void)
{
  return 0;
}

lnat getPageSize(void)
{
  return PAGE_SIZE;
}

void setExecutable( void    *p    __attribute__((unused)),
                    lnat     len  __attribute__((unused)),
                    rtsBool  exec __attribute((unused))    )
{
  /* Not feasible on the x86 in 32-bit mode without messing with ugly
   * segmentation register stuff. */
  return;
}

/******************************************************************************/

static void *next_request = (void*)MBLOCK_SIZE;

void osMemInit(void)
{
  /* Nothing to do */ 
}

void *osGetMBlocks(nat n)
{
  void *start_attempt = next_request;
  uint32_t size = n * MBLOCK_SIZE;

  assert( ((uint32_t)start_attempt % MBLOCK_SIZE) == 0);
  do {
    if( claim_vspace(next_request, size) ) {
      void *retval = next_request;
      back_pages(retval, retval + size, PROT_READ | PROT_WRITE );
      next_request += size;
      return retval;
    }
    next_request += MBLOCK_SIZE;
  } while(next_request != start_attempt);

  /* EEEK! We couldn't find memory to allocate! */
  printf("osGetMBlocks failing!\n");
  return NULL;
}

void osFreeAllMBlocks(void)
{
  /* Nothing really to do, this container is about to be destroyed, and
   * the shared library concern mentioned in the posix tree and bug 
   * report doesn't really matter */
}

