#include "spinlocks.h"

void spinlock(uint32_t *addr)
{
  while(!__sync_bool_compare_and_swap(addr, 0, 1));
}

void spinunlock(uint32_t volatile *addr)
{
  *addr = 0;
}
