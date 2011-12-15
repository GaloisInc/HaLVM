#ifndef COMMUNICATION_SPINLOCKS_H
#define COMMUNICATION_SPINLOCKS_H

#include <sys/types.h>

void spinlock(uint32_t *addr);
void spinunlock(uint32_t volatile *addr);

#endif
