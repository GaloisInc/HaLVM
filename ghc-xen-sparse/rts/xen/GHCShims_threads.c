// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND

#include <Rts.h>
#include "AwaitEvent.h"
#include "Schedule.h"
#include "errno.h"
#include "time.h"

extern lnat getourtimeofday(void);
extern void startSignalHandlers(Capability *cap);
extern int signals_pending(void);

int forkOS_createThread( HsStablePtr entry __attribute__((unused)) )
{
  return ENOSYS;
}

static rtsBool wakeUpSleepingThreads(lnat ticks)
{
  StgTSO *tso;
  rtsBool flag = rtsFalse;

  while(sleeping_queue != END_TSO_QUEUE && 
        (int)(ticks - sleeping_queue->block_info.target) > 0)
  {
    tso = sleeping_queue;
    sleeping_queue = tso->_link;
    tso->why_blocked = NotBlocked;
    tso->_link = END_TSO_QUEUE;
    pushOnRunQueue(&MainCapability, tso);
    flag = rtsTrue;
  }

  return flag;
}

void awaitEvent(rtsBool wait)
{
  lnat min, ticks;

  do {
    ticks = getourtimeofday();
    /* anyone sleeping that should wake up now? */
    if(wakeUpSleepingThreads(ticks)) {
      return;
    }
    /* block for a bit */
    if(wait) {
      if(sleeping_queue != END_TSO_QUEUE) {
        min  = sleeping_queue->block_info.target - ticks; /* in ticks */
        min *= RtsFlags.MiscFlags.tickInterval; /* in milliseconds */
      } else {
        min = 0x7ffffff;
      }
      block_domain(min);
    }
    /* if there are any signals pending, schedule them */
    if(signals_pending()) {
      startSignalHandlers(&MainCapability);
      return; /* still hold the lock */
    }
    /* we were interrupted, return to the scheduler immediately. */
    if(sched_state >= SCHED_INTERRUPTING) {
      return; /* still hold the lock */
    }
    /* check for threads that need waking up */
    wakeUpSleepingThreads(getourtimeofday());
  } while( wait && (sched_state == SCHED_RUNNING) 
                && emptyRunQueue(&MainCapability) );
}

#ifdef THREADED_RTS
static inline uint32_t atomic_cas_u32(uint32_t *ptr, uint32_t old, uint32_t new)
{
    unsigned long res;

    __asm__(
       "lock cmpxchgl %%ecx, (%%edx)"
     : "=a" (res) /* OUT: eax -> res */
     : "a" (old) /* IN: eax = old */, 
       "c" (new) /* IN: ecx = new */,
       "d" (ptr) /* IN: edx = ptr */
     : "memory"
    );

    return res;
}

int halvm_acquire_lock(Mutex *m)
{
  while(atomic_cas_u32(m, 0, 1)) { }
}

int halvm_release_lock(Mutex *m)
{
  *m = 0;
}

void initMutex(Mutex *m)
{
  *m = 0;
}

void closeMutex(Mutex *m)
{
  *m = 0;
}

#endif
